/*
* Created by Harm for TeknoParrot
* This file is part of the OpenParrot project - https://teknoparrot.com / https://github.com/teknogods
*
* See LICENSE and MENTIONS in the root of the source tree for information
* regarding licensing.
*/

#include <windows.h>
#include <mmsystem.h>
#include "OpenSndVoyager.h"
#include <vector>
#include <dsound.h>
#pragma comment(lib, "dsound.lib")
#pragma comment(lib, "dxguid.lib")
#include <shlwapi.h>
#pragma comment(lib, "shlwapi.lib")
#include <stdio.h>
#include <time.h>

// Forward declarations
void InitLogging();
void CloseLogging();
void info(const char* format, ...);
void StopAllChannels();

// Global Synchronization
CRITICAL_SECTION g_csStream;

// DLL entry point
BOOL APIENTRY DllMain(HMODULE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		InitLogging();
		InitializeCriticalSection(&g_csStream);
		info("DllMain: DLL_PROCESS_ATTACH");
		break;
	case DLL_PROCESS_DETACH:
		info("DllMain: DLL_PROCESS_DETACH");
		DeleteCriticalSection(&g_csStream);
		CloseLogging();
		break;
	}
	return TRUE;
}

// === Enhanced Logging System ===
static FILE* g_logFile = NULL;
static BOOL g_enableFileLogging = TRUE;
static BOOL g_enableDetailedLogging = TRUE;
static DWORD g_frameCounter = 0;

// Thread synchronization
// CRITICAL_SECTION g_csStream; // Moved to top

void InitLogging()
{
	if (g_enableFileLogging && g_logFile == NULL)
	{
		char logPath[MAX_PATH];
		char buf[MAX_PATH];
		memset(buf, 0, sizeof(buf));
		GetCurrentDirectoryA(256, buf);
		auto len = strlen(buf);
		buf[len] = '\\';
		strcat_s(buf, "stv\\logs\\");
		CreateDirectoryA(buf, NULL);
		
		time_t now = time(NULL);
		struct tm timeinfo;
		localtime_s(&timeinfo, &now);
		sprintf_s(logPath, "%sOpenSndVoyager_%04d%02d%02d_%02d%02d%02d.log", buf,
			timeinfo.tm_year + 1900, timeinfo.tm_mon + 1, timeinfo.tm_mday,
			timeinfo.tm_hour, timeinfo.tm_min, timeinfo.tm_sec);
		
		fopen_s(&g_logFile, logPath, "w");
		if (g_logFile)
		{
			fprintf(g_logFile, "=== OpenSndVoyager Enhanced Debug Log ===\n");
			fprintf(g_logFile, "Build Time: %s %s\n\n", __DATE__, __TIME__);
			fflush(g_logFile);
		}
	}
}

void CloseLogging()
{
	if (g_logFile)
	{
		fprintf(g_logFile, "\n=== Log Closed ===\n");
		fclose(g_logFile);
		g_logFile = NULL;
	}
}

#ifdef _DEBUG
void info(const char* format, ...)
{
	va_list args;
	char buffer[2048];

	va_start(args, format);
	int len = _vsnprintf_s(buffer, sizeof(buffer) - 2, _TRUNCATE, format, args);
	va_end(args);

	if (len > 0)
	{
		buffer[len] = '\n';
		buffer[len + 1] = '\0';

		OutputDebugStringA(buffer);
		
		if (g_logFile)
		{
			fprintf(g_logFile, "[%06d] %s", g_frameCounter, buffer);
			fflush(g_logFile);
		}
	}
}
#else
#define info(x, ...) {}
#endif

// === Music-specific logging macros ===
#define LOG_MUSIC(fmt, ...) info("[MUSIC] " fmt, __VA_ARGS__)
#define LOG_SOUND(fmt, ...) info("[SOUND] " fmt, __VA_ARGS__)
#define LOG_BUFFER(fmt, ...) info("[BUFFER] " fmt, __VA_ARGS__)
#define LOG_MIX(fmt, ...) //info("[MIX] " fmt, __VA_ARGS__)
#define LOG_OPCODE(fmt, ...) info("[OPCODE] " fmt, __VA_ARGS__)

// Per-channel volume scaling for testing (256 = normal, lower = quieter)
static INT channelVolumeScale[CHANNELS] = {
	256, 256, 256, 256, 256, 256, 256, 256, 256, 256,  // 0-9
	256, 256, 256, 256, 256, 256, 256, 256, 256, 256,  // 10-19
	16, 16, 16, 16, 16, 16, 16, 16, 16, 16,            // 20-29 (music)
	16, 256, 256                                        // 30=music, 31-32=voices
};

// Buffer Oscillator - continuously generates audio samples (like original BO)
struct ChannelOscillator
{
	BOOL active = FALSE;              // Is this channel generating audio?
	DWORD sampleStart = 0;           // Start position in ROM (byte offset)
	DWORD sampleEnd = 0;             // End position in ROM
	DWORD sampleLoop = 0;            // Loop point in ROM
	DWORD samplePos = 0;             // Current position (24.8 fixed point)
	DWORD sampleIncrement = 0;       // Speed (24.8 fixed point, how much to advance per sample)
	WORD volume = 1023;              // Volume (0-1023, like original)
	BYTE lastSpeed = 0;              // Last speed value set (for music detection)
};

// Parsing state - separate from oscillator (like original CCB)
struct ChannelParseState
{
	WORD cursor = 0;                 // Current position in opcode stream
	INT soundCode = 0;               // The original sound code
	BYTE* pOpcodeData = nullptr;     // Pointer to opcode stream
	WORD currentPatch = 0;           // Current sample number set by SETPATCH
	BYTE currentLevel = 0xFF;        // Current attenuation
	BYTE currentSpeed = 0x40;        // Current speed
	BYTE loopControl = 0;            // Loop counter for OC_SET_LUP_CNTL
	BYTE pendingRestDuration = 0;    // REST opcode flag
	INT durationTicks = 0;           // Remaining ticks before next parse (ccb+8)
	INT durationCounter = 0;         // Current tick count (ccb+12)
	INT localTempo = 255;            // Local tempo value (ccb+20, default 255)
	INT tempoFractionAccumulator = 0; // Fractional accumulator (ccb+24)
	BYTE tickCounter = 0;            // Sub-tick counter (ccb+47)
	BOOL isActive = FALSE;           // Is this channel's sequence active?
};

// Channel states
ChannelOscillator channelOscillator[CHANNELS];
ChannelParseState channelState[CHANNELS];

DWORD hexSize = 0x12E08;
BYTE* pHexBuffer = new BYTE[hexSize];

DWORD sampleHeadersSize = 0x6DD0;
BYTE* pSampleHeaders = new BYTE[sampleHeadersSize];

// DirectSound streaming
LPDIRECTSOUND8 pDirectSound = NULL;
LPDIRECTSOUNDBUFFER pPrimaryBuffer = NULL;
LPDIRECTSOUNDBUFFER8 pStreamingBuffer = NULL;
LPDIRECTSOUNDNOTIFY8 pNotify = NULL;
HANDLE notifyEvents[4] = {NULL, NULL, NULL, NULL};
HANDLE streamingThread = NULL;
BOOL shutdownStreaming = FALSE;

BYTE* pDataBuffer = NULL;
DWORD binSize = 0x100000;
DWORD bufferSize = binSize * 4; // Total ROM size (4 files)
BOOL bufferReady = FALSE;

// Streaming parameters
const DWORD SAMPLE_RATE = 44100;
const DWORD BITS_PER_SAMPLE = 16;
const DWORD NUM_CHANNELS_OUTPUT = 2; // Stereo
const DWORD BUFFER_SECTIONS = 4;
const DWORD SECTION_SIZE_MS = 100; // 100ms per section
const DWORD SECTION_SIZE_SAMPLES = (SAMPLE_RATE * SECTION_SIZE_MS) / 1000;
const DWORD SECTION_SIZE_BYTES = SECTION_SIZE_SAMPLES * (BITS_PER_SAMPLE / 8) * NUM_CHANNELS_OUTPUT;
const DWORD STREAMING_BUFFER_SIZE = SECTION_SIZE_BYTES * BUFFER_SECTIONS;

// Timing for sequencer updates (like original)
// Timing constants based on stvsnd.c analysis (stuff_audio_data_block_new called with 184 samples)
// The original game processes logic every 184 audio samples at 44100Hz (~240Hz)
const float SAMPLES_PER_TICK = 184.0f;
const float TICKS_PER_SECOND = SAMPLE_RATE / SAMPLES_PER_TICK;

float restMultiplier = 4.4f;

// === OLD CODE COMPATIBILITY (will be removed) ===
// These variables are for old buffer-based code that's being phased out
HANDLE updateThread = NULL;
BOOL shutdownThread = FALSE;

// Old BUFFER structure stub (for old code that hasn't been removed yet)
struct QueuedSample {
	WORD sample;
	BYTE speed;
	BYTE level;
	WORD stay;
	
	QueuedSample() : sample(0), speed(0), level(0), stay(0) {}
	QueuedSample(WORD s, BYTE sp, BYTE l, WORD st) : sample(s), speed(sp), level(l), stay(st) {}
};

struct BUFFER_OLD {
	LPDIRECTSOUNDBUFFER8 dsBuffer = NULL;
	WAVEFORMATEX dsFormat = {0};
	BOOL playing = FALSE;
	BOOL loop = FALSE;
	INT qIndex = -1;
	BOOL needsParse = FALSE;
	BOOL isActive = FALSE;
	WORD cursor = 0;
	INT soundCode = 0;
	BYTE* pSampleHeader = nullptr;
	BYTE* pAudioData = nullptr;
	BYTE currentLevel = 0xFF;
	BYTE currentSpeed = 0x40;
	WORD currentSample = 0;
	WORD lastPlayedSample = 0;
	BYTE pendingRestDuration = 0;
	DWORD audioDataSize = 0;
	DWORD playPosition = 0;
	std::vector<QueuedSample> qSamples;
};
BUFFER_OLD channelBuffer[CHANNELS];

// Logging macros
#define LOG_LOOP(fmt, ...) info("[LOOP] " fmt, __VA_ARGS__)
#define LOG_QUEUE(fmt, ...) info("[QUEUE] " fmt, __VA_ARGS__)

// Forward declarations
DWORD SpeedToSampleRate(BYTE speed);
void ParseAndUpdateChannel(BYTE channel);
void KillOscillator(BYTE channel);
void SetPatch(BYTE channel, WORD sampleNum);
void SetFrequency(BYTE channel, BYTE speed);

// === Core Streaming/Mixing Functions ===

// Read one sample byte from ROM with looping (like original stuff_bo_data_block)
inline BYTE ReadSampleByte(BYTE channel)
{
	ChannelOscillator& osc = channelOscillator[channel];
	
	if (!osc.active) return 128; // Silence
	
	DWORD pos = osc.samplePos >> 8; // Convert from 24.8 fixed point to byte offset
	
	// Check bounds and loop
	if (pos >= osc.sampleEnd)
	{
		// Relaxed loop check: trust osc.sampleLoop if it is less than osc.sampleEnd
		if (osc.sampleLoop < osc.sampleEnd)
		{
			// Loop back
			DWORD loopOffset = pos - osc.sampleEnd;
			pos = osc.sampleLoop + loopOffset;
			osc.samplePos = pos << 8;
		}
		else
		{
			// No valid loop, stop oscillator
			osc.active = FALSE;
			return 128;
		}
	}
	
	return pDataBuffer[pos];
}

// Mix all 33 channels into output buffer (like original stuff_audio_data_block_new)
void MixChannels(SHORT* outputBuffer, DWORD numSamples)
{
	for (DWORD i = 0; i < numSamples; i++)
	{
		INT mixedLeft = 0;
		INT mixedRight = 0;
		
		// Mix all channels
		for (BYTE ch = 0; ch < CHANNELS; ch++)
		{
			ChannelOscillator& osc = channelOscillator[ch];
			
			if (osc.active && osc.sampleIncrement > 0)
			{
				// Read sample
				BYTE sampleByte = ReadSampleByte(ch);
				
				// Convert unsigned 8-bit (0-255) to signed 16-bit
				INT sampleValue = ((INT)sampleByte - 128) * channelVolumeScale[ch]; // Apply per-channel scale
				sampleValue = (sampleValue * (INT)osc.volume) >> 10; // Apply volume (divide by 1024)
				
				mixedLeft += sampleValue;
				mixedRight += sampleValue;
				
				// Advance sample position
				osc.samplePos += osc.sampleIncrement;
			}
		}
		
		// Clamp and store stereo samples
		if (mixedLeft > 32767) mixedLeft = 32767;
		if (mixedLeft < -32768) mixedLeft = -32768;
		if (mixedRight > 32767) mixedRight = 32767;
		if (mixedRight < -32768) mixedRight = -32768;
		
		outputBuffer[i * 2] = (SHORT)mixedLeft;
		outputBuffer[i * 2 + 1] = (SHORT)mixedRight;
	}
}

// Update sequencer tick counters (like original update_sequencer)
void UpdateSequencer(DWORD numSamples)
{
	static float sampleAccumulator = 0.0f;
	
	sampleAccumulator += numSamples;
	
	while (sampleAccumulator >= SAMPLES_PER_TICK)
	{
		sampleAccumulator -= SAMPLES_PER_TICK;
		
		// Process all channels (mimics original update_sequencer exactly)
		for (BYTE ch = 0; ch < CHANNELS; ch++)
		{
			ChannelParseState& state = channelState[ch];
			
			if (!state.isActive || state.durationTicks <= 0)
				continue;
			
			// Original code: v3 = ccb[47] + 64
			state.tickCounter = (state.tickCounter + 64) & 0xFF;
			
			// Original code: v4 = ccb[20] + ccb[24]
			INT oldFraction = state.tempoFractionAccumulator;
			state.tempoFractionAccumulator = state.localTempo + state.tempoFractionAccumulator;
			
			// Check if high byte changed (overflow)
			if ((oldFraction & 0xFF00) != (state.tempoFractionAccumulator & 0xFF00))
			{
				// Keep only low byte
				state.tempoFractionAccumulator = state.tempoFractionAccumulator & 0xFF;
				
				// Increment duration counter
				state.durationCounter++;
				
				// Check if duration expired
				if (state.durationCounter >= state.durationTicks)
				{
					state.durationCounter = 0;
					LOG_MIX("UpdateSequencer: ch%d duration expired, parsing next", ch);
					ParseAndUpdateChannel(ch);
				}
			}
		}
	}
}

// Streaming buffer notification thread
DWORD WINAPI StreamingThreadProc(LPVOID lpParameter)
{
	info("Streaming thread started");
	
	DWORD sectionIndex = 0;
	
	while (!shutdownStreaming)
	{
		// Wait for next section to need filling
		DWORD result = WaitForMultipleObjects(BUFFER_SECTIONS, notifyEvents, FALSE, INFINITE);
		
		if (result >= WAIT_OBJECT_0 && result < WAIT_OBJECT_0 + BUFFER_SECTIONS)
		{
			EnterCriticalSection(&g_csStream);
			sectionIndex = result - WAIT_OBJECT_0;
			
			// Lock buffer section
			LPVOID pBuffer1, pBuffer2;
			DWORD dwBytes1, dwBytes2;
			DWORD offset = sectionIndex * SECTION_SIZE_BYTES;
			
			HRESULT hr = pStreamingBuffer->Lock(offset, SECTION_SIZE_BYTES,
				&pBuffer1, &dwBytes1, &pBuffer2, &dwBytes2, 0);
			
			if (SUCCEEDED(hr))
			{
				// Interleaved mixing and sequencer updates for accurate timing (60Hz resolution)
				// We mix small chunks (one tick size ~735 samples) then update sequencer, then mix next chunk
				
				DWORD samplesBytes1 = dwBytes1 / (sizeof(SHORT) * NUM_CHANNELS_OUTPUT);
				SHORT* pOutput = (SHORT*)pBuffer1;
				DWORD samplesProcessed = 0;
				DWORD samplesPerTickInt = (DWORD)SAMPLES_PER_TICK; 

				while (samplesProcessed < samplesBytes1)
				{
					DWORD chunk = samplesPerTickInt;
					if (samplesProcessed + chunk > samplesBytes1)
					{
						chunk = samplesBytes1 - samplesProcessed;
					}

					// Mix one tick portion
					MixChannels(pOutput, chunk);
					
					// Update sequencer (might advance state for next chunk)
					UpdateSequencer(chunk);
					
					pOutput += chunk * NUM_CHANNELS_OUTPUT;
					samplesProcessed += chunk;
				}

				// Handle wrap-around buffer (pBuffer2) if any
				if (pBuffer2 && dwBytes2 > 0)
				{
					DWORD samplesBytes2 = dwBytes2 / (sizeof(SHORT) * NUM_CHANNELS_OUTPUT);
					SHORT* pOutput2 = (SHORT*)pBuffer2;
					DWORD samplesProcessed2 = 0;

					while (samplesProcessed2 < samplesBytes2)
					{
						DWORD chunk = samplesPerTickInt;
						if (samplesProcessed2 + chunk > samplesBytes2)
						{
							chunk = samplesBytes2 - samplesProcessed2;
						}

						MixChannels(pOutput2, chunk);
						UpdateSequencer(chunk);
						
						pOutput2 += chunk * NUM_CHANNELS_OUTPUT;
						samplesProcessed2 += chunk;
					}
				}
				
				pStreamingBuffer->Unlock(pBuffer1, dwBytes1, pBuffer2, dwBytes2);
			}
			LeaveCriticalSection(&g_csStream);
		}
	}
	
	info("Streaming thread stopped");
	return 0;
}

float restMultiplier_OLD = 4.4f; // Keep old variable for compatibility

// === Logging and Diagnostic Functions ===

void LogChannelState(BYTE channel, const char* context)
{
	if (!g_enableDetailedLogging) return;
	
	if (channel >= CHANNELS) return;
	
	ChannelParseState& state = channelState[channel];
	ChannelOscillator& osc = channelOscillator[channel];
	
	LOG_BUFFER("=== Channel %u State (%s) ===", channel, context);
	LOG_BUFFER("  Parse State: active=%d cursor=0x%04X patch=%d level=%d speed=%d",
		state.isActive, state.cursor, state.currentPatch, state.currentLevel, state.currentSpeed);
	LOG_BUFFER("  Timing: duration=%d durationCtr=%d tempo=%d tempoFrac=%d tickCtr=%d",
		state.durationTicks, state.durationCounter, state.localTempo, 
		state.tempoFractionAccumulator, state.tickCounter);
	LOG_BUFFER("  Oscillator: active=%d start=0x%X end=0x%X loop=0x%X",
		osc.active, osc.sampleStart, osc.sampleEnd, osc.sampleLoop);
	LOG_BUFFER("  Audio: pos=0x%X.%02X inc=0x%X vol=%d",
		osc.samplePos >> 8, (osc.samplePos & 0xFF) * 100 / 256,
		osc.sampleIncrement, osc.volume);
}

// Compatibility wrappers for old logging calls
void LogBufferState(BYTE channel, const char* context)
{
	LogChannelState(channel, context);
}

void LogSampleQueue(BYTE channel)
{
	// No longer applicable with streaming architecture
	// Keep for compatibility but do nothing
}

void DumpSampleData(BYTE channel, WORD sample, DWORD offset, DWORD size)
{
	if (!g_enableFileLogging || !g_logFile) return;
	
	if (size > 256) size = 256; // Limit dump size
	
	fprintf(g_logFile, "\n[DUMP] Channel %u, Sample %u, Offset 0x%X, Size %u:\n", channel, sample, offset, size);
	fprintf(g_logFile, "       ");
	
	for (DWORD i = 0; i < size && (offset + i) < bufferSize; i++)
	{
		if (i > 0 && i % 16 == 0)
			fprintf(g_logFile, "\n       ");
		fprintf(g_logFile, "%02X ", pDataBuffer[offset + i]);
	}
	
	fprintf(g_logFile, "\n");
	fflush(g_logFile);
}

void LogMusicCode(INT code)
{
	// List of known music codes for Star Trek Voyager
	const int musicCodes[] = {4, 5, 6, 7, 20, 22, 26, 36, 188, 189, 196, 268, 301};
	BOOL isMusic = FALSE;
	
	for (int mc : musicCodes)
	{
		if (code == mc)
		{
			isMusic = TRUE;
			break;
		}
	}
	
	if (isMusic)
	{
		LOG_MUSIC(">>> MUSIC CODE DETECTED: %d <<<", code);
	}
}

// === Sample Header Analysis Tool ===
__declspec(dllexport) void snd_analyze_code(INT code)
{
	if (!bufferReady) 
	{ 
		info("OpenSndVoyager::snd_analyze_code ERROR! Buffer not ready!"); 
		return; 
	}
	
	info("===========================================");
	info("=== ANALYZING CODE: %d ===", code);
	info("===========================================");
	
	WORD hOffset = pSampleHeaders[code * 2] << 8 | pSampleHeaders[code * 2 + 1];
	info("Header Offset: 0x%04X", hOffset);
	
	BYTE* pSampleHeader = pSampleHeaders + hOffset;
	
	BYTE channel = pSampleHeader[0];
	BYTE unknown1 = pSampleHeader[1];
	BYTE unknown2 = pSampleHeader[2];
	
	info("Channel: %u | Unknown1: 0x%02X | Unknown2: 0x%02X", channel, unknown1, unknown2);
	
	// Nothing
	if (pSampleHeader[3] == OC_EOS || channel == 0x00)
	{
		info("CODE IS EMPTY (EOS or channel=0)");
		info("===========================================");
		return;
	}
	
	// Parse and display all opcodes without executing
	info("\\nOPCODE SEQUENCE:");
	WORD cursor = 3;
	WORD opcodeNum = 0;
	WORD sampleCount = 0;
	BOOL hasLoop = FALSE;
	WORD totalStay = 0;
	
	while (cursor < 1024 && pSampleHeader[cursor] != OC_EOS) // Safety limit
	{
		BYTE opcode = pSampleHeader[cursor];
		opcodeNum++;
		
		switch (opcode)
		{
		case OC_REST:
			info("[%03d @ 0x%04X] REST: delay=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_START_VOICE:
			info("[%03d @ 0x%04X] START_VOICE: code=%u", opcodeNum, cursor, pSampleHeader[cursor + 1] + 0x200);
			cursor += 2;
			break;
		case OC_SETATTEN:
			info("[%03d @ 0x%04X] SETATTEN: level=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SETPATCH:
		{
			WORD sample = pSampleHeader[cursor + 1] << 8 | pSampleHeader[cursor + 2];
			if (sample <= 676)
			{
				sampleCount++;
				if (pSampleHeader[cursor + 3] >= OC_SET_LUP_CNTL && pSampleHeader[cursor + 3] <= OC_DIAGNOSTIC)
				{
					info("[%03d @ 0x%04X] SETPATCH: sample=%u (short form)", opcodeNum, cursor, sample);
					cursor += 3;
				}
				else
				{
					BYTE speed = pSampleHeader[cursor + 3];
					info("[%03d @ 0x%04X] SETPATCH: sample=%u speed=0x%02X (%u Hz)",
						opcodeNum, cursor, sample, speed, SpeedToSampleRate(speed));
					cursor += 5;
				}
			}
			else
			{
				info("[%03d @ 0x%04X] SETPATCH: INVALID sample=%u", opcodeNum, cursor, sample);
				cursor += 1;
			}
			break;
		}
		case OC_SETPITCHOFFSET:
			info("[%03d @ 0x%04X] SETPITCHOFFSET: 0x%02X%02X", opcodeNum, cursor, pSampleHeader[cursor + 1], pSampleHeader[cursor + 2]);
			cursor += 3;
			break;
		case OC_STAY:
		{
			WORD stayVal = pSampleHeader[cursor + 1];
			totalStay += stayVal;
			info("[%03d @ 0x%04X] STAY: value=%u", opcodeNum, cursor, stayVal);
			cursor += 2;
			break;
		}
		case OC_SETENVELOPE:
			info("[%03d @ 0x%04X] SETENVELOPE: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_STRING_BRANCH:
		{
			WORD target = pSampleHeader[cursor + 1] << 8 | pSampleHeader[cursor + 2];
			info("[%03d @ 0x%04X] STRING_BRANCH: target=0x%04X *** LOOP POINT ***", opcodeNum, cursor, target);
			hasLoop = TRUE;
			cursor += 3;
			break;
		}
		case OC_SETPRIORITY:
			info("[%03d @ 0x%04X] SETPRIORITY: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SYSKILLBSMT:
			info("[%03d @ 0x%04X] SYSKILLBSMT: channel=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SEQSTARTVOICE:
		{
			WORD temp = pSampleHeader[cursor + 2] << 8 | pSampleHeader[cursor + 3];
			WORD realCode = *(pSampleHeaders + temp) + 0x200;
			info("[%03d @ 0x%04X] SEQSTARTVOICE: realCode=%u", opcodeNum, cursor, realCode);
			cursor += 4;
			break;
		}
		case OC_CHANNELATTEN:
			info("[%03d @ 0x%04X] CHANNELATTEN: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SETMUSICTEMPO:
			info("[%03d @ 0x%04X] SETMUSICTEMPO: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_LOCALTEMPO:
			info("[%03d @ 0x%04X] LOCALTEMPO: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SET_LUP_CNTL:
			info("[%03d @ 0x%04X] SET_LUP_CNTL: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SYSATTENDELTA:
			info("[%03d @ 0x%04X] SYSATTENDELTA: ch=%u delta=%u", opcodeNum, cursor, pSampleHeader[cursor + 1], pSampleHeader[cursor + 2]);
			cursor += 3;
			break;
		case OC_TEST_LUPC_JMP:
			info("[%03d @ 0x%04X] TEST_LUPC_JMP: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SETGLOBALVAR:
			info("[%03d @ 0x%04X] SETGLOBALVAR", opcodeNum, cursor);
			cursor += 1;
			break;
		case OC_GETMUSICTEMPO:
			info("[%03d @ 0x%04X] GETMUSICTEMPO", opcodeNum, cursor);
			cursor += 1;
			break;
		case OC_SETOCTOFF:
			info("[%03d @ 0x%04X] SETOCTOFF: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SETNOTEOFF:
			info("[%03d @ 0x%04X] SETNOTEOFF: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_SIGNEDATTEN:
			info("[%03d @ 0x%04X] SIGNEDATTEN: ch=%u value=0x%02X%02X", opcodeNum, cursor,
				pSampleHeader[cursor + 1], pSampleHeader[cursor + 2], pSampleHeader[cursor + 3]);
			cursor += 4;
			break;
		case OC_STRING_JSR:
			info("[%03d @ 0x%04X] STRING_JSR: target=0x%02X%02X", opcodeNum, cursor,
				pSampleHeader[cursor + 1], pSampleHeader[cursor + 2]);
			cursor += 3;
			break;
		case OC_RANDOMDURATION:
			info("[%03d @ 0x%04X] RANDOMDURATION: value=%u", opcodeNum, cursor, pSampleHeader[cursor + 1]);
			cursor += 2;
			break;
		case OC_EOS:
			info("[%03d @ 0x%04X] EOS", opcodeNum, cursor);
			cursor++;
			break;
		default:
			if (opcode >= OC_SET_LUP_CNTL && opcode <= OC_DIAGNOSTIC)
			{
				info("[%03d @ 0x%04X] UNKNOWN_OPCODE: 0x%02X", opcodeNum, cursor, opcode);
				cursor += 1;
			}
			else
			{
				info("[%03d @ 0x%04X] POSSIBLE_SPEED_BYTE: 0x%02X", opcodeNum, cursor, opcode);
				cursor += 2;
			}
			break;
		}
	}
	
	info("\\n=== SUMMARY ===");
	info("Total Opcodes: %u", opcodeNum);
	info("Samples Queued: %u", sampleCount);
	info("Total Stay Value: %u", totalStay);
	info("Has Loop Branch: %s", hasLoop ? "YES *** CRITICAL ***" : "NO");
	
	if (sampleCount == 1 && !hasLoop)
	{
		info("\\n!!! WARNING: Single sample without loop - WILL CAUSE 1-SECOND LOOP ISSUE !!!");
	}
	
	if (hasLoop && sampleCount > 0)
	{
		info("\\n!!! NOTE: Has loop branch but not implemented - music will still break !!!");
	}
	
	info("===========================================");
}


// Forward declarations
void ParseAndPlayNextNote(BYTE channel);
DWORD WINAPI UpdateThreadProc(LPVOID lpParameter);

// Check if a DirectSound buffer has finished playing
BOOL IsBufferFinished(BYTE channel)
{
	if (channel >= CHANNELS || channelBuffer[channel].dsBuffer == NULL)
		return FALSE;
		
	DWORD status = 0;
	if (FAILED(channelBuffer[channel].dsBuffer->GetStatus(&status)))
		return FALSE;
		
	// Buffer is finished if it's not playing
	return !(status & DSBSTATUS_PLAYING);
}

const BYTE blob[4256] = { 0x83, 0xF3, 0x00, 0x00, 0xB9, 0xFF, 0x00, 0x00, 0xA7, 0xFF, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0xB2, 0x8F, 0x00, 0x00, 0xE1, 0xB7, 0x00, 0x00, 0x79, 0xB7, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0xBE, 0xE8, 0x00, 0x00, 0xA9, 0xFB, 0x00, 0x00, 0x80, 0xF9, 0x00, 0x00, 0x32, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x99, 0x64, 0x00, 0x00, 0x18, 0x62, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x78, 0x00, 0x00, 0xF4, 0x46, 0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0xD2, 0xEC, 0x00, 0x00, 0xF5, 0xFF, 0x00, 0x00, 0xF1, 0xFF, 0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x40, 0x74, 0x00, 0x00, 0xC9, 0xBF, 0x00, 0x00, 0x40, 0xBF, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0xDF, 0xC0, 0x00, 0x00, 0xC4, 0xDD, 0x00, 0x00, 0x0A, 0xDC, 0x00, 0x00, 0x3A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x54, 0x00, 0x00, 0x3D, 0x54, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0xB2, 0xF1, 0x00, 0x00, 0x12, 0xFD, 0x00, 0x00, 0xFE, 0xFC, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x73, 0xCC, 0x00, 0x00, 0xB1, 0xF1, 0x00, 0x00, 0xA1, 0xF1, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x40, 0xCA, 0x00, 0x00, 0xF7, 0xEE, 0x00, 0x00, 0xF0, 0xEE, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00, 0x1E, 0x58, 0x00, 0x00, 0xCE, 0x93, 0x00, 0x00, 0x5F, 0x92, 0x00, 0x00, 0x3D, 0x00, 0x00, 0x00, 0x40, 0xCE, 0x00, 0x00, 0x54, 0xF4, 0x00, 0x00, 0x4C, 0xF4, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x9B, 0xB7, 0x00, 0x00, 0x94, 0xED, 0x00, 0x00, 0x62, 0xED, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x4A, 0xEE, 0x00, 0x00, 0xEE, 0xFA, 0x00, 0x00, 0xE8, 0xFA, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x36, 0xEC, 0x00, 0x00, 0x2E, 0xFF, 0x00, 0x00, 0x29, 0xFF, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0xBC, 0x9B, 0x00, 0x00, 0x63, 0xC8, 0x00, 0x00, 0xF8, 0xC2, 0x00, 0x00, 0x33, 0x00, 0x00, 0x00, 0xFA, 0xEE, 0x00, 0x00, 0x26, 0xF8, 0x00, 0x00, 0x21, 0xF8, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00, 0x71, 0xCF, 0x00, 0x00, 0x84, 0xF0, 0x00, 0x00, 0x47, 0xF0, 0x00, 0x00, 0x2C, 0x00, 0x00, 0x00, 0x16, 0xC6, 0x00, 0x00, 0x44, 0xE4, 0x00, 0x00, 0xF2, 0xE3, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x7E, 0xC3, 0x00, 0x00, 0x2E, 0xFE, 0x00, 0x00, 0x7D, 0xEC, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x5B, 0x7C, 0x00, 0x00, 0x0D, 0x41, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0xC3, 0xDB, 0x00, 0x00, 0x4A, 0xFE, 0x00, 0x00, 0x36, 0xFE, 0x00, 0x00, 0x25, 0x00, 0x00, 0x00, 0x34, 0xD9, 0x00, 0x00, 0x8C, 0xF2, 0x00, 0x00, 0x4A, 0xF1, 0x00, 0x00, 0x3D, 0x00, 0x00, 0x00, 0xD8, 0x5B, 0x00, 0x00, 0x9E, 0x98, 0x00, 0x00, 0x4C, 0x98, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xD6, 0x5E, 0x00, 0x00, 0xEC, 0x5C, 0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x6C, 0x7E, 0x00, 0x00, 0x64, 0x49, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x34, 0x7E, 0x00, 0x00, 0x36, 0x52, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x80, 0x78, 0x00, 0x00, 0xA2, 0xC7, 0x00, 0x00, 0x8B, 0xC7, 0x00, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x79, 0x00, 0x00, 0xDA, 0x79, 0x00, 0x00, 0x0B, 0x00, 0x00, 0x00, 0x65, 0xCC, 0x00, 0x00, 0xCF, 0xEC, 0x00, 0x00, 0xA5, 0xEA, 0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0xC0, 0xD0, 0x00, 0x00, 0x0C, 0xF7, 0x00, 0x00, 0xC8, 0xF4, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0xFC, 0xA7, 0x00, 0x00, 0x14, 0xDB, 0x00, 0x00, 0xD0, 0xD8, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x00, 0x5C, 0x00, 0x00, 0x1A, 0x99, 0x00, 0x00, 0x38, 0x97, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0xD4, 0x5B, 0x00, 0x00, 0x93, 0x98, 0x00, 0x00, 0x86, 0x98, 0x00, 0x00, 0x39, 0x00, 0x00, 0x00, 0x74, 0x5E, 0x00, 0x00, 0xB2, 0x9C, 0x00, 0x00, 0x44, 0x9B, 0x00, 0x00, 0x31, 0x00, 0x00, 0x00, 0xB4, 0x77, 0x00, 0x00, 0x25, 0xC6, 0x00, 0x00, 0x6A, 0xC5, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x00, 0xA5, 0x98, 0x00, 0x00, 0x1F, 0xC4, 0x00, 0x00, 0x0F, 0xC4, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0xA4, 0x5C, 0x00, 0x00, 0x0D, 0x9A, 0x00, 0x00, 0xA5, 0x5C, 0x00, 0x00, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xD7, 0x5B, 0x00, 0x00, 0x9E, 0x5B, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x8F, 0xEC, 0x00, 0x00, 0xCE, 0xFE, 0x00, 0x00, 0xAA, 0xFE, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0x3E, 0xEE, 0x00, 0x00, 0xB5, 0xF6, 0x00, 0x00, 0xB4, 0xF6, 0x00, 0x00, 0x0B, 0x00, 0x00, 0x00, 0xF4, 0xF3, 0x00, 0x00, 0x0C, 0xF8, 0x00, 0x00, 0xFC, 0xF7, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8B, 0x62, 0x00, 0x00, 0xCE, 0x61, 0x00, 0x00, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1B, 0x75, 0x00, 0x00, 0x0C, 0x75, 0x00, 0x00, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xAB, 0x71, 0x00, 0x00, 0x8D, 0x71, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0xE6, 0xB8, 0x00, 0x00, 0x15, 0xD5, 0x00, 0x00, 0xE6, 0xB8, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0xE7, 0x7A, 0x00, 0x00, 0xFE, 0xCA, 0x00, 0x00, 0xE4, 0xCA, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFA, 0x5B, 0x00, 0x00, 0xEC, 0x5B, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x40, 0xD3, 0x00, 0x00, 0x71, 0xF4, 0x00, 0x00, 0x40, 0xF4, 0x00, 0x00, 0x2A, 0x00, 0x00, 0x00, 0xD3, 0xBC, 0x00, 0x00, 0x33, 0xD9, 0x00, 0x00, 0xF3, 0xD8, 0x00, 0x00, 0x3D, 0x00, 0x00, 0x00, 0xB3, 0x9C, 0x00, 0x00, 0x4F, 0xCB, 0x00, 0x00, 0xF3, 0xCA, 0x00, 0x00, 0x31, 0x00, 0x00, 0x00, 0x00, 0x69, 0x00, 0x00, 0x95, 0xA9, 0x00, 0x00, 0x50, 0xA9, 0x00, 0x00, 0x23, 0x00, 0x00, 0x00, 0x15, 0xC7, 0x00, 0x00, 0x17, 0xE6, 0x00, 0x00, 0xF5, 0xE5, 0x00, 0x00, 0x34, 0x00, 0x00, 0x00, 0x0B, 0xEB, 0x00, 0x00, 0xBB, 0xFC, 0x00, 0x00, 0xB6, 0xFC, 0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0xD4, 0xB9, 0x00, 0x00, 0x16, 0xF0, 0x00, 0x00, 0x0D, 0xF0, 0x00, 0x00, 0x19, 0x00, 0x00, 0x00, 0x40, 0x5F, 0x00, 0x00, 0xC2, 0x9D, 0x00, 0x00, 0x60, 0x9D, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x95, 0x54, 0x00, 0x00, 0xA7, 0x53, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xE3, 0x5B, 0x00, 0x00, 0x2A, 0x5B, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0x4B, 0x62, 0x00, 0x00, 0x00, 0xA1, 0x00, 0x00, 0xF0, 0xA0, 0x00, 0x00, 0x2A, 0x00, 0x00, 0x00, 0x80, 0x59, 0x00, 0x00, 0xFF, 0x95, 0x00, 0x00, 0xAF, 0x95, 0x00, 0x00, 0x3B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x7A, 0x00, 0x00, 0x40, 0x79, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x9E, 0x65, 0x00, 0x00, 0xE8, 0x64, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0xE4, 0x5B, 0x00, 0x00, 0xC6, 0x98, 0x00, 0x00, 0xAA, 0x98, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x26, 0x77, 0x00, 0x00, 0x1C, 0x77, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xB3, 0x77, 0x00, 0x00, 0x40, 0x77, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x87, 0x6E, 0x00, 0x00, 0xF5, 0x6D, 0x00, 0x00, 0x1E, 0x00, 0x00, 0x00, 0x48, 0x94, 0x00, 0x00, 0x04, 0xBE, 0x00, 0x00, 0xFB, 0xBD, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0xBF, 0xAF, 0x00, 0x00, 0x34, 0xE5, 0x00, 0x00, 0xE2, 0xE4, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x91, 0xA8, 0x00, 0x00, 0xC2, 0xDB, 0x00, 0x00, 0x6E, 0xDB, 0x00, 0x00, 0x25, 0x00, 0x00, 0x00, 0x9B, 0x72, 0x00, 0x00, 0x66, 0xBC, 0x00, 0x00, 0x5E, 0xBC, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x00, 0x69, 0x00, 0x00, 0x91, 0xA9, 0x00, 0x00, 0xFA, 0xA8, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0xDA, 0xBA, 0x00, 0x00, 0xF0, 0xF1, 0x00, 0x00, 0xD3, 0xF1, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x6E, 0x72, 0x00, 0x00, 0x44, 0x72, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x95, 0x5D, 0x00, 0x00, 0xBC, 0x9B, 0x00, 0x00, 0xB9, 0x9B, 0x00, 0x00, 0x33, 0x00, 0x00, 0x00, 0xDB, 0xE7, 0x00, 0x00, 0x7C, 0xFF, 0x00, 0x00, 0xDC, 0xE7, 0x00, 0x00, 0x33, 0x00, 0x00, 0x00, 0xE2, 0x5F, 0x00, 0x00, 0x81, 0x9E, 0x00, 0x00, 0x80, 0x9E, 0x00, 0x00, 0x2B, 0x00, 0x00, 0x00, 0x9B, 0xD7, 0x00, 0x00, 0x0E, 0xFA, 0x00, 0x00, 0x0A, 0xFA, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0xCF, 0xD3, 0x00, 0x00, 0x3F, 0xF6, 0x00, 0x00, 0x3B, 0xF6, 0x00, 0x00, 0x29, 0x00, 0x00, 0x00, 0x16, 0xDB, 0x00, 0x00, 0x96, 0xFD, 0x00, 0x00, 0x91, 0xFD, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0xEA, 0xD7, 0x00, 0x00, 0x5D, 0xFA, 0x00, 0x00, 0x58, 0xFA, 0x00, 0x00, 0x27, 0x00, 0x00, 0x00, 0x80, 0x7B, 0x00, 0x00, 0x73, 0xCC, 0x00, 0x00, 0x18, 0xCB, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x98, 0xD1, 0x00, 0x00, 0x1C, 0xF8, 0x00, 0x00, 0x9D, 0xD1, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0xA8, 0xD3, 0x00, 0x00, 0xFF, 0xFA, 0x00, 0x00, 0xAC, 0xD3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xE6, 0xC6, 0x00, 0x00, 0x08, 0xEB, 0x00, 0x00, 0xED, 0xC6, 0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0xB4, 0xE1, 0x00, 0x00, 0xED, 0xFC, 0x00, 0x00, 0xE4, 0xFC, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0xDF, 0x6D, 0x00, 0x00, 0xBE, 0xAF, 0x00, 0x00, 0x81, 0xAF, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC8, 0x65, 0x00, 0x00, 0xE0, 0x64, 0x00, 0x00, 0x27, 0x00, 0x00, 0x00, 0x14, 0x5E, 0x00, 0x00, 0x48, 0x9C, 0x00, 0x00, 0x2E, 0x9C, 0x00, 0x00, 0x32, 0x00, 0x00, 0x00, 0x31, 0x9A, 0x00, 0x00, 0x14, 0xC7, 0x00, 0x00, 0x09, 0xC7, 0x00, 0x00, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F, 0x78, 0x00, 0x00, 0x70, 0x78, 0x00, 0x00, 0x0C, 0x00, 0x00, 0x00, 0xA7, 0xC4, 0x00, 0x00, 0x7A, 0xFF, 0x00, 0x00, 0x63, 0xFF, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xB7, 0x74, 0x00, 0x00, 0xA7, 0x74, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00, 0x81, 0x9E, 0x00, 0x00, 0x00, 0xD0, 0x00, 0x00, 0xE8, 0xCF, 0x00, 0x00, 0x2B, 0x00, 0x00, 0x00, 0x8C, 0x62, 0x00, 0x00, 0x75, 0xA1, 0x00, 0x00, 0x32, 0xA1, 0x00, 0x00, 0x29, 0x00, 0x00, 0x00, 0x9F, 0x65, 0x00, 0x00, 0xFE, 0xA4, 0x00, 0x00, 0xFC, 0xA4, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x25, 0x68, 0x00, 0x00, 0xFC, 0xA7, 0x00, 0x00, 0xF7, 0xA7, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0xE4, 0xF5, 0x00, 0x00, 0x98, 0xFC, 0x00, 0x00, 0x95, 0xFC, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x71, 0x00, 0x00, 0xFB, 0x71, 0x00, 0x00, 0x1A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x16, 0x58, 0x00, 0x00, 0x08, 0x58, 0x00, 0x00, 0x3D, 0x00, 0x00, 0x00, 0x00, 0x7A, 0x00, 0x00, 0xBD, 0xC9, 0x00, 0x00, 0x39, 0xC9, 0x00, 0x00, 0x0B, 0x00, 0x00, 0x00, 0x80, 0x71, 0x00, 0x00, 0x1F, 0xB7, 0x00, 0x00, 0xA0, 0xB6, 0x00, 0x00, 0x1C, 0x00, 0x00, 0x00, 0xE6, 0xEC, 0x00, 0x00, 0x18, 0xFC, 0x00, 0x00, 0x15, 0xFC, 0x00, 0x00, 0x1C, 0x00, 0x00, 0x00, 0x70, 0xEE, 0x00, 0x00, 0xE7, 0xFC, 0x00, 0x00, 0xDF, 0xFC, 0x00, 0x00, 0x2E, 0x00, 0x00, 0x00, 0x16, 0xCC, 0x00, 0x00, 0x36, 0xEC, 0x00, 0x00, 0xF6, 0xEB, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x13, 0x7C, 0x00, 0x00, 0x73, 0xCD, 0x00, 0x00, 0x68, 0xCD, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x5F, 0x00, 0x00, 0x13, 0x5F, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x05, 0xBE, 0x00, 0x00, 0x84, 0xDA, 0x00, 0x00, 0x6D, 0xDA, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x26, 0xC6, 0x00, 0x00, 0x86, 0xE9, 0x00, 0x00, 0x46, 0xE9, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x44, 0xF5, 0x00, 0x00, 0xB3, 0xFA, 0x00, 0x00, 0xB0, 0xFA, 0x00, 0x00, 0x16, 0x00, 0x00, 0x00, 0xA3, 0xE2, 0x00, 0x00, 0xF0, 0xFD, 0x00, 0x00, 0xDE, 0xFD, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x7C, 0x00, 0x00, 0x39, 0x7C, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFE, 0x75, 0x00, 0x00, 0x40, 0x75, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x82, 0xBD, 0x00, 0x00, 0x43, 0xF5, 0x00, 0x00, 0x82, 0xBD, 0x00, 0x00, 0x16, 0x00, 0x00, 0x00, 0xD1, 0x7F, 0x00, 0x00, 0xA2, 0xD3, 0x00, 0x00, 0xBE, 0xA9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA9, 0x6F, 0x00, 0x00, 0x48, 0xB4, 0x00, 0x00, 0x05, 0xB4, 0x00, 0x00, 0x1D, 0x00, 0x00, 0x00, 0x5C, 0x7C, 0x00, 0x00, 0x81, 0xCE, 0x00, 0x00, 0xFD, 0xCD, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x8D, 0xF2, 0x00, 0x00, 0x92, 0xFF, 0x00, 0x00, 0x7C, 0xFF, 0x00, 0x00, 0x3D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x5E, 0x00, 0x00, 0xF8, 0x5C, 0x00, 0x00, 0x32, 0x00, 0x00, 0x00, 0xF7, 0xB2, 0x00, 0x00, 0x90, 0xE8, 0x00, 0x00, 0x24, 0xE8, 0x00, 0x00, 0x1E, 0x00, 0x00, 0x00, 0xA7, 0x9D, 0x00, 0x00, 0x89, 0xCD, 0x00, 0x00, 0x55, 0xCD, 0x00, 0x00, 0x2E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA9, 0x6F, 0x00, 0x00, 0xE4, 0x6E, 0x00, 0x00, 0x1D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x90, 0x58, 0x00, 0x00, 0x2C, 0x58, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xD6, 0x72, 0x00, 0x00, 0xCA, 0x72, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xDC, 0x5B, 0x00, 0x00, 0xB4, 0x5B, 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x27, 0x5F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2E, 0x00, 0x00, 0x00, 0xCF, 0xF7, 0x00, 0x00, 0x21, 0xFF, 0x00, 0x00, 0x17, 0xFF, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0xB0, 0x68, 0x00, 0x00, 0x91, 0xA8, 0x00, 0x00, 0x2C, 0xA8, 0x00, 0x00, 0x25, 0x00, 0x00, 0x00, 0x40, 0x7A, 0x00, 0x00, 0x40, 0xCA, 0x00, 0x00, 0x3C, 0xCA, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA6, 0x69, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x22, 0x00, 0x00, 0x00, 0xFE, 0xA4, 0x00, 0x00, 0x9B, 0xD7, 0x00, 0x00, 0x98, 0xD7, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x25, 0x68, 0x00, 0x00, 0x22, 0x68, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0xCA, 0xBF, 0x00, 0x00, 0xCF, 0xF7, 0x00, 0x00, 0xCC, 0xF7, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x96, 0xA9, 0x00, 0x00, 0x8B, 0xDD, 0x00, 0x00, 0x88, 0xDD, 0x00, 0x00, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7A, 0x6E, 0x00, 0x00, 0x77, 0x6E, 0x00, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xB0, 0x68, 0x00, 0x00, 0xAD, 0x68, 0x00, 0x00, 0x25, 0x00, 0x00, 0x00, 0xC2, 0x5F, 0x00, 0x00, 0x4E, 0x9E, 0x00, 0x00, 0x4B, 0x9E, 0x00, 0x00, 0x2C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0x6D, 0x00, 0x00, 0x74, 0x6D, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0xC9, 0x65, 0x00, 0x00, 0x4D, 0xA5, 0x00, 0x00, 0x4A, 0xA5, 0x00, 0x00, 0x27, 0x00, 0x00, 0x00, 0xF6, 0xE9, 0x00, 0x00, 0x91, 0xFE, 0x00, 0x00, 0x8E, 0xFE, 0x00, 0x00, 0x1D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7E, 0x77, 0x00, 0x00, 0x7B, 0x77, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x9B, 0x72, 0x00, 0x00, 0x98, 0x72, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0xC3, 0x9D, 0x00, 0x00, 0xCC, 0xCD, 0x00, 0x00, 0xC9, 0xCD, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0xAC, 0x71, 0x00, 0x00, 0x9B, 0xB7, 0x00, 0x00, 0x98, 0xB7, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x87, 0xCF, 0x00, 0x00, 0xCE, 0xF5, 0x00, 0x00, 0xCB, 0xF5, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xAB, 0xC7, 0x00, 0x00, 0x1E, 0xEC, 0x00, 0x00, 0x0D, 0xEC, 0x00, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x40, 0x54, 0x00, 0x00, 0xB1, 0x8F, 0x00, 0x00, 0xA3, 0x8F, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0xDD, 0xC5, 0x00, 0x00, 0xA6, 0xE8, 0x00, 0x00, 0xA0, 0xE8, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0xFF, 0xCA, 0x00, 0x00, 0xFE, 0xEF, 0x00, 0x00, 0xE0, 0xEF, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x20, 0xE3, 0x00, 0x00, 0x76, 0xFE, 0x00, 0x00, 0x5B, 0xFE, 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x17, 0x90, 0x00, 0x00, 0xE5, 0xB8, 0x00, 0x00, 0xB6, 0xB8, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x8A, 0xF0, 0x00, 0x00, 0x72, 0xFF, 0x00, 0x00, 0x71, 0xFF, 0x00, 0x00, 0x2C, 0x00, 0x00, 0x00, 0x89, 0xCD, 0x00, 0x00, 0x70, 0xEE, 0x00, 0x00, 0x45, 0xEE, 0x00, 0x00, 0x2E, 0x00, 0x00, 0x00, 0x15, 0xD5, 0x00, 0x00, 0x4A, 0xEE, 0x00, 0x00, 0x1C, 0xEE, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x5F, 0xD1, 0x00, 0x00, 0xE3, 0xF7, 0x00, 0x00, 0xBE, 0xF7, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0xCC, 0xCD, 0x00, 0x00, 0xBB, 0xEE, 0x00, 0x00, 0x95, 0xEE, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x4D, 0xA5, 0x00, 0x00, 0xE9, 0xD7, 0x00, 0x00, 0xE6, 0xD7, 0x00, 0x00, 0x27, 0x00, 0x00, 0x00, 0xE0, 0xEA, 0x00, 0x00, 0xD5, 0xFD, 0x00, 0x00, 0xD4, 0xFD, 0x00, 0x00, 0x31, 0x00, 0x00, 0x00, 0xBB, 0xEE, 0x00, 0x00, 0x3F, 0xFD, 0x00, 0x00, 0x33, 0xFD, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x73, 0xCD, 0x00, 0x00, 0x83, 0xF3, 0x00, 0x00, 0x61, 0xF3, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0xEC, 0xD3, 0x00, 0x00, 0x8F, 0xEC, 0x00, 0x00, 0x8E, 0xEC, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0xFF, 0x95, 0x00, 0x00, 0x1F, 0xC0, 0x00, 0x00, 0x1E, 0xC0, 0x00, 0x00, 0x3B, 0x00, 0x00, 0x00, 0x49, 0x9C, 0x00, 0x00, 0x4E, 0xC9, 0x00, 0x00, 0x09, 0xC9, 0x00, 0x00, 0x32, 0x00, 0x00, 0x00, 0x54, 0x9D, 0x00, 0x00, 0x64, 0xCC, 0x00, 0x00, 0x14, 0xCC, 0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x77, 0x6D, 0x00, 0x00, 0xA8, 0xAE, 0x00, 0x00, 0x98, 0xAE, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0xD6, 0x72, 0x00, 0x00, 0xE2, 0xBD, 0x00, 0x00, 0xCA, 0xBD, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x4F, 0xC9, 0x00, 0x00, 0xBD, 0xE8, 0x00, 0x00, 0xA8, 0xE8, 0x00, 0x00, 0x32, 0x00, 0x00, 0x00, 0x84, 0xDA, 0x00, 0x00, 0x53, 0xF4, 0x00, 0x00, 0x33, 0xF4, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x90, 0xE8, 0x00, 0x00, 0xBE, 0xFB, 0x00, 0x00, 0xA7, 0xFB, 0x00, 0x00, 0x1E, 0x00, 0x00, 0x00, 0xF1, 0xC2, 0x00, 0x00, 0xDC, 0xDF, 0x00, 0x00, 0xC0, 0xDF, 0x00, 0x00, 0x39, 0x00, 0x00, 0x00, 0x05, 0xE4, 0x00, 0x00, 0xF4, 0xFF, 0x00, 0x00, 0xDC, 0xFF, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0x8B, 0xDD, 0x00, 0x00, 0x2D, 0xF9, 0x00, 0x00, 0x1A, 0xF9, 0x00, 0x00, 0x23, 0x00, 0x00, 0x00, 0xFF, 0xEF, 0x00, 0x00, 0xD2, 0xF9, 0x00, 0x00, 0xCA, 0xF9, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x1F, 0xEC, 0x00, 0x00, 0x1E, 0xFE, 0x00, 0x00, 0x0E, 0xFE, 0x00, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x7A, 0x6E, 0x00, 0x00, 0xD0, 0xB1, 0x00, 0x00, 0xFA, 0xB0, 0x00, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x4F, 0xE7, 0x00, 0x00, 0xE2, 0xFF, 0x00, 0x00, 0x9F, 0xFF, 0x00, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x90, 0x58, 0x00, 0x00, 0x48, 0x94, 0x00, 0x00, 0xD0, 0x93, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0xB2, 0xAE, 0x00, 0x00, 0x05, 0xE4, 0x00, 0x00, 0xD4, 0xE3, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0x87, 0x6E, 0x00, 0x00, 0xF7, 0xB2, 0x00, 0x00, 0xF4, 0xB2, 0x00, 0x00, 0x1E, 0x00, 0x00, 0x00, 0xC7, 0xDD, 0x00, 0x00, 0x94, 0xF8, 0x00, 0x00, 0x91, 0xF8, 0x00, 0x00, 0x3A, 0x00, 0x00, 0x00, 0xE4, 0x5B, 0x00, 0x00, 0xF6, 0x98, 0x00, 0x00, 0xF3, 0x98, 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x67, 0xBC, 0x00, 0x00, 0xF4, 0xF3, 0x00, 0x00, 0xF1, 0xF3, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x16, 0x72, 0x00, 0x00, 0xD4, 0xB9, 0x00, 0x00, 0xBD, 0xB9, 0x00, 0x00, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x13, 0x7C, 0x00, 0x00, 0xC7, 0x7B, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x29, 0xC4, 0x00, 0x00, 0xB4, 0xE1, 0x00, 0x00, 0x80, 0xE1, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x1F, 0xC0, 0x00, 0x00, 0xB3, 0xDC, 0x00, 0x00, 0x5B, 0xDC, 0x00, 0x00, 0x3B, 0x00, 0x00, 0x00, 0xDC, 0xDF, 0x00, 0x00, 0xEA, 0xFA, 0x00, 0x00, 0xA4, 0xFA, 0x00, 0x00, 0x39, 0x00, 0x00, 0x00, 0x9C, 0xC1, 0x00, 0x00, 0x2D, 0xFC, 0x00, 0x00, 0x1D, 0xFC, 0x00, 0x00, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xDA, 0x5F, 0x00, 0x00, 0xC8, 0x5F, 0x00, 0x00, 0x2B, 0x00, 0x00, 0x00, 0xE3, 0xBD, 0x00, 0x00, 0xE4, 0xF5, 0x00, 0x00, 0xB3, 0xF5, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x0D, 0x5A, 0x00, 0x00, 0x9F, 0x96, 0x00, 0x00, 0x8A, 0x96, 0x00, 0x00, 0x3A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xB1, 0x5E, 0x00, 0x00, 0x93, 0x5E, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x2D, 0x9D, 0x00, 0x00, 0x16, 0xCC, 0x00, 0x00, 0xF3, 0xCB, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x35, 0xE5, 0x00, 0x00, 0x9C, 0xFD, 0x00, 0x00, 0x8F, 0xFD, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x95, 0xED, 0x00, 0x00, 0xBD, 0xFD, 0x00, 0x00, 0x9E, 0xFD, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4B, 0x62, 0x00, 0x00, 0xBF, 0x61, 0x00, 0x00, 0x2A, 0x00, 0x00, 0x00, 0xE2, 0xB7, 0x00, 0x00, 0xEC, 0xD3, 0x00, 0x00, 0xB5, 0xD3, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0xB3, 0xDC, 0x00, 0x00, 0x2F, 0xF7, 0x00, 0x00, 0xDC, 0xF6, 0x00, 0x00, 0x3B, 0x00, 0x00, 0x00, 0x4E, 0x9E, 0x00, 0x00, 0x71, 0xCF, 0x00, 0x00, 0x3D, 0xCF, 0x00, 0x00, 0x2C, 0x00, 0x00, 0x00, 0x20, 0xB7, 0x00, 0x00, 0xE6, 0xEC, 0x00, 0x00, 0xD4, 0xEC, 0x00, 0x00, 0x1C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC3, 0x5B, 0x00, 0x00, 0xB3, 0x5B, 0x00, 0x00, 0x39, 0x00, 0x00, 0x00, 0x81, 0xCE, 0x00, 0x00, 0xB1, 0xF4, 0x00, 0x00, 0xAE, 0xF4, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x18, 0xE6, 0x00, 0x00, 0xAC, 0xFD, 0x00, 0x00, 0xA9, 0xFD, 0x00, 0x00, 0x34, 0x00, 0x00, 0x00, 0x6D, 0x7E, 0x00, 0x00, 0x98, 0xD1, 0x00, 0x00, 0x95, 0xD1, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x48, 0x99, 0x00, 0x00, 0x16, 0xC6, 0x00, 0x00, 0x13, 0xC6, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x50, 0xCB, 0x00, 0x00, 0xE0, 0xEA, 0x00, 0x00, 0xDD, 0xEA, 0x00, 0x00, 0x31, 0x00, 0x00, 0x00, 0x86, 0xE9, 0x00, 0x00, 0x33, 0xFF, 0x00, 0x00, 0x30, 0xFF, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x00, 0xB8, 0xDF, 0x00, 0x00, 0x61, 0xFB, 0x00, 0x00, 0x5C, 0xFB, 0x00, 0x00, 0x22, 0x00, 0x00, 0x00, 0xFA, 0xEE, 0x00, 0x00, 0x4D, 0xFF, 0x00, 0x00, 0x4A, 0xFF, 0x00, 0x00, 0x1A, 0x00, 0x00, 0x00, 0x01, 0xA1, 0x00, 0x00, 0x40, 0xD3, 0x00, 0x00, 0x3C, 0xD3, 0x00, 0x00, 0x2A, 0x00, 0x00, 0x00, 0x35, 0x7E, 0x00, 0x00, 0x5F, 0xD1, 0x00, 0x00, 0x5A, 0xD1, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0xA7, 0xE8, 0x00, 0x00, 0xAE, 0xFD, 0x00, 0x00, 0xAB, 0xFD, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0xD0, 0xB1, 0x00, 0x00, 0x4F, 0xE7, 0x00, 0x00, 0x4C, 0xE7, 0x00, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0x7D, 0x00, 0x00, 0xBD, 0x7D, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0xC7, 0x98, 0x00, 0x00, 0xD6, 0xC4, 0x00, 0x00, 0xD3, 0xC4, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0xD4, 0x93, 0x00, 0x00, 0xD3, 0xBC, 0x00, 0x00, 0xD0, 0xBC, 0x00, 0x00, 0x3D, 0x00, 0x00, 0x00, 0x21, 0xC5, 0x00, 0x00, 0x20, 0xE3, 0x00, 0x00, 0x0C, 0xE3, 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xD1, 0x7F, 0x00, 0x00, 0xCE, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0xB4, 0x00, 0x00, 0xF6, 0xE9, 0x00, 0x00, 0x4C, 0xE9, 0x00, 0x00, 0x1D, 0x00, 0x00, 0x00, 0xA6, 0x69, 0x00, 0x00, 0x7C, 0xAA, 0x00, 0x00, 0xCB, 0xA9, 0x00, 0x00, 0x22, 0x00, 0x00, 0x00, 0xF6, 0x98, 0x00, 0x00, 0x21, 0xC5, 0x00, 0x00, 0x7E, 0xC4, 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x7C, 0xAA, 0x00, 0x00, 0xB8, 0xDF, 0x00, 0x00, 0xA7, 0xDF, 0x00, 0x00, 0x22, 0x00, 0x00, 0x00, 0xCD, 0xB8, 0x00, 0x00, 0xFA, 0xEE, 0x00, 0x00, 0xB3, 0xEE, 0x00, 0x00, 0x1A, 0x00, 0x00, 0x00, 0x27, 0x5F, 0x00, 0x00, 0xA7, 0x9D, 0x00, 0x00, 0x1B, 0x9D, 0x00, 0x00, 0x2E, 0x00, 0x00, 0x00, 0x01, 0xD0, 0x00, 0x00, 0x30, 0xF1, 0x00, 0x00, 0x79, 0xF0, 0x00, 0x00, 0x2B, 0x00, 0x00, 0x00, 0xB1, 0x5E, 0x00, 0x00, 0x2D, 0x9D, 0x00, 0x00, 0x00, 0x9D, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0xD6, 0xC4, 0x00, 0x00, 0xA3, 0xE2, 0x00, 0x00, 0x17, 0xE2, 0x00, 0x00, 0x37, 0x00, 0x00, 0x00, 0xD7, 0x5E, 0x00, 0x00, 0x54, 0x9D, 0x00, 0x00, 0xC0, 0x9C, 0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x75, 0xA1, 0x00, 0x00, 0xCF, 0xD3, 0x00, 0x00, 0x4C, 0xD3, 0x00, 0x00, 0x29, 0x00, 0x00, 0x00, 0xE1, 0x7C, 0x00, 0x00, 0x87, 0xCF, 0x00, 0x00, 0x82, 0xCF, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x6F, 0x72, 0x00, 0x00, 0xDA, 0xBA, 0x00, 0x00, 0xD7, 0xBA, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x40, 0x7C, 0x00, 0x00, 0x40, 0xCE, 0x00, 0x00, 0x3D, 0xCE, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0xBE, 0xC9, 0x00, 0x00, 0x3E, 0xEE, 0x00, 0x00, 0x3B, 0xEE, 0x00, 0x00, 0x0B, 0x00, 0x00, 0x00, 0x10, 0x78, 0x00, 0x00, 0xE6, 0xC6, 0x00, 0x00, 0xE3, 0xC6, 0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x27, 0x77, 0x00, 0x00, 0xA7, 0xC4, 0x00, 0x00, 0xA4, 0xC4, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0xAC, 0x72, 0x00, 0x00, 0x82, 0xBD, 0x00, 0x00, 0x7F, 0xBD, 0x00, 0x00, 0x16, 0x00, 0x00, 0x00, 0xB8, 0x74, 0x00, 0x00, 0x78, 0xC0, 0x00, 0x00, 0x75, 0xC0, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00, 0x9F, 0x96, 0x00, 0x00, 0xDF, 0xC0, 0x00, 0x00, 0xDC, 0xC0, 0x00, 0x00, 0x3A, 0x00, 0x00, 0x00, 0x78, 0xC0, 0x00, 0x00, 0xF1, 0xF8, 0x00, 0x00, 0xEE, 0xF8, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x69, 0x00, 0x00, 0xFD, 0x68, 0x00, 0x00, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x71, 0x00, 0x00, 0x7D, 0x71, 0x00, 0x00, 0x1C, 0x00, 0x00, 0x00, 0x1C, 0x75, 0x00, 0x00, 0x9C, 0xC1, 0x00, 0x00, 0x99, 0xC1, 0x00, 0x00, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x74, 0x00, 0x00, 0x3D, 0x74, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x7B, 0x00, 0x00, 0x7D, 0x7B, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x59, 0x00, 0x00, 0x7D, 0x59, 0x00, 0x00, 0x3B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xAC, 0x72, 0x00, 0x00, 0xA9, 0x72, 0x00, 0x00, 0x16, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x16, 0x72, 0x00, 0x00, 0x13, 0x72, 0x00, 0x00, 0x19, 0x00, 0x00, 0x00, 0xD2, 0xDC, 0x00, 0x00, 0x63, 0xFF, 0x00, 0x00, 0x60, 0xFF, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xDF, 0x6D, 0x00, 0x00, 0xDC, 0x6D, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0xFF, 0x75, 0x00, 0x00, 0x7E, 0xC3, 0x00, 0x00, 0x7B, 0xC3, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC2, 0x5F, 0x00, 0x00, 0xBF, 0x5F, 0x00, 0x00, 0x2C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA4, 0x5C, 0x00, 0x00, 0xA1, 0x5C, 0x00, 0x00, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x95, 0x5D, 0x00, 0x00, 0x92, 0x5D, 0x00, 0x00, 0x33, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x74, 0x5E, 0x00, 0x00, 0x71, 0x5E, 0x00, 0x00, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xE1, 0x7C, 0x00, 0x00, 0xDE, 0x7C, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0D, 0x5A, 0x00, 0x00, 0x0A, 0x5A, 0x00, 0x00, 0x3A, 0x00, 0x00, 0x00, 0x96, 0x54, 0x00, 0x00, 0x17, 0x90, 0x00, 0x00, 0x14, 0x90, 0x00, 0x00, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xE7, 0x7A, 0x00, 0x00, 0xE4, 0x7A, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x7E, 0x77, 0x00, 0x00, 0xDD, 0xC5, 0x00, 0x00, 0xDA, 0xC5, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x49, 0xE4, 0x00, 0x00, 0xD1, 0xFF, 0x00, 0x00, 0xCE, 0xFF, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x00, 0x72, 0x00, 0x00, 0xCD, 0xB8, 0x00, 0x00, 0xCA, 0xB8, 0x00, 0x00, 0x1A, 0x00, 0x00, 0x00, 0xBE, 0xC8, 0x00, 0x00, 0xDB, 0xE7, 0x00, 0x00, 0xD8, 0xE7, 0x00, 0x00, 0x33, 0x00, 0x00, 0x00, 0x94, 0x98, 0x00, 0x00, 0xF1, 0xC2, 0x00, 0x00, 0xEE, 0xC2, 0x00, 0x00, 0x39, 0x00, 0x00, 0x00, 0xC0, 0x7D, 0x00, 0x00, 0xC0, 0xD0, 0x00, 0x00, 0xBD, 0xD0, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x92, 0xA9, 0x00, 0x00, 0xD2, 0xDC, 0x00, 0x00, 0xCF, 0xDC, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00 };
const FLOAT volumeLookup[11] = { 0.0f, 0.013362f, 0.032258f, 0.058982f, 0.096774f, 0.150221f, 0.225806f, 0.3327f, 0.483871f, 0.697659f, 1.0f };

// Make a byte from 2 ASCII bytes
int getByte(char msb, char lsb)
{
	int result = 0;
	char combo[2] = { msb, lsb };

	result = strtol(combo, NULL, 16);

	return result;
}

void SetMasterVolume(DWORD level)
{
	float volume = volumeLookup[level];
	info("OpenSndVoyager::SetMasterVolume level: %u volume: %f", level, volume);

	if (pPrimaryBuffer == NULL) return;
	
	// Convert linear volume to DirectSound decibels
	LONG dsVolume;
	if (volume <= 0.0f)
	{
		dsVolume = DSBVOLUME_MIN;
	}
	else
	{
		dsVolume = (LONG)(2000.0f * log10f(volume));
		if (dsVolume < DSBVOLUME_MIN) dsVolume = DSBVOLUME_MIN;
		if (dsVolume > DSBVOLUME_MAX) dsVolume = DSBVOLUME_MAX;
	}
	
	CHECK_HR(pPrimaryBuffer->SetVolume(dsVolume));
}

// === NEW Streaming Architecture Initialization ===

extern "C" __declspec(dllexport) void snd_init_new(DWORD level)
{
	__try
	{
		info("============================================");
		info("OpenSndVoyager::snd_init_new (STREAMING) level: %u", level);
		info("============================================");

		// Stop existing streaming thread
		if (streamingThread != NULL)
		{
			info("Stopping existing streaming thread...");
			shutdownStreaming = TRUE;
			
			// Signal all events to wake thread
			for (int i = 0; i < BUFFER_SECTIONS; i++)
			{
				if (notifyEvents[i]) SetEvent(notifyEvents[i]);
			}
			
			WaitForSingleObject(streamingThread, 2000);
			CloseHandle(streamingThread);
			streamingThread = NULL;
			shutdownStreaming = FALSE;
		}

		// Clean up existing resources
		if (pStreamingBuffer)
		{
			pStreamingBuffer->Stop();
			pStreamingBuffer->Release();
			pStreamingBuffer = NULL;
		}
		
		for (int i = 0; i < BUFFER_SECTIONS; i++)
		{
			if (notifyEvents[i])
			{
				CloseHandle(notifyEvents[i]);
				notifyEvents[i] = NULL;
			}
		}
		
		if (pNotify)
		{
			pNotify->Release();
			pNotify = NULL;
		}

		// Load binary audio data files (ROM samples)
		info("Loading ROM data...");
		if (!pDataBuffer)
		{
			pDataBuffer = new BYTE[bufferSize];
		}
		
		DWORD bytesRead = 0;
		char buf[MAX_PATH];
		memset(buf, 0, sizeof(buf));
		GetCurrentDirectoryA(256, buf);
		auto len = strlen(buf);
		buf[len] = '\\';
		strcat_s(buf, "stv\\arom\\");

		for (BYTE i = 0; i < 4; i++)
		{
			char filePath[MAX_PATH];
			sprintf_s(filePath, "%svoyager%u.bin", buf, i);
			info("Loading %s", filePath);

			HANDLE hFile = CreateFileA(filePath, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
			if (hFile == INVALID_HANDLE_VALUE)
			{
				info("ERROR! Opening file failed (%u)", GetLastError());
				return;
			}

			DWORD fileSize = GetFileSize(hFile, NULL);
			if (fileSize != binSize)
			{
				info("ERROR! Wrong file size! (%u)", fileSize);
				CloseHandle(hFile);
				return;
			}

			if (!ReadFile(hFile, pDataBuffer + binSize * i, binSize, &bytesRead, NULL))
			{
				info("ERROR! ReadFile failed for voyager%u.bin (%u)", i, GetLastError());
				CloseHandle(hFile);
				return;
			}
			
			CloseHandle(hFile);
		}

		// Convert to unsigned (ROM data is signed 8-bit)
		for (size_t i = 0; i < bufferSize; i++)
		{
			pDataBuffer[i] = (BYTE)((char)pDataBuffer[i] + 128);
		}

		// Load hex file (sample headers)
		char hexPath[MAX_PATH];
		sprintf_s(hexPath, "%scpuarom.hex", buf);
		info("Loading %s", hexPath);
		
		HANDLE hHexFile = CreateFileA(hexPath, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hHexFile == INVALID_HANDLE_VALUE)
		{
			info("ERROR! Opening hex file failed (%u)", GetLastError());
			return;
		}

		DWORD hexFileSize = GetFileSize(hHexFile, NULL);
		if (hexFileSize != hexSize)
		{
			info("ERROR! Wrong hex file size! (%u)", hexFileSize);
			CloseHandle(hHexFile);
			return;
		}

		if (!ReadFile(hHexFile, pHexBuffer, hexSize, &bytesRead, NULL))
		{
			info("ERROR! ReadFile failed for cpuarom.hex (%u)", GetLastError());
			CloseHandle(hHexFile);
			return;
		}
		
		CloseHandle(hHexFile);

		// Parse hex file to extract sample headers
		DWORD line = 0;
		BOOL done = FALSE;
		DWORD lineSize = 44;

		while (!done)
		{
			DWORD lOffset = lineSize * line;
			BYTE start = pHexBuffer[lOffset + 0];
			BYTE count = getByte(pHexBuffer[lOffset + 1], pHexBuffer[lOffset + 2]);
			WORD address = getByte(pHexBuffer[lOffset + 3], pHexBuffer[lOffset + 4]) << 8 | 
			               getByte(pHexBuffer[lOffset + 5], pHexBuffer[lOffset + 6]);
			BYTE type = getByte(pHexBuffer[lOffset + 7], pHexBuffer[lOffset + 8]);

			if (type == 0x01)
			{
				done = TRUE;
				continue;
			}

			if (start == 0x3A && count == 0x10 && address >= 0x00 && address <= sampleHeadersSize)
			{
				DWORD cursor = address;
				for (BYTE i = 9; i < 41; i += 2)
				{
					BYTE data = getByte(pHexBuffer[lOffset + i], pHexBuffer[lOffset + i + 1]);
					pSampleHeaders[cursor] = data;
					cursor++;
				}
			}
			else
			{
				info("ERROR! Hex parse error at line %d", line);
				return;
			}

			line++;
		}

		// Initialize DirectSound8
		HRESULT hrCom = CoInitializeEx(nullptr, COINIT_MULTITHREADED);
		if (FAILED(hrCom) && hrCom != RPC_E_CHANGED_MODE && hrCom != S_FALSE)
		{
			info("ERROR! CoInitializeEx failed: %08x", hrCom);
			return;
		}
		
		HRESULT hr = DirectSoundCreate8(NULL, &pDirectSound, NULL);
		if (FAILED(hr))
		{
			info("ERROR! DirectSoundCreate8 failed: %08x", hr);
			return;
		}
		
		HWND hwnd = GetForegroundWindow();
		if (hwnd == NULL)
		{
			hwnd = GetDesktopWindow();
			info("Using desktop window for DirectSound");
		}
		
		hr = pDirectSound->SetCooperativeLevel(hwnd, DSSCL_PRIORITY);
		if (FAILED(hr))
		{
			info("ERROR! SetCooperativeLevel failed: %08x", hr);
			pDirectSound->Release();
			pDirectSound = NULL;
			return;
		}
		
		// Create primary buffer
		DSBUFFERDESC primaryDesc = { 0 };
		primaryDesc.dwSize = sizeof(DSBUFFERDESC);
		primaryDesc.dwFlags = DSBCAPS_PRIMARYBUFFER | DSBCAPS_CTRLVOLUME;
		
		hr = pDirectSound->CreateSoundBuffer(&primaryDesc, &pPrimaryBuffer, NULL);
		if (FAILED(hr))
		{
			info("ERROR! CreateSoundBuffer (primary) failed: %08x", hr);
			pDirectSound->Release();
			pDirectSound = NULL;
			return;
		}
		
		// Set primary buffer format to match our streaming format
		WAVEFORMATEX primaryFormat = { 0 };
		primaryFormat.wFormatTag = WAVE_FORMAT_PCM;
		primaryFormat.nChannels = NUM_CHANNELS_OUTPUT;
		primaryFormat.nSamplesPerSec = SAMPLE_RATE;
		primaryFormat.wBitsPerSample = BITS_PER_SAMPLE;
		primaryFormat.nBlockAlign = (primaryFormat.wBitsPerSample * primaryFormat.nChannels) / 8;
		primaryFormat.nAvgBytesPerSec = primaryFormat.nSamplesPerSec * primaryFormat.nBlockAlign;
		
		hr = pPrimaryBuffer->SetFormat(&primaryFormat);
		if (FAILED(hr))
		{
			info("WARNING! SetFormat on primary buffer failed: %08x", hr);
		}
		
		hr = pPrimaryBuffer->Play(0, 0, DSBPLAY_LOOPING);
		if (FAILED(hr))
		{
			info("WARNING! Primary buffer Play failed: %08x", hr);
		}

		SetMasterVolume(level);

		// Create streaming buffer with notifications
		WAVEFORMATEX streamFormat = { 0 };
		streamFormat.wFormatTag = WAVE_FORMAT_PCM;
		streamFormat.nChannels = NUM_CHANNELS_OUTPUT;
		streamFormat.nSamplesPerSec = SAMPLE_RATE;
		streamFormat.wBitsPerSample = BITS_PER_SAMPLE;
		streamFormat.nBlockAlign = (streamFormat.wBitsPerSample * streamFormat.nChannels) / 8;
		streamFormat.nAvgBytesPerSec = streamFormat.nSamplesPerSec * streamFormat.nBlockAlign;
		
		DSBUFFERDESC streamDesc = { 0 };
		streamDesc.dwSize = sizeof(DSBUFFERDESC);
		// DSBCAPS_GLOBALFOCUS so audio continues when window loses focus
		streamDesc.dwFlags = DSBCAPS_GLOBALFOCUS | DSBCAPS_CTRLPOSITIONNOTIFY | DSBCAPS_GETCURRENTPOSITION2;
		streamDesc.dwBufferBytes = STREAMING_BUFFER_SIZE;
		streamDesc.lpwfxFormat = &streamFormat;
		
		hr = pDirectSound->CreateSoundBuffer(&streamDesc, (LPDIRECTSOUNDBUFFER*)&pStreamingBuffer, NULL);
		if (FAILED(hr))
		{
			info("ERROR! CreateSoundBuffer (streaming) failed: %08x", hr);
			return;
		}
		
		// Set up notification events
		hr = pStreamingBuffer->QueryInterface(IID_IDirectSoundNotify8, (LPVOID*)&pNotify);
		if (FAILED(hr))
		{
			info("ERROR! QueryInterface for IDirectSoundNotify8 failed: %08x", hr);
			return;
		}
		
		DSBPOSITIONNOTIFY posNotify[BUFFER_SECTIONS];
		for (int i = 0; i < BUFFER_SECTIONS; i++)
		{
			notifyEvents[i] = CreateEvent(NULL, FALSE, FALSE, NULL);
			posNotify[i].dwOffset = (i * SECTION_SIZE_BYTES) + SECTION_SIZE_BYTES - 1;
			posNotify[i].hEventNotify = notifyEvents[i];
			
			info("Notification %d at offset %u", i, posNotify[i].dwOffset);
		}
		
		hr = pNotify->SetNotificationPositions(BUFFER_SECTIONS, posNotify);
		if (FAILED(hr))
		{
			info("ERROR! SetNotificationPositions failed: %08x", hr);
			return;
		}

		// Initialize all channel oscillators and parse states
		for (BYTE ch = 0; ch < CHANNELS; ch++)
		{
			ZeroMemory(&channelOscillator[ch], sizeof(ChannelOscillator));
			ZeroMemory(&channelState[ch], sizeof(ChannelParseState));
			channelState[ch].currentLevel = 255; // Default volume
			channelState[ch].currentSpeed = 0x20; // Default speed
			channelState[ch].localTempo = 255; // Default tempo (like original initsnd_6809)
			channelState[ch].tempoFractionAccumulator = 0;
			channelState[ch].tickCounter = 0;
		}

		// Fill buffer with silence initially
		LPVOID pBuffer;
		DWORD dwBytes;
		hr = pStreamingBuffer->Lock(0, STREAMING_BUFFER_SIZE, &pBuffer, &dwBytes, NULL, NULL, 0);
		if (SUCCEEDED(hr))
		{
			ZeroMemory(pBuffer, dwBytes);
			pStreamingBuffer->Unlock(pBuffer, dwBytes, NULL, 0);
		}

		// Start streaming thread
		streamingThread = CreateThread(NULL, 0, StreamingThreadProc, NULL, 0, NULL);
		if (streamingThread == NULL)
		{
			info("ERROR! Failed to create streaming thread");
			return;
		}
		
		info("Streaming thread created");

		// Start playback
		hr = pStreamingBuffer->Play(0, 0, DSBPLAY_LOOPING);
		if (FAILED(hr))
		{
			info("ERROR! Streaming buffer Play failed: %08x", hr);
			return;
		}

		bufferReady = TRUE;
		info("snd_init_new complete - streaming architecture active");
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		info("EXCEPTION in snd_init_new! Code: %08x", GetExceptionCode());
	}
}

// Process channels that need parsing (called outside XAudio2 callbacks)
void ProcessPendingParses()
{
	// Disabling old parser logic completely
	return;

	/*
	for (BYTE channel = 0; channel < CHANNELS; channel++)
	{
		if (channelBuffer[channel].needsParse && channelBuffer[channel].isActive)
		{
			channelBuffer[channel].needsParse = FALSE;
			LOG_LOOP("Processing pending parse for channel %u", channel);
			ParseAndPlayNextNote(channel);
		}
	}
	*/
}

// Background thread that continuously processes pending parses (mimics original update_sequencer)
DWORD WINAPI UpdateThreadProc(LPVOID lpParameter)
{
	info("Update thread started (DISABLED)");
	
	while (!shutdownThread)
	{
		Sleep(100);
	}
	return 0;
}

/* Old UpdateThreadProc code removed */

extern "C" __declspec(dllexport) void snd_code_always(INT code)
{
	// Process any pending parses first
	ProcessPendingParses();
	
	info("OpenSndVoyager::snd_code_always code: %u", code);

}

// === Periodic Status Report ===
extern "C" __declspec(dllexport) void snd_get_channel_status()
{
	static ULONGLONG lastReportTime = 0;
	ULONGLONG currentTime = GetTickCount64();
	
	// Report every 2 seconds
	if (currentTime - lastReportTime < 2000)
		return;
		
	lastReportTime = currentTime;
	
	info("=== CHANNEL STATUS REPORT ===");
	WORD activeChannels = 0;
	
	for (BYTE i = 0; i < CHANNELS; i++)
	{
		if (channelBuffer[i].playing || channelBuffer[i].dsBuffer != NULL)
		{
			activeChannels++;
			info("Channel %02u: Playing=%s Buffer=%s Loop=%s Queue=%d/%d",
				i,
				channelBuffer[i].playing ? "Y" : "N",
				channelBuffer[i].dsBuffer ? "Y" : "N",
				channelBuffer[i].loop ? "Y" : "N",
				channelBuffer[i].qIndex,
				channelBuffer[i].qSamples.size());
				
			if (channelBuffer[i].dsBuffer)
			{
				DWORD playPos = 0, writePos = 0;
				channelBuffer[i].dsBuffer->GetCurrentPosition(&playPos, &writePos);
				info("  PlayPos=%u WritePos=%u Size=%u", playPos, writePos, channelBuffer[i].audioDataSize);
			}
		}
	}
	
	if (activeChannels == 0)
	{
		info("No active channels");
	}
	else
	{
		info("Total active channels: %u", activeChannels);
	}
	info("=============================");
}


void SetChannelVolume(BYTE channel, BYTE level)
{
	info("OpenSndVoyager::SetChannelVolume channel: %u level: %u", channel, level);

	if (channel >= CHANNELS) { return; }

	if (channelBuffer[channel].dsBuffer == NULL) { return; }
	
	// Convert 0-255 level to DirectSound decibels (DSBVOLUME_MIN to DSBVOLUME_MAX)
	// DSBVOLUME_MAX is 0 (full volume), DSBVOLUME_MIN is -10000 (silence)
	float fVolume = (float)level / 255.0f;
	LONG dsVolume;
	
	if (fVolume <= 0.0f)
	{
		dsVolume = DSBVOLUME_MIN;
	}
	else
	{
		// Convert linear to decibels: dB = 2000 * log10(volume)
		// Range from -10000 (silence) to 0 (full volume)
		dsVolume = (LONG)(2000.0f * log10f(fVolume));
		if (dsVolume < DSBVOLUME_MIN) dsVolume = DSBVOLUME_MIN;
		if (dsVolume > DSBVOLUME_MAX) dsVolume = DSBVOLUME_MAX;
	}
	
	CHECK_HR(channelBuffer[channel].dsBuffer->SetVolume(dsVolume));
}

void SetupChannel(BYTE channel)
{
	info("OpenSndVoyager::SetupChannel channel: %u", channel);
	LogSampleQueue(channel);

	if (channel >= CHANNELS) { return; }

	WORD sample = channelBuffer[channel].qSamples[0].sample;
	BYTE speed = channelBuffer[channel].qSamples[0].speed;
	BYTE level = channelBuffer[channel].qSamples[0].level;
	WORD stay = channelBuffer[channel].qSamples[0].stay;
	BOOL loop = channelBuffer[channel].loop;

	info("OpenSndVoyager::SetupChannel stay: %u loop: %s", stay, loop ? "YES" : "NO");

	DWORD base = 0;
	DWORD offsetBegin = 0;
	DWORD offsetEnd = 0;
	DWORD offsetOther = 0;
	DWORD size = 0;

	// Rest
	if (sample == 0xFFFF)
	{
		// Calculate how many bytes we need for ms rest
		WORD ms = (WORD)((float)level * restMultiplier);
		size = (DWORD)(SpeedToSampleRate(speed) * ms / 1000);
		level = 0;
		info("OpenSndVoyager::SetupChannel rest ms: %u size: %u", ms, size);
		
		// Skip zero-duration rests
		if (size == 0 || ms == 0)
		{
			info("OpenSndVoyager::SetupChannel skipping zero-duration REST");
			channelBuffer[channel].playing = FALSE;
			channelBuffer[channel].needsParse = TRUE;
			return;
		}
	}
	else
	{
		DWORD* pHeader = (DWORD*)blob + (sample * 4);

		base = pHeader[3] * 0x10000;
		offsetBegin = base + pHeader[0];
		offsetEnd = base + pHeader[1];
		offsetOther = base + pHeader[2];
		size = offsetEnd - offsetBegin;
	}

	info("OpenSndVoyager::SetupChannel channel: %X sample: %u speed: %X", channel, sample, speed);
	info("OpenSndVoyager::SetupChannel base: %X offsetBegin: %X offsetEnd: %X offsetOther: %X size: %X", base, offsetBegin, offsetEnd, offsetOther, size);
	
	if (sample != 0xFFFF)
	{
		DumpSampleData(channel, sample, offsetBegin, size);
	}

	// Setup new channel data
	auto sampleRate = SpeedToSampleRate(speed);
	auto sampleBits = 8;
	auto channels = 1;

	channelBuffer[channel].playing = TRUE;
	channelBuffer[channel].loop = loop;
	channelBuffer[channel].qIndex = 0;

	// Setup DirectSound format
	channelBuffer[channel].dsFormat.nAvgBytesPerSec = (sampleRate * sampleBits * channels) / 8;
	channelBuffer[channel].dsFormat.nSamplesPerSec = sampleRate;
	channelBuffer[channel].dsFormat.wBitsPerSample = sampleBits;
	channelBuffer[channel].dsFormat.nChannels = channels;
	channelBuffer[channel].dsFormat.wFormatTag = WAVE_FORMAT_PCM;
	channelBuffer[channel].dsFormat.nBlockAlign = (sampleBits * channels) / 8;

	// Calculate actual buffer size based on loop count
	DWORD actualBufferSize = size;
	DWORD loopCount = 0;
	
	if (loop)
	{
		LOG_LOOP("Setting INFINITE loop for channel %u", channel);
		// For infinite loop, we'll use DSBPLAY_LOOPING flag when playing
	}
	else if (stay > 0)
	{
		float ms = (float)stay * restMultiplier;
		float length = (float)size / sampleRate * 1000;
		loopCount = (WORD)(ms / length);

		LOG_LOOP("SetupChannel STAY: ms: %f length: %f lCount: %u", ms, length, loopCount);

		if (loopCount > 1)
		{
			LOG_LOOP("Setting finite loop for channel %u: count=%u", channel, loopCount);
			// For finite looping, repeat the data in the buffer
			actualBufferSize = size * loopCount;
		}
	}

	// Create DirectSound secondary buffer
	DSBUFFERDESC bufferDesc = { 0 };
	bufferDesc.dwSize = sizeof(DSBUFFERDESC);
	bufferDesc.dwFlags = DSBCAPS_CTRLVOLUME | DSBCAPS_CTRLFREQUENCY | DSBCAPS_GETCURRENTPOSITION2 | DSBCAPS_GLOBALFOCUS;
	bufferDesc.dwBufferBytes = actualBufferSize;
	bufferDesc.lpwfxFormat = &channelBuffer[channel].dsFormat;
	
	LPDIRECTSOUNDBUFFER pTempBuffer = NULL;
	CHECK_HR(pDirectSound->CreateSoundBuffer(&bufferDesc, &pTempBuffer, NULL));
	CHECK_HR(pTempBuffer->QueryInterface(IID_IDirectSoundBuffer8, (LPVOID*)&channelBuffer[channel].dsBuffer));
	pTempBuffer->Release();
	
	// Lock buffer and write audio data
	LPVOID pBuffer1, pBuffer2;
	DWORD dwBytes1, dwBytes2;
	CHECK_HR(channelBuffer[channel].dsBuffer->Lock(0, actualBufferSize, &pBuffer1, &dwBytes1, &pBuffer2, &dwBytes2, 0));
	
	// Copy audio data from pDataBuffer starting at offsetBegin
	if (sample == 0xFFFF)
	{
		// Fill with silence for REST (0x80 is silent for 8-bit PCM)
		memset(pBuffer1, 0x80, dwBytes1);
		if (pBuffer2 && dwBytes2 > 0)
		{
			memset(pBuffer2, 0x80, dwBytes2);
		}
	}
	else if (loopCount > 1)
	{
		// Copy the sample multiple times for finite looping
		for (DWORD i = 0; i < loopCount; i++)
		{
			memcpy((BYTE*)pBuffer1 + (i * size), pDataBuffer + offsetBegin, size);
		}
	}
	else
	{
		memcpy(pBuffer1, pDataBuffer + offsetBegin, size);
	}
	
	if (pBuffer2 && dwBytes2 > 0)
	{
		memcpy(pBuffer2, pDataBuffer + offsetBegin, dwBytes2);
	}
	
	CHECK_HR(channelBuffer[channel].dsBuffer->Unlock(pBuffer1, dwBytes1, pBuffer2, dwBytes2));
	
	// Store audio data info
	channelBuffer[channel].pAudioData = pDataBuffer + offsetBegin;
	channelBuffer[channel].audioDataSize = size;
	channelBuffer[channel].playPosition = 0;
	
	SetChannelVolume(channel, level);
	
	LogBufferState(channel, "SetupChannel Complete");
}

DWORD SpeedToSampleRate(BYTE speed)
{
	DWORD sampleRate = 0;

	// Use lookup table for known sound effect speeds
	if (speed == 0x20 || speed == 0x25 || speed == 0x26)
	{
		sampleRate = 8000;
	}
	else if (speed == 0x30 || speed == 0x31 || speed == 0x32 || speed == 0x2A)
	{
		sampleRate = 11025;
	}
	else if (speed == 0x36)
	{
		sampleRate = 16000;
	}
	else if (speed == 0x40 || speed == 0x41 || speed == 0x3B)
	{
		sampleRate = 22050;
	}
	else if (speed == 0x60)
	{
		sampleRate = 96000;
	}
	else
	{
		// For music speeds (typically 0x60-0x80 range), use exponential formula
		// Formula: frequency = baseFreq * 2^((speed - 0x40) / 64)
		// where 0x40 = 22050 Hz base for the formula
		const double BASE_FREQ = 22050.0;
		const double BASE_SPEED = 64.0; // 0x40
		
		double octaves = ((double)speed - BASE_SPEED) / 64.0;
		double frequency = BASE_FREQ * pow(2.0, octaves);
		
		// Clamp to reasonable range
		if (frequency < 1000.0) frequency = 1000.0;
		if (frequency > 48000.0) frequency = 48000.0;
		
		sampleRate = (DWORD)frequency;
		info("SpeedToSampleRate: speed=0x%02X (formula) octaves=%.2f freq=%d Hz", speed, octaves, sampleRate);
	}

	return sampleRate;
}

void PlayChannelNext(BYTE channel)
{
	info("OpenSndVoyager::PlayChannelNext channel: %u", channel);

	if (channel >= CHANNELS) { return; }

	channelBuffer[channel].qIndex++;

	WORD sample = channelBuffer[channel].qSamples[channelBuffer[channel].qIndex].sample;
	BYTE speed = channelBuffer[channel].qSamples[channelBuffer[channel].qIndex].speed;
	BYTE level = channelBuffer[channel].qSamples[channelBuffer[channel].qIndex].level;
	WORD stay = channelBuffer[channel].qSamples[channelBuffer[channel].qIndex].stay;

	info("OpenSndVoyager::SetupChannel stay: %u", stay);

	DWORD base = 0;
	DWORD offsetBegin = 0;
	DWORD offsetEnd = 0;
	DWORD offsetOther = 0;
	DWORD size = 0;

	// Rest
	if (sample == 0xFFFF)
	{
		// Calculate how many bytes we need for ms rest
		WORD ms = (WORD)((float)level * restMultiplier);
		size = (DWORD)(SpeedToSampleRate(speed) * ms / 1000);
		level = 0;
		info("OpenSndVoyager::PlayChannelNext rest ms: %u size: %u", ms, size);
	}
	else
	{
		DWORD* pHeader = (DWORD*)blob + (sample * 4);

		base = pHeader[3] * 0x10000;
		offsetBegin = base + pHeader[0];
		offsetEnd = base + pHeader[1];
		offsetOther = base + pHeader[2];
		size = offsetEnd - offsetBegin;
	}

	info("OpenSndVoyager::PlayChannelNext channel: %X sample: %u speed: %X", channel, sample, speed);
	info("OpenSndVoyager::PlayChannelNext base: %X offsetBegin: %X offsetEnd: %X offsetOther: %X size: %X", base, offsetBegin, offsetEnd, offsetOther, size);

	// Stop and recreate buffer with new sample
	if (channelBuffer[channel].dsBuffer != NULL)
	{
		channelBuffer[channel].dsBuffer->Stop();
		channelBuffer[channel].dsBuffer->Release();
		channelBuffer[channel].dsBuffer = NULL;
	}

	// Setup new channel data
	auto sampleRate = SpeedToSampleRate(speed);
	auto sampleBits = 8;
	auto channels = 1;

	// Setup DirectSound format
	WAVEFORMATEX dsFormat = { 0 };
	dsFormat.nAvgBytesPerSec = (sampleRate * sampleBits * channels) / 8;
	dsFormat.nSamplesPerSec = sampleRate;
	dsFormat.wBitsPerSample = sampleBits;
	dsFormat.nChannels = channels;
	dsFormat.wFormatTag = WAVE_FORMAT_PCM;
	dsFormat.nBlockAlign = (sampleBits * channels) / 8;

	DWORD actualBufferSize = size;
	DWORD loopCount = 0;

	if (stay > 0)
	{
		float ms = (float)stay * restMultiplier;
		float length = (float)size / sampleRate * 1000;
		loopCount = (WORD)(ms / length);

		info("OpenSndVoyager::PlayChannelNext ms: %f length: %f lCount: %u", ms, length, loopCount);

		if (loopCount > 1)
		{
			actualBufferSize = size * loopCount;
		}
	}
	
	// Create DirectSound secondary buffer
	DSBUFFERDESC bufferDesc = { 0 };
	bufferDesc.dwSize = sizeof(DSBUFFERDESC);
	bufferDesc.dwFlags = DSBCAPS_CTRLVOLUME | DSBCAPS_CTRLFREQUENCY | DSBCAPS_GETCURRENTPOSITION2;
	bufferDesc.dwBufferBytes = actualBufferSize;
	bufferDesc.lpwfxFormat = &dsFormat;
	
	LPDIRECTSOUNDBUFFER pTempBuffer = NULL;
	CHECK_HR(pDirectSound->CreateSoundBuffer(&bufferDesc, &pTempBuffer, NULL));
	CHECK_HR(pTempBuffer->QueryInterface(IID_IDirectSoundBuffer8, (LPVOID*)&channelBuffer[channel].dsBuffer));
	pTempBuffer->Release();
	
	// Lock buffer and write audio data
	LPVOID pBuffer1, pBuffer2;
	DWORD dwBytes1, dwBytes2;
	CHECK_HR(channelBuffer[channel].dsBuffer->Lock(0, actualBufferSize, &pBuffer1, &dwBytes1, &pBuffer2, &dwBytes2, 0));
	
	if (loopCount > 1)
	{
		for (DWORD i = 0; i < loopCount; i++)
		{
			memcpy((BYTE*)pBuffer1 + (i * size), pDataBuffer + offsetBegin, size);
		}
	}
	else
	{
		memcpy(pBuffer1, pDataBuffer + offsetBegin, size);
	}
	
	if (pBuffer2 && dwBytes2 > 0)
	{
		memcpy(pBuffer2, pDataBuffer + offsetBegin, dwBytes2);
	}
	
	CHECK_HR(channelBuffer[channel].dsBuffer->Unlock(pBuffer1, dwBytes1, pBuffer2, dwBytes2));
	
	// Store audio data info
	channelBuffer[channel].pAudioData = pDataBuffer + (sample != 0xFFFF ? offsetBegin : 0);
	channelBuffer[channel].audioDataSize = size;
	channelBuffer[channel].playPosition = 0;
	
	SetChannelVolume(channel, level);
	
	LogBufferState(channel, "SetupChannel Complete");
}

void PlayChannel(BYTE channel)
{
	if (channel >= CHANNELS) { return; }
	if (channelBuffer[channel].dsBuffer == NULL) { return; }

	info("OpenSndVoyager::PlayChannel Playing channel %u", channel);

	// Reset buffer position to start
	CHECK_HR(channelBuffer[channel].dsBuffer->SetCurrentPosition(0));
	
	// Play buffer with looping if channel.loop is set
	DWORD flags = channelBuffer[channel].loop ? DSBPLAY_LOOPING : 0;
	CHECK_HR(channelBuffer[channel].dsBuffer->Play(0, 0, flags));
	channelBuffer[channel].playing = TRUE;
}

void StopChannel(BYTE channel)
{
	if (channel >= CHANNELS) { return; }

	info("OpenSndVoyager::StopChannel Stopping channel %u", channel);
	if (channelBuffer[channel].playing && channelBuffer[channel].dsBuffer != NULL)
	{
		CHECK_HR(channelBuffer[channel].dsBuffer->Stop());
		channelBuffer[channel].playing = FALSE;
	}

	ResetChannel(channel);
}

void StopAllChannels()
{
	info("OpenSndVoyager::StopAllChannels");

	for (BYTE i = 0; i < CHANNELS; i++)
	{
		StopChannel(i);
	}
}

void ResetChannel(BYTE channel)
{
	info("ResetChannel %u", channel);

	if (channel >= CHANNELS) { return; }

	// Release DirectSound buffer
	if (channelBuffer[channel].dsBuffer != NULL)
	{
		channelBuffer[channel].dsBuffer->Release();
		channelBuffer[channel].dsBuffer = NULL;
	}

	// Reset rest
	channelBuffer[channel].dsFormat = { 0 };
	channelBuffer[channel].pAudioData = NULL;
	channelBuffer[channel].audioDataSize = 0;
	channelBuffer[channel].playPosition = 0;
	channelBuffer[channel].playing = FALSE;
	channelBuffer[channel].loop = FALSE;
	channelBuffer[channel].qSamples.clear();
	channelBuffer[channel].qIndex = -1;
	
	// Event-driven parsing state
	channelBuffer[channel].cursor = 0;
	channelBuffer[channel].soundCode = 0;
	channelBuffer[channel].pSampleHeader = NULL;
	channelBuffer[channel].currentLevel = 0xFF;
	channelBuffer[channel].currentSpeed = 0x40;
	channelBuffer[channel].currentSample = 0;
	channelBuffer[channel].isActive = FALSE;
	channelBuffer[channel].needsParse = FALSE;
}

void AddSampleToChannel(BYTE channel, WORD sample, BYTE speed, BYTE level)
{
	info("AddSampleToChannel channel: %u sample: %u speed: %u level: %u", channel, sample, speed, level);

	if (channel >= CHANNELS) { return; }

	channelBuffer[channel].qSamples.emplace_back(sample, speed, level, 0);
}

// === Oscillator Parameter Functions (like original bsmt_set_* functions) ===

// Set sample start/end/loop pointers (like original bsmt_set_patch)
void SetPatch(BYTE channel, WORD sampleNum)
{
	if (channel >= CHANNELS || sampleNum == 0 || sampleNum > 676) return;
	
	ChannelOscillator& osc = channelOscillator[channel];
	
	DWORD* pHeader = (DWORD*)blob + (sampleNum * 4);
	
	DWORD base = pHeader[3] * 0x10000;
	osc.sampleStart = base + pHeader[0];
	osc.sampleEnd = base + pHeader[1];
	osc.sampleLoop = base + pHeader[2];
	osc.samplePos = osc.sampleStart << 8; // Initialize position to start (24.8 fixed point)
	
	LOG_OPCODE("  SetPatch ch%d: sample=%d start=0x%X end=0x%X loop=0x%X",
		channel, sampleNum, osc.sampleStart, osc.sampleEnd, osc.sampleLoop);
}

// Set frequency/speed (like original bsmt_set_si_24_8)
void SetFrequency(BYTE channel, BYTE speed)
{
	if (channel >= CHANNELS) return;
	
	ChannelOscillator& osc = channelOscillator[channel];
	
	// Store speed for music detection in mixer
	osc.lastSpeed = speed;
	
	// Calculate sample increment in 24.8 fixed point
	DWORD sampleRate;
	
	// Music channels (20-30) use different pitch calculation
	if (channel >= 20 && channel <= 30)
	{
		// For music: use speed value more directly (linear scaling)
		// Speed 0x40 (64) = base, higher speeds = proportionally higher pitch
		// Formula: increment = speed * 2 for proper musical intervals
		osc.sampleIncrement = ((DWORD)speed) << 1;
		LOG_OPCODE("  SetFrequency ch%d: speed=0x%02X → inc=0x%X (music direct)",
			channel, speed, osc.sampleIncrement);
	}
	else
	{
		// For voices and sound effects: use sample rate conversion
		sampleRate = SpeedToSampleRate(speed);
		osc.sampleIncrement = (sampleRate << 8) / SAMPLE_RATE;
		LOG_OPCODE("  SetFrequency ch%d: speed=0x%02X → %d Hz → inc=0x%X",
			channel, speed, sampleRate, osc.sampleIncrement);
	}
}

// Set volume (0-1023 like original)
void SetVolume(BYTE channel, BYTE level)
{
	if (channel >= CHANNELS) return;
	
	// Convert level (0-255) to volume (0-1023)
	// Map proportionally: level * 1023 / 255 ≈ level * 4
	WORD rawVolume = ((WORD)level << 2); // level * 4
	
	// Clamp to max
	if (rawVolume > 1023)
		rawVolume = 1023;
	
	channelOscillator[channel].volume = rawVolume;
}

// Start oscillator (like original GateUp)
void GateUp(BYTE channel)
{
	if (channel >= CHANNELS) return;
	
	ChannelParseState& state = channelState[channel];
	
	// Set patch from current state
	if (state.currentPatch > 0 && state.currentPatch <= 676)
	{
		SetPatch(channel, state.currentPatch);
		SetFrequency(channel, state.currentSpeed);
		SetVolume(channel, state.currentLevel);
		
		channelOscillator[channel].active = TRUE;
		
		LOG_OPCODE("  GateUp ch%d: patch=%d speed=0x%02X level=%d",
			channel, state.currentPatch, state.currentSpeed, state.currentLevel);
	}
}

// Stop oscillator
void KillOscillator(BYTE channel)
{
	if (channel >= CHANNELS) return;
	
	channelOscillator[channel].active = FALSE;
	LOG_OPCODE("  KillOscillator ch%d", channel);
}

// === NEW Opcode Parsing (streaming architecture) ===

// Parse opcodes and update channel parameters (like original parse_snd)
// Does NOT trigger playback - oscillator continues streaming
void ParseAndUpdateChannel(BYTE channel)
{
	if (channel >= CHANNELS) return;
	
	ChannelParseState& state = channelState[channel];
	if (!state.isActive || !state.pOpcodeData) return;
	
	BYTE* pData = state.pOpcodeData;
	WORD cursor = state.cursor;
	BOOL parsing = TRUE;
	WORD opcodeCount = 0;
	
	info("[PARSE] === ParseAndUpdateChannel: ch%d cursor=0x%04X active=%d soundCode=%d ===", 
		channel, cursor, state.isActive, state.soundCode);
	LOG_OPCODE("=== ParseAndUpdateChannel: ch%d cursor=0x%04X ===", channel, cursor);
	
	while (parsing && opcodeCount < 1000)
	{
		BYTE opcode = pData[cursor];
		opcodeCount++;
		
		// Check if it's a note (not an opcode)
		if ((opcode & 0x80) == 0)
		{
			// This is a speed/note byte, followed by duration
			state.currentSpeed = opcode;
			BYTE duration = pData[cursor + 1];
			state.durationTicks = duration;      // Set target duration (ccb+8)
			state.durationCounter = 0;           // Reset counter (ccb+12)
			
			LOG_OPCODE("  [Note] speed=0x%02X duration=%d patch=%d level=%d tempo=%d",
				state.currentSpeed, duration, state.currentPatch, state.currentLevel, state.localTempo);
			
			cursor += 2;
			state.cursor = cursor;
			
			// Activate oscillator with current parameters
			GateUp(channel);
			
			return; // Done parsing for now
		}
		
		// Process opcode
		LOG_OPCODE("  [@0x%04X] Opcode=0x%02X", cursor, opcode);
		
		switch (opcode)
		{
		case OC_REST:
			LOG_OPCODE("    REST: duration=%d", pData[cursor + 1]);
			state.durationTicks = pData[cursor + 1];
			state.durationCounter = 0;
			KillOscillator(channel); // Stop sound during rest
			cursor += 2;
			state.cursor = cursor;
			return; // Done
			
		case OC_SETATTEN:
			state.currentLevel = pData[cursor + 1];
			LOG_OPCODE("    SETATTEN: level=%d", state.currentLevel);
			SetVolume(channel, state.currentLevel); // Update volume immediately
			cursor += 2;
			break;
			
		case OC_SETPATCH:
		{
			WORD patchSample = (pData[cursor + 1] << 8) | pData[cursor + 2];
			
			if (patchSample > 676)
			{
				LOG_OPCODE("    SETPATCH: Invalid sample %d", patchSample);
			}
			else
			{
				state.currentPatch = patchSample;
				LOG_OPCODE("    SETPATCH: sample=%d", state.currentPatch);
			}
			
			// Original logic: Always consumes 3 bytes (Opcode + WORD)
			cursor += 3;
			break;
		}
		
		case OC_STRING_BRANCH:
		{
			WORD target = (pData[cursor + 1] << 8) | pData[cursor + 2];
			LOG_OPCODE("    STRING_BRANCH: → 0x%04X (looping music)", target);
			cursor = target;
			// Don't return - continue parsing from the new location
			break;
		}
		
		case OC_STAY:
		{
			// STAY sets duration and STOPS parsing (like original Stay() returning 0)
			// The sequencer will call ParseAndUpdateChannel again when duration expires
			BYTE stayDuration = pData[cursor + 1];
			LOG_OPCODE("    STAY: duration=%d ticks", stayDuration);
			
			// Set duration
			state.durationTicks = stayDuration;
			state.durationCounter = 0;
			cursor += 2;
			state.cursor = cursor;
			
			// If patch is set but oscillator not active, start it now
			if (state.currentPatch > 0 && state.currentPatch <= 676 && !channelOscillator[channel].active)
			{
				LOG_OPCODE("    STAY: Starting oscillator");
				GateUp(channel);
			}
			
			// ALWAYS return after STAY (stops parsing until duration expires)
			return;
		}
		
		case OC_START_VOICE:
		{
			BYTE voiceNum = pData[cursor + 1];
			WORD soundCode = voiceNum + 512;
			LOG_OPCODE("    START_VOICE: voice=%d → code=%d", voiceNum, soundCode);
			sndPlay(soundCode);
			cursor += 2;
			break;
		}
		
		case OC_SYSKILLBSMT:
		{
			BYTE targetChannel = pData[cursor + 1];
			if (targetChannel != channel && targetChannel < CHANNELS)
			{
				LOG_OPCODE("    SYSKILLBSMT: kill ch%d", targetChannel);
				KillOscillator(targetChannel);
				channelState[targetChannel].isActive = FALSE;
			}
			cursor += 2;
			break;
		}
		
		case OC_SETMUSICTEMPO:
			LOG_OPCODE("    SETMUSICTEMPO: value=%d (global, ignored)", pData[cursor + 1]);
			cursor += 2;
			break;
			
		case OC_CHANNELATTEN:
		{
			BYTE attenValue = pData[cursor + 1];
		// Attenuation reduces volume: full volume (255) minus attenuation
		state.currentLevel = 255 - attenValue;
		SetVolume(channel, state.currentLevel);
		LOG_OPCODE("    CHANNELATTEN: atten=%d -> level=%d", attenValue, state.currentLevel);
		cursor += 2;
		break;
	}
	
	case OC_GETMUSICTEMPO:
			LOG_OPCODE("    GETMUSICTEMPO: (read tempo, ignored)");
			cursor += 1;
			break;
			
		case OC_SETENVELOPE:
			LOG_OPCODE("    SETENVELOPE: value=%d (ignored)", pData[cursor + 1]);
			cursor += 2;
			break;
			
		case OC_SETOCTOFF:
			LOG_OPCODE("    SETOCTOFF: value=%d (ignored)", pData[cursor + 1]);
			cursor += 2;
			break;
			
		case OC_SETNOTEOFF:
			LOG_OPCODE("    SETNOTEOFF: value=%d (ignored)", pData[cursor + 1]);
			cursor += 2;
			break;
		
		case OC_SEQSTARTVOICE:
		{
			// byte v1, word v6, word v2. Total 5 args + 1 opcode = 6 bytes.
			// v6 is "ix of subroutine" in ROM.
			WORD subRoutineIx = (pData[cursor + 2] << 8) | pData[cursor + 3];
			
			// Note: We are ignoring v1 (count) and v2 (global var) for now as we don't fully emulate the global var system
			// But we MUST consume the bytes correctly.
			
			// Calculate the sound code from the subroutine index
			// In stvsnd.c: startAvoice((unsigned __int8)audio_program_rom[v6] + 512);
			if (subRoutineIx < 0x8000) // Sanity check
			{
				WORD soundCode = *(pSampleHeaders + subRoutineIx) + 0x200;
				LOG_OPCODE("    SEQSTARTVOICE: ix=%d → code=%d", subRoutineIx, soundCode);
				sndPlay(soundCode);
			}
			
			cursor += 6;
			break;
		}
		
		case OC_EOS:
			LOG_OPCODE("    EOS: End of sequence");
			KillOscillator(channel);
			state.isActive = FALSE;
			return;
			
		case OC_SET_LUP_CNTL:
			state.loopControl = pData[cursor + 1];
			LOG_OPCODE("    SET_LUP_CNTL: count=%d", state.loopControl);
			cursor += 2;
			break;

		case OC_TEST_LUPC_JMP:
		{
			// Loop control logic matching stvsnd.c test_lupc_jmp
			BYTE currentCount = state.loopControl;
			
			// Decrement in memory
			if (state.loopControl > 0) state.loopControl--;
			
			if (currentCount == 1)
			{
				// Loop finished (was 1, now 0). Skip the jump address.
				LOG_OPCODE("    TEST_LUPC_JMP: Loop finished. Skipping jump.");
				cursor += 3;
			}
			else
			{
				// Loop continues. Jump to address.
				WORD target = (pData[cursor + 1] << 8) | pData[cursor + 2];
				LOG_OPCODE("    TEST_LUPC_JMP: Looping (rem=%d) -> 0x%04X", state.loopControl, target);
				cursor = target;
			}
			break;
		}

		// 3-Byte Opcodes (Opcode + Word)
		case OC_STRING_JSR:
		case OC_SETPITCHOFFSET:
		case OC_SYSATTENDELTA: // 1+1+1=3
			cursor += 3;
			break;

		// 4-byte opcodes
		// None currently identified as exactly 4 bytes
		
		// Ignore these opcodes (1-byte: opcode only)
		case OC_TOGGLESYNC:
		case OC_NEWNOTE:
		case OC_SETPAN:
		case OC_GETNOTEOFF:
		case OC_SETSTEPMODE:
		case OC_SETPDELT:
		case OC_CLRPDELT:
		case OC_SETFRAC:
		case OC_SETTEMPOMODE:
		case OC_SETVIBRATO:
		case OC_DOAUTONOTRIG:
		case OC_MASTEROPLATTEN:
		case OC_SETGLOBALVAR:
		case OC_GLOBALKEYSWITCH:
		case OC_SETGATEPER:
		case OC_STRING_RTS:
		case OC_DIAGNOSTIC:
			cursor += 1;
			break;
		
		// Ignore these opcodes (2-byte: opcode + param)
		case OC_SETPRIORITY:
		// OC_SET_LUP_CNTL (0x80) handled above
		// OC_TEST_LUPC_JMP (0x81) handled above
		case OC_SETGATETHRESHOLD:
		case OC_SIGNEDATTEN: // 0xB0 is 2 bytes (grab_byte)
			cursor += 2;
			break;
			
		default:
			// Unknown opcode - log and skip
			LOG_OPCODE("    UNKNOWN OPCODE: 0x%02X", opcode);
			cursor += 1;
			break;
		}
		
		state.cursor = cursor;
	}
	
	if (opcodeCount >= 1000)
	{
		LOG_LOOP("!!! Ch%d: Parse overflow !!!", channel);
		state.isActive = FALSE;
		KillOscillator(channel);
	}
}

// === OLD Opcode Parsing (will be removed) ===

// Parse and play one note (original stvsnd architecture)
// This is called when a note finishes playing - we parse opcodes until we hit the next note
void ParseAndPlayNextNote(BYTE channel)
{
	if (channel >= CHANNELS || !channelBuffer[channel].isActive) { return; }
	
	BYTE* pData = channelBuffer[channel].pSampleHeader;
	WORD cursor = channelBuffer[channel].cursor;
	BYTE level = channelBuffer[channel].currentLevel;
	BYTE speed = channelBuffer[channel].currentSpeed;
	WORD sample = channelBuffer[channel].currentSample;
	WORD opcodeCount = 0;
	BOOL parsing = TRUE;
	
	LOG_OPCODE("=== ParseAndPlayNextNote: channel %u, cursor 0x%04X ===", channel, cursor);
	
	while (parsing && opcodeCount < 1000) // Safety limit
	{
		BYTE opcode = pData[cursor];
		opcodeCount++;
		
		// Check if it's a note (not an opcode)
		if ((opcode & 0x80) == 0)
		{
			// This is a note/speed byte, followed by duration
			speed = opcode;
			BYTE duration = pData[cursor + 1];

			// Check if we have a pending REST from REST opcode
			WORD actualSample;
			if (channelBuffer[channel].pendingRestDuration > 0)
			{
				// Play REST instead of currentSample
				actualSample = 0xFFFF;
				level = channelBuffer[channel].pendingRestDuration;
				channelBuffer[channel].pendingRestDuration = 0; // Clear flag
			}
			else
			{
				// Use currentSample from channel state
				actualSample = channelBuffer[channel].currentSample;
			}
			
			LOG_OPCODE("  [Note] speed: 0x%02X (%u Hz), duration: %u, sample: %u, level: %u",
				speed, SpeedToSampleRate(speed), duration, actualSample, level);
			
			cursor += 2;
			channelBuffer[channel].cursor = cursor;
			channelBuffer[channel].currentSpeed = speed;
			channelBuffer[channel].lastPlayedSample = actualSample; // Remember what we're playing for STAY opcodes
			
			// Play this note
			if (actualSample != 0xFFFF && actualSample < 677)
			{
				// Reset queue and play this one note with duration (use stay parameter for looping)
				channelBuffer[channel].qSamples.clear();
				channelBuffer[channel].qIndex = -1;
				channelBuffer[channel].qSamples.emplace_back(actualSample, speed, level, duration);
				
				SetupChannel(channel);
				PlayChannel(channel);
			}
			else if (actualSample == 0xFFFF)
			{
				// REST - play silence
				LOG_OPCODE("  REST detected, playing silence for %u", duration);
				channelBuffer[channel].qSamples.clear();
				channelBuffer[channel].qIndex = -1;
				channelBuffer[channel].qSamples.emplace_back(0xFFFF, speed, duration, 0);
				
				SetupChannel(channel);
				PlayChannel(channel);
			}
			
			return; // Done - we've queued one note
		}
		
		// Process opcode
		LOG_OPCODE("  [@0x%04X] Opcode: 0x%02X", cursor, opcode);
		
		switch (opcode)
		{
		case OC_REST:
			LOG_OPCODE("    REST: delay=%u", pData[cursor + 1]);
			channelBuffer[channel].currentSample = 0xFFFF;
			level = pData[cursor + 1];
			cursor += 2;
			break;
			
		case OC_SETATTEN:
			level = pData[cursor + 1];
			LOG_OPCODE("    SETATTEN: level=%u", level);
			channelBuffer[channel].currentLevel = level;
			cursor += 2;
			break;
			
		case OC_SETPATCH:
	{
		WORD patchSample = pData[cursor + 1] << 8 | pData[cursor + 2];
		if (patchSample > 676)
		{
			LOG_OPCODE("    SETPATCH: Invalid sample %u, skipping", patchSample);
			cursor += 1;
			break;
		}

		// Some are shorter
		if (pData[cursor + 3] >= OC_SET_LUP_CNTL && pData[cursor + 3] <= OC_DIAGNOSTIC)
		{
			LOG_OPCODE("    SETPATCH: sample=%u (short form)", patchSample);
			cursor += 3;
		}
		else
		{
			speed = pData[cursor + 3];
			LOG_OPCODE("    SETPATCH: sample=%u, speed=0x%02X", patchSample, speed);
			cursor += 5;
		}

		channelBuffer[channel].currentSample = patchSample;
		channelBuffer[channel].currentSpeed = speed;
		break;
	}
		
	case OC_STRING_BRANCH:
{
	WORD target = pData[cursor + 1] << 8 | pData[cursor + 2];
	LOG_OPCODE("    STRING_BRANCH: jumping to 0x%04X", target);
	LOG_LOOP("*** STRING_BRANCH: Channel %u jumping from 0x%04X to 0x%04X ***", channel, cursor, target);
	cursor = target; // Jump immediately like original
	break;
}
			
		case OC_SETPITCHOFFSET:
			cursor += 3;
			break;
			
		case OC_STAY:
		{
			// STAY extends the duration of the currently playing note
			// We need to add more sample data (loop the current sample)
			BYTE stayValue = pData[cursor + 1];
			LOG_OPCODE("    STAY: value=%u", stayValue);
			cursor += 2;
			
			// If we have a current sample playing, extend it by queueing more of the same sample
			// Use lastPlayedSample since 'sample' may have been cleared to 0
			WORD lastSample = channelBuffer[channel].lastPlayedSample;
			if (lastSample != 0xFFFF && lastSample < 677 && channelBuffer[channel].playing)
			{
				// Add the same sample with the stay duration
				channelBuffer[channel].qSamples.emplace_back(lastSample, speed, level, stayValue);
				LOG_OPCODE("    STAY: Queued sample %u for %u more ticks", lastSample, stayValue);
			}
			else if (lastSample == 0xFFFF)
			{
				// REST + STAY = longer rest
				channelBuffer[channel].qSamples.emplace_back(0xFFFF, speed, stayValue, 0);
				LOG_OPCODE("    STAY: Queued REST for %u ticks", stayValue);
			}
			break;
		}
			
		case OC_SETENVELOPE:
			LOG_OPCODE("    SETENVELOPE: (ignored)");
			cursor += 2;
			break;
			
		case OC_SETPRIORITY:
			cursor += 2;
			break;
			
		case OC_SYSKILLBSMT:
			if (pData[cursor + 1] != channel)
				StopChannel(pData[cursor + 1]);
			cursor += 2;
			break;
			
		case OC_SEQSTARTVOICE:
		{
			WORD temp = pData[cursor + 2] << 8 | pData[cursor + 3];
			WORD realOpcode = *(pSampleHeaders + temp) + 0x200;
			LOG_OPCODE("    SEQSTARTVOICE: realCode=%u", realOpcode);
			sndPlay(realOpcode);
			cursor += 4;
			break;
		}
		
		case OC_LOCALTEMPO:
		case OC_SET_LUP_CNTL:
		case OC_TEST_LUPC_JMP:
		case OC_SETOCTOFF:
		case OC_SETNOTEOFF:
		case OC_RANDOMDURATION:
		case OC_CHANNELATTEN:
		case OC_SETMUSICTEMPO:
		case OC_GETMUSICTEMPO:
			cursor += 2;
			break;
		case OC_EOS:
			LOG_OPCODE("    EOS: End of sequence");
			
			// If we have a sample set but no note played yet, play it now
			if (channelBuffer[channel].currentSample != 0 && channelBuffer[channel].currentSample < 677)
			{
				LOG_OPCODE("  [Auto-Play] Playing sample %u at EOS with speed 0x%02X, level %u",
					channelBuffer[channel].currentSample, 
					channelBuffer[channel].currentSpeed,
					channelBuffer[channel].currentLevel);
				
				// Play the sample
				channelBuffer[channel].qSamples.clear();
				channelBuffer[channel].qIndex = -1;
				channelBuffer[channel].qSamples.emplace_back(
					channelBuffer[channel].currentSample,
					channelBuffer[channel].currentSpeed,
					channelBuffer[channel].currentLevel,
					0);
				
				channelBuffer[channel].currentSample = 0; // Clear it
				
				SetupChannel(channel);
				PlayChannel(channel);
			}
			
			channelBuffer[channel].isActive = FALSE;
			channelBuffer[channel].playing = FALSE;
			return;
			
		default:
			if (opcode >= OC_SET_LUP_CNTL && opcode <= OC_DIAGNOSTIC)
			{
				LOG_OPCODE("    UNKNOWN_OPCODE: 0x%02X", opcode);
				cursor += 1;
			}
			else
			{
				// Treat as speed byte
				LOG_OPCODE("    SPEED_BYTE: 0x%02X", opcode);
				cursor += 2;
			}
			break;
		}
		
		channelBuffer[channel].cursor = cursor;
	}
	
	if (opcodeCount >= 1000)
	{
		LOG_LOOP("!!! Channel %u: Safety limit reached, stopping parse !!!", channel);
		channelBuffer[channel].isActive = FALSE;
	}
}

extern "C" __declspec(dllexport) void sndPlay(INT code)
{
	g_frameCounter++;
	
	if (!bufferReady) { return; }
	
	// Skip these for some reason
	if (code == -0x2a474b3 || code == 0xC2)
	{
		return;
	}

	// Master volume control
	if (code >= 1000 && code <= 1010)
	{
		SetMasterVolume(code - 1000);
		return;
	}

	EnterCriticalSection(&g_csStream);

	// Kill all
	if (code == 0)
	{
		for (BYTE ch = 0; ch < CHANNELS; ch++)
		{
			channelState[ch].isActive = FALSE;
			KillOscillator(ch);
		}
		LeaveCriticalSection(&g_csStream);
		return;
	}

	// Mute music?
	if (MUTE_MUSIC && (code == 4 || code == 5 || code == 6 || code == 7 || code == 20 || code == 22 || code == 26 || code == 36 || code == 188 || code == 189 || code == 196 || code == 268 || code == 301))
	{
		info("sndPlay: MUSIC CODE %d MUTED", code);
		LeaveCriticalSection(&g_csStream);
		return;
	}
	
	// Log music codes
	if (code == 4 || code == 5 || code == 6 || code == 7 || code == 20 || code == 22 || code == 26 || code == 36 || code == 188 || code == 189 || code == 196 || code == 268 || code == 301)
	{
		info(">>> MUSIC CODE %d STARTED <<<", code);
	}

	// Get sound header from ROM
	WORD hOffset = pSampleHeaders[code * 2] << 8 | pSampleHeaders[code * 2 + 1];
	BYTE* pSampleHeader = pSampleHeaders + hOffset;

	BYTE channel = pSampleHeader[0];
	BYTE priority = pSampleHeader[1];
	BYTE initialLevel = pSampleHeader[2];

	// Nothing
	if (pSampleHeader[3] == OC_EOS || channel == 0x00)
	{
		LeaveCriticalSection(&g_csStream);
		return;
	}

	// Initialize channel for streaming (like original startAvoice)
	
	// Kill oscillator first (like original startAvoice line 707)
	KillOscillator(channel);
	
	// Setup channel parse state
	channelState[channel].cursor = 3;  // Start after channel/priority/level bytes
	channelState[channel].soundCode = code;
	channelState[channel].pOpcodeData = pSampleHeader;
	channelState[channel].currentPatch = 0;
	channelState[channel].currentLevel = initialLevel;
	channelState[channel].currentSpeed = 0x40;  // Default speed
	channelState[channel].pendingRestDuration = 0;
	channelState[channel].durationTicks = 0;
	channelState[channel].durationCounter = 0;
	channelState[channel].localTempo = 255;  // Reset to default
	channelState[channel].tempoFractionAccumulator = 0;
	channelState[channel].tickCounter = 0;
	channelState[channel].isActive = TRUE;
	
	// Start parsing opcodes - streaming thread will handle audio generation
	ParseAndUpdateChannel(channel);
	
	LeaveCriticalSection(&g_csStream);
}