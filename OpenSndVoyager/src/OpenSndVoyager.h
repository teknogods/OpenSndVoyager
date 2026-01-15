/*
* Created by Harm for TeknoParrot
* This file is part of the OpenParrot project - https://teknoparrot.com / https://github.com/teknogods
*
* See LICENSE and MENTIONS in the root of the source tree for information
* regarding licensing.
*/

#pragma once

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <dsound.h>

#define CHECK_HR(exp) { HRESULT hr = exp; if (FAILED(hr)) { info("failed %s: %08x\n", #exp, hr); abort(); } }

#define CHANNELS 0x21
#define SAMPLES 266
#define MUTE_MUSIC FALSE

// Opcodes (0x0804e120)
#define OC_SET_LUP_CNTL      0X80    // 0x8048E80 uint set_lup_cntl(int param_1)
#define OC_TEST_LUPC_JMP     0X81    // 0x8048E9C uint test_lupc_jmp(int param_1)
#define OC_START_VOICE       0X82    // 0x8048EF4 uint start_voice(int pVoice)
#define OC_SETGATETHRESHOLD  0X83    // 0x8048F18 uint setGateThreshold(int param_1)
#define OC_STRING_BRANCH     0X84    // 0x8048F34 uint string_branch(int param_1)
#define OC_STRING_JSR        0X85    // 0x8048FD8 uint string_jsr(int param_1)
#define OC_STRING_RTS        0X86    // 0x8049004 uint string_rts(uint * param_1)
#define OC_SETOCTOFF         0X87    // 0x8049020 uint setOctOff(int param_1)
#define OC_EOS               0X88    // 0x8048ED0 uint eos(uint * param_1)
#define OC_REST              0X89    // 0x804904C uint rest(int param_1)
#define OC_TOGGLESYNC        0X8A    // 0x8049078 uint togglesync(void)
#define OC_NEWNOTE           0X8B    // 0x804908C uint newnote(void)
#define OC_SETNOTEOFF        0X8C    // 0x80490A0 uint setNoteOff(int param_1)
#define OC_SETPAN            0X8D    // 0x80490BC uint SetPan(void)
#define OC_GETNOTEOFF        0X8E    // 0x80490D0 uint getNoteOff(void)
#define OC_SETSTEPMODE       0X8F    // 0x80490E4 uint SetStepMode(void)
#define OC_SETPATCH          0X90    // 0x80490F8 uint setpatch(int param_1)
#define OC_SETPDELT          0X91    // 0x8049114 uint setPDelt(void)
#define OC_CLRPDELT          0X92    // 0x8049128 uint clrPDelt(void)
#define OC_SETFRAC           0X93    // 0x804913C uint setFrac(void)
#define OC_SETTEMPOMODE      0X94    // 0x8049150 uint setTempoMode(void)
#define OC_SETVIBRATO        0X95    // 0x8049164 uint setVibrato(void)
#define OC_DOAUTONOTRIG      0X96    // 0x8049178 uint doAutoNoTrig(void)
#define OC_MASTEROPLATTEN    0X97    // 0x804918C uint masterOPLatten(void)
#define OC_SETAUTO           0X98    // 0x80491A0 uint setAuto(void)
#define OC_DOAUTO            0X99    // 0x80491B4 uint doAuto(void)
#define OC_CHECKKEY          0X9A    // 0x80491C8 uint checkKey(void)
#define OC_RNDVOICE          0X9B    // 0x80491DC uint rndVoice(void)
#define OC_INITSYNC          0X9C    // 0x80491F0 uint initsync(void)
#define OC_WAITSYNC          0X9D    // 0x8049204 uint waitsync(void)
#define OC_SETATTEN          0X9E    // 0x804926C uint setAtten(uint * param_1)
#define OC_SETKEY            0X9F    // 0x8049294 uint setKey(void)
#define OC_STAY              0XA0    // 0x80492A8 uint Stay(int param_1)
#define OC_RETRIGGER         0XA1    // 0x80492CC uint retrigger(void)
#define OC_RNDSTRJMP         0XA2    // 0x80492E0 uint RndStrJmp(void)
#define OC_GLOBALKEYSWITCH   0XA3    // 0x80492F4 uint globalKeySwitch(void)
#define OC_SETGATEPER        0XA4    // 0x8049308 uint setGatePer(void)
#define OC_SETPRIORITY       0XA5    // 0x804931C uint setPriority(int param_1)
#define OC_SETMUSICTEMPO     0XA6    // 0x8049338 uint setmusicTempo(int param_1)
#define OC_SETPITCHOFFSET    0XA7    // 0x8049354 uint SetPitchOffset(int param_1)
#define OC_DYNAMICPAN        0XA8    // 0x8049370 uint DynamicPan(void)
#define OC_LOCALTEMPO        0XA9    // 0x8049384 uint LocalTempo(int param_1)
#define OC_PUTTICKET         0XAA    // 0x80493B8 uint putTicket(void)
#define OC_CHANNELATTEN      0XAB    // 0x80493CC uint ChannelAtten(uint * param_1)
#define OC_SYSATTEN          0XAC    // 0x80493F4 uint sysAtten(void)
#define OC_SYSATTENDELTA     0XAD    // 0x804942C uint sysAttenDelta(int param_1)
#define OC_ATTRACT           0XAE    // 0x8049484 uint Attract(void)
#define OC_INCGLOBALVAR      0XAF    // 0x8049498 uint incGlobalVar(void)
#define OC_SIGNEDATTEN       0XB0    // 0x80494AC uint signedAtten(uint * param_1)
#define OC_SETGLOBALVAR      0XB1    // 0x80494E0 uint setGlobalVar(void)
#define OC_STRJSRINC         0XB2    // 0x80494F4 uint StrJsrInc(void)
#define OC_SYSKILLBSMT       0XB3    // 0x8049508 uint sysKillBSMT(int param_1)
#define OC_CONSTARTVOICE     0XB4    // 0x8049558 uint ConStartVoice(void)
#define OC_SETRNDFREQ        0XB5    // 0x804956C uint setRndFreq(void)
#define OC_STARTPOINTER      0XB6    // 0x8049580 uint StartPointer(void)
#define OC_RNDSTRJSR         0XB7    // 0x8049594 uint RndStrJsr(void)
#define OC_SYSKEY            0XB8    // 0x80495A8 uint sysKey(void)
#define OC_SYSKEYDELTA       0XB9    // 0x80495BC uint sysKeyDelta(void)
#define OC_GETMUSICTEMPO     0XBA    // 0x80495D8 uint getmusicTempo(int param_1)
#define OC_SNOP              0XBB    // 0x80495FC uint snop(void)
#define OC_RANDOMDURATION    0XBC    // 0x8049610 uint RandomDuration(void)
#define OC_TEMPODELTA        0XBD    // 0x8049624 uint tempodelta(void)
#define OC_GLOBALTEMPODELTA  0XBE    // 0x8049638 uint GlobalTempoDelta(void)
#define OC_UPDATEGLOBALTEMPO 0XBF    // 0x804964C uint UpdateGlobalTempo(void)
#define OC_SETMUSICLEVEL     0XC0    // 0x8049660 uint setMusicLevel(void)
#define OC_MUSICLEVELSTART   0XC1    // 0x8049674 uint MusicLevelStart(void)
#define OC_PLAYQ             0XC2    // 0x8049688 uint playQ(void)
#define OC_SYSKILLCONDUCT    0XC3    // 0x804969C uint sysKillConduct(void)
#define OC_SETPITCHBENDRANGE 0XC4    // 0x80496B0 uint SetPitchBendRange(void)
#define OC_SETENVELOPE       0XC5    // 0x80496C4 uint SetEnvelope(int param_1)
#define OC_MUSICKEYDELTA     0XC6    // 0x8049708 uint MusicKeyDelta(void)
#define OC_SETKEYCHASEMODE   0XC7    // 0x804971C uint setKeyChaseMode(void)
#define OC_SETSYSPRIORITY    0XC8    // 0x8049730 uint setSysPriority(void)
#define OC_SETVELOCITY       0XC9    // 0x8049744 uint setVelocity(void)
#define OC_GLOBALVARTOTEMPO  0XCA    // 0x8049758 uint GlobalVarToTempo(void)
#define OC_ANDORB            0XCB    // 0x804976C uint ANDORB(void)
#define OC_ORORB             0XCC    // 0x8049780 uint ORORB(void)
#define OC_SETPITCHBEND      0XCD    // 0x8049794 uint SetPitchBend(void)
#define OC_FLUSHQ            0XCE    // 0x80497A8 uint FlushQ(void)
#define OC_SETALGORITHM      0XCF    // 0x80497BC uint setAlgorithm(void)
#define OC_ANDORA            0XD0    // 0x80497D0 uint ANDORA(void)
#define OC_ORORA             0XD1    // 0x80497E4 uint ORORA(void)
#define OC_EOS_2             0XD2    // 0x8048ED0 uint eos(uint * param_1)
#define OC_EOS_3             0XD3    // 0x8048ED0 uint eos(uint * param_1)
#define OC_FORK              0XD4    // 0x80497F8 uint FORK(void)
#define OC_TEMPOTOGLOBALVAR  0XD5    // 0x804980C uint TempoToGlobalVar(void)
#define OC_SETMULTISAMPLE    0XD6    // 0x8049820 uint SetMultiSample(void)
#define OC_EOS_4             0XD7    // 0x8048ED0 uint eos(uint * param_1)
#define OC_RNDNOTE           0XD8    // 0x8049834 uint RndNote(void)
#define OC_INCCOINCOUNT      0XD9    // 0x804985C uint IncCoinCount(void)
#define OC_INCTICKETCOUNT    0XDA    // 0x8049848 uint IncTicketCount(void)
#define OC_SETWORDPOINTER    0XDB    // 0x8049870 uint SetWordPointer(void)
#define OC_INCMUSICLEVEL     0XDC    // 0x8049884 uint IncMusicLevel(void)
#define OC_DECMUSICLEVEL     0XDD    // 0x8049898 uint DecMusicLevel(void)
#define OC_SWITCHSOUNDTABLE  0XDE    // 0x80498AC uint SwitchSoundTable(void)
#define OC_IFTHEN            0XDF    // 0x80498C0 uint IfThen(void)
#define OC_SEQSTARTVOICE     0XE0    // 0x80498D4 uint SeqStartVoice(int param_1)
#define OC_DIAGNOSTIC        0XE1    // 0x804999C uint Diagnostic(void)

extern "C" {
__declspec(dllexport) void snd_init_new(DWORD level);
__declspec(dllexport) void snd_code_always(INT code);
__declspec(dllexport) void sndPlay(INT code);
__declspec(dllexport) void snd_get_channel_status();
__declspec(dllexport) void snd_analyze_code(INT code);
}

// Declarations
struct BUFFER;
struct qSample;
void ResetChannel(BYTE channel);
void PlayChannelNext(BYTE channel);
DWORD SpeedToSampleRate(BYTE speed);
void StopChannel(BYTE channel);