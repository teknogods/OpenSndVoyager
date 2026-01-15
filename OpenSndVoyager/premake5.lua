project "OpenSndVoyager"
	targetname "OpenSndVoyager"
	language "C++"
	kind "SharedLib"
	removeplatforms { "x64" }

	files
	{
		"src/**.cpp", "src/**.h",
		"deps/cpp/**.cpp", "deps/inc/**.h",
		"src/OpenSndVoyager.aps", "src/OpenSndVoyager.rc"
	}

	includedirs { "src" }

	defines { 
		"VOYAGER_VER_MAJOR=1", 
		"VOYAGER_VER_MINOR=0", 
		"VOYAGER_VER_BUILD=0", 
		"VOYAGER_VER_REV=0",
		"VOYAGER_VER_STR=\"1.0.0.0\""
	}

postbuildcommands {
  "if not exist $(TargetDir)output mkdir $(TargetDir)output",
  "{COPY} $(TargetDir)OpenSndVoyager.dll $(TargetDir)output/"
}