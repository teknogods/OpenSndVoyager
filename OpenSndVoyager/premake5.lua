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

postbuildcommands {
  "if not exist $(TargetDir)output mkdir $(TargetDir)output",
  "{COPY} $(TargetDir)OpenSndVoyager.dll $(TargetDir)output/"
}