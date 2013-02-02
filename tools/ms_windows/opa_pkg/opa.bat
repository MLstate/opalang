REM
REM This scripts sets all needed env var before calling the compiler
REM It assumes the package include everything (flexdll, mingw, libz, libcrypto, bash_nt, ...)
REM
	@echo off

REM Saving old values for external var
	set OLDPATH=%PATH%
	REM OCAML set OLDOCAMLLIB=%OCAMLLIB%

REM BEGIN ENV VAR NEEDED BY THE COMPILER
	REM COMMON PATHS
	set OPABASEDIRLIB=%OPABASEDIR%\lib
	set OPABASEDIRMINGW=%OPABASEDIRLIB%\mingw
	REM OCAML set OPABASEDIROCAML=%OPABASEDIRLIB%\ocaml
	REM FINAL NEEDED VARS
	set MLSTATELIBS=%OPABASEDIR%
	REM OCAML set OCAMLLIB=%OPABASEDIROCAML%\lib
	REM OCAML set FLEXLINKFLAGS=-nocygpath -L %OPABASEDIRMINGW%\x86_64-w64-mingw32\lib -L %OPABASEDIRMINGW%\lib
	REM BE AWARE YOU CANNOT DOUBLE QUOTE %OPABASEDIRMINGW%\bin OTHERWISE GCC IS COMPLAINING ABOUT A MISSING LIB
	set PATH=%OPABASEDIR%\bin;%OPABASEDIRMINGW%\bin
	REM OCAML set PATH=%OPABASEDIR%\bin;%OPABASEDIRLIB%\flexdll;%OPABASEDIROCAML%\bin\;%OPABASEDIRMINGW%\bin

REM CALLING THE COMPILER OR OTHER TOOLS
	if "%1"=="create" (
		if "%2"=="" (
			"%OPABASEDIR%\bin\opa-create.exe"
		) else (
			"%OPABASEDIR%\bin\opa-create.exe" --name %2
		)
	) else (
		"%OPABASEDIR%\lib\opa\bin\opa-bin.exe" --no-color %*
	)

REM Restoring old values
	set PATH=%OLDPATH%
	REM OCAML set OCAMLLIB=%OLDOCAMLLIB%