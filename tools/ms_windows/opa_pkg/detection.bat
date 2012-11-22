REM After is old detection stuff, to keep and migrate in post_install.cmd

REM Search locations
REM get the location of VC from the environment
REM !!! NO SPACE BEFORE & !!!
REM if defined VS100COMNTOOLS set VSLOCATION=%VS100COMNTOOLS%\..\..& goto :vcvarsallOK
REM if defined VS90COMNTOOLS set VSLOCATION=%VS90COMNTOOLS%\..\..& goto :vcvarsallOK
REM if defined VS80COMNTOOLS set VSLOCATION=%VS80COMNTOOLS%\..\..& goto :vcvarsallOK
REM if defined VS70COMNTOOLS set VSLOCATION=%VS70COMNTOOLS%\..\..& goto :vcvarsallOK
REM goto :vcvarsallKO
REM if VS is found call the VC var setter
REM :vcvarsallOK
REM call "%VSLOCATION%\VC\vcvarsall.bat" > NUL
REM :vcvarsallKO


REM get the location of flexdll
REM if exist "%OPABASEDIROCAML%\flexdll"      set FLEXDLLLOCATION=%OPABASEDIROCAML%\flexdll& goto :flexdllOK
REM if exist "c:\%FLEXDLLflexdll"             set FLEXDLLLOCATION=c:\flexdll& goto :flexdllOK
REM if exist "c:\Program Files\flexdll"       set FLEXDLLLOCATION=c:\Program Files\flexdll& goto :flexdllOK
REM if exist "c:\Program Files (x86)\flexdll" set FLEXDLLLOCATION=c:\Program Files (x86)\flexdll& goto :flexdllOK
REM goto :flexdllKO
REM :flexdllOK
REM set PATH=%PATH%;%FLEXDLLLOCATION%
REM :flexdllKO

REM set PATH=""
REM set INCLUDE=""
REM set CLPATH=C:\Program Files\Microsoft Visual Studio 10.0\VC\bin
REM set VSLIBS=C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE
REM set FLEXDLL=C:\flexdll
REM set MTPATH=C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin
REM set PATH="%OPABASEDIR%/bin";%CLPATH%;%VSLIBS%;%FLEXDLLOCATION%;%MTPATH%;C:\Windows\system32


REM Checking presence of dependencies
REM cl.exe exists
REM chdir %ProgramFiles%
REM attrib /s cl.exe | find "soft" > NUL
REM if not %errorlevel% == 0 goto :noCl
REM cd %WD%

REM set DEST=NUL
REM %TMP\\OPACHECK

REM cl.exe is callable
REM cl.exe > %DEST% 2>&1
REM if %errorlevel% == 9009 goto :noCl
REM mt.exe /BAD > %DEST% 2>&1
REM if %errorlevel% == 9009 goto :noCl

REM gcc.exe is callable
REM gcc.exe > %DEST% 2>&1
REM if %errorlevel% == 9009 goto :noGcc


REM flexlink is callable
REM flexlink.exe > %DEST% 2>&1
REM if %errorlevel% == 9009 goto :noFlexlink


REM if %ABORT% == YES exit /B 1


REM set OLDOCAMLLIB=%OCAMLLIB%
REM set OLDBASEDIR=%BASEDIR%
REM set MLSTATELIBS=%OPABASEDIR%
REM set OCAMLOPT=ocamlopt.opt.exe
REM set OCAMLC=ocamlc.opt.exe
REM set PATH="%OPABASEDIROCAML%\bin";"%OPABASEDIR%\bin";%PATH%
REM set OCAMLLIB=%OPABASEDIROCAML%\lib


REM for /F "tokens=* delims=" %%A in ('where gcc') do set MINGWGCC=%%A
REM for %%F in (%MINGWGCC%) do set MINGWDIR=%%~dpF
REM set MINGWLIB=%MINGWDIR%..\x86_64-w64-mingw32\lib
REM set FLEXLINKFLAGS=-nocygpath -L %MINGWLIB%

REM "%OPABASEDIR%\bin\runopa.exe" --no-color %*

REM Cleaning what have been set

set PATH=%OLDPATH%
set OCAMLLIB=%OLDOCAMLLIB%

goto :end

:noFlexlink
echo ERROR: You must install flexlink tool (see http://alain.frisch.fr/flexdll/) or correct your PATH variable (e.g. set PATH=c:\some path\flexdll)
set ABORT=YES
pause
exit /B 1

:noCl
echo ERROR: You must install microsoft compilation tools (see http://msdn.microsoft.com/fr-fr/express/aa975050.aspx) or correct your PATH variable
set ABORT=YES
pause
exit /B 1

:noGcc
echo ERROR: You must install GCC for windows (see http://mingw-w64.sourceforge.net/) or correct your PATH variable
set ABORT=YES
pause
exit /B 1
 

:end