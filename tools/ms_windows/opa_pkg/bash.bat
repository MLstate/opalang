@echo off
type %OPABASEDIR%\bin\path.sh %1 > %1.sh  2>NUL
%OPABASEDIR%\lib\bash_nt\bash.exe %1.sh
REM Something better should be possible with -c or rc file
REM rename %1 *.bat > NUL 2>&1
REM %1.bat > NUL 2>&1