@echo off
echo Hello! I am going to help you to install recommended software for Opa

REM This scriptis run just after the install on Windows

REM In case OPABASEDIR is not propagated yet, we use the first parameter if existing
	if NOT "%~1"=="" set OPABASEDIR=%~1

pause
REM Showing some message to the user
	echo Have a look at the README, if needed
	start notepad "%OPABASEDIR%\share\opa\READMEWIN.txt"

REM Detection Sublime in order to install the Opa package
	set SOPAMODE=Opa.sublime-package
	if exist "%APPDATA%\Sublime Text 2\" set SUBLIMEINSTALLED=%APPDATA%\Sublime Text 2\Installed Packages&goto :sublimeOK
	:sublimeKO
		echo Please, consider installing sublime http://www.sublimetext.com/ for a better Opa experience
		start http://www.sublimetext.com/
		goto :afterSublime
	:sublimeOK
		echo Sublime Text has been found
		copy /Y "%OPABASEDIR%\share\opa\%SOPAMODE%" "%SUBLIMEINSTALLED%\"
		echo The Opa plugin for Sublime Text has been installed.
	:afterSublime

:MongoDetect
REM Mongo detection
	for %%X in (mongod.exe) do (set FOUNDMONGO=%%~$PATH:X)
	if defined FOUNDMONGO goto MongoOK
	:MongoKO
		echo MongoDb cannot be found, please install it.
		start http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows
		start http://www.mongodb.org/downloads
		goto :afterMongo
	:MongoOK
		echo MongoDb has been found
	:afterMongo


:NodeDetect
REM Node detection
	for %%X in (node.exe) do (set FOUNDNODE=%%~$PATH:X)
	if defined FOUNDNODE goto NodeOK
	:NodeKO
		echo Nodejs cannot be found, please install it.
		start http://nodejs.org/#download
		pause
		goto :NodeDetect
	:NodeOK
		echo Nodejs has been found
		echo Make node the default js interpreter
		ftype JSFILE="%FOUNDNODE%" %%1 %%*
		echo Installing plugins
		npm -g install mongodb nodemailer imap formidable
		REM iconv

REM Vim & emacs
	REM vim/{ftdetect,syntax}/opa.vim
	REM vim/{ftdetect,syntax}/opa.vim
	REM if exists %HOME% then goto emacsOk
	REM :emacsKO
	REM goto :afterEmacs
	REM :emacsOk
	REM echo ; Opa Mode >> %DOTEMACS%
	REM echo (autoload 'opa-js-mode "%OPABASEDIR%\share\opa\emacs\opa-js-mode.el" "OPA JS editing mode." t) >> %DOTEMACS%
	REM echo (autoload 'opa-classic-mode "%OPABASEDIR%\share\opa\emacs\opa-mode.el" "OPA CLASSIC editing mode." t) >> %DOTEMACS%
	REM echo (add-to-list 'auto-mode-alist '("\\.opa$" . opa-js-mode)) ;; Set the default mode here >> %DOTEMACS%
	REM echo (add-to-list 'auto-mode-alist '("\\.js\\.opa$" . opa-js-mode)) >> %DOTEMACS%
	REM echo (add-to-list 'auto-mode-alist '("\\.classic\\.opa$" . opa-classic-mode)) >> %DOTEMACS%
	REM :afterEmacs

REM Letting the user the time to read something
	echo "In case you need to call me, I am in menu [ Start -> Opa -> post-install.cmd ]"
	pause
