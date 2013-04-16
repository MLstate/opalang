# -*- coding: utf-8 -*-
!include "MUI2.nsh"
#!include "FileAssociation.nsh"

### Env variables

!include "WordFunc.nsh"
!define REG_ENVIRONMENT "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"
!macro DualUseFunctions_ un_
function ${un_}SetPathVar
        # stack top: <'string to add'> / <AppendFlag>
        Exch $0		; new string
        Exch
        Exch $1		; append = 2, prefix = 1, remove = 0
        Push $R0	; saved working registers

        ReadRegStr $R0 HKLM "${REG_ENVIRONMENT}" "Path"

        ${Select} $1
        ${Case} 0
                ${${un_}WordAdd} "$R0" ";" "-$0" $R0
        ${Case} 1
                ${${un_}WordAdd} "$0" ";" "+$R0" $R0
        ${Case} 2
                ${${un_}WordAdd} "$R0" ";" "+$0" $R0
        ${EndSelect}

        WriteRegExpandStr HKLM "${REG_ENVIRONMENT}" "Path" "$R0"
        System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("PATH", R0).r2'

        Pop $R0				; restore registers
        Pop $1
        Pop $0
functionEnd
!macroend
!insertmacro DualUseFunctions_ ""
!insertmacro DualUseFunctions_ "un."

### end env variables



# General information
!define PRODUCT_NAME "Opa"
!define PRODUCT_PUBLISHER "MLstate"
!define PRODUCT_WEB_SITE "http://www.opalang.org"
!define PRODUCT_VERSION "0.9.0"


# We support french and english
!insertmacro MUI_LANGUAGE English
!insertmacro MUI_LANGUAGE French

LangString MsgPost ${LANG_ENGLISH} "Installation done.$\r$\nWould you like to view the README, verify dependencies and install syntax highlighting?$\r$\nYou can relaunch his operation later if needed ($INSTDIR\share\opa\post_install.cmd)"
LangString MsgPost ${LANG_FRENCH}  "Installation terminée.$\r$\nVoulez-vous lire le README, vérifier l'installation et installer la coloration syntaxique ?$\r$\nVous pouvez relancer cette operation plus tard si besoin ($INSTDIR\share\opa\post_install.cmd)"

# post install script
Function .onInstSuccess
    MessageBox MB_YESNO $(MsgPost) IDNO NoReadme
      Exec '"$INSTDIR\share\opa\post_install.cmd" "$INSTDIR"'
    NoReadme:
FunctionEnd


Name "${PRODUCT_NAME}"
outFile "installer.exe"
Icon bin\uninstall\opa_logo_72x72.ico
CRCCheck on
BrandingText "MLstate"

InstallDir "C:\Dev\Opa"

LangString MsgFolder ${LANG_ENGLISH} "Please, choose an installation folder (without symbols or spaces)."
LangString MsgFolder ${LANG_FRENCH}  "Veuillez choisir un répertoire d'intallation (sans symboles ou espaces)."

DirText $(MsgFolder)

# Include license
LicenseLangString license ${LANG_ENGLISH} share/doc/opa/LICENSE
LicenseLangString license ${LANG_FRENCH}  share/doc/opa/LICENSE
LicenseData $(license)

LangString MsgLicense ${LANG_ENGLISH} "Please read and accept Opa license"
LangString MsgLicense ${LANG_FRENCH} "Veuillez lire et accepter la licence d'utilisation d'Opa"
LicenseText $(MsgLicense)

#!define MUI_WELCOMEPAGE_TEXT "Hello!"
#!define MUI_WELCOMEPAGE_TITLE "Title!"
#!insertmacro MUI_PAGE_WELCOME

!define REG_UNINSTALL "Software\Microsoft\Windows\CurrentVersion\Uninstall\Opa"

Page license
Page directory
Page components
Page instfiles

Section Opa
DetailPrint "Opa"
SectionIn RO

WriteRegStr HKLM "${REG_UNINSTALL}" "DisplayName" "${PRODUCT_NAME}"
WriteRegStr HKLM "${REG_UNINSTALL}" "Publisher" "${PRODUCT_PUBLISHER}"
WriteRegStr HKLM "${REG_UNINSTALL}" "DisplayIcon" "$INSTDIR\bin\uninstall\opa_logo_72x72.ico"
WriteRegStr HKLM "${REG_UNINSTALL}" "UninstallString" "$INSTDIR\bin\uninstall\Uninstall.exe"
WriteRegStr HKLM "${REG_UNINSTALL}" "DisplayVersion" "1.0.7"
WriteRegDWORD HKLM "${REG_UNINSTALL}" "EstimatedSize" "70000"
WriteRegDWORD HKLM "${REG_UNINSTALL}" "NoModify" 1
WriteRegDWORD HKLM "${REG_UNINSTALL}" "NoRepair" 1
WriteRegStr HKLM "${REG_UNINSTALL}" "InstallSource" "$INSTDIR\"
WriteRegStr HKLM "${REG_UNINSTALL}" "InstallLocation" "$INSTDIR\"


SetOutPath "$INSTDIR"
#for i in $(ls -1d * | grep "/" | tr -d "/"); do echo file /r $i ; done to automate
file /r bin
file /r lib
file /r share

CreateDirectory "$SMPROGRAMS\Opa"
CreateShortCut "$SMPROGRAMS\Opa\opa.lnk" "$INSTDIR\bin\opa.bat"
CreateShortCut "$SMPROGRAMS\Opa\post_install.lnk" "$INSTDIR\share\opa\post_install.cmd"
CreateShortCut "$SMPROGRAMS\Opa\README.lnk" "$INSTDIR\share\opa\READMEWIN.txt"


writeUninstaller $INSTDIR\bin\uninstall\Uninstall.exe


Push 1		; prefix
Push "$INSTDIR\bin"
Call SetPathVar

!define env_hklm 'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
!define env_hku 'HKU ".DEFAULT\Environment"'
!define env_hkcu 'HKCU "Environment"'
WriteRegExpandStr ${env_hklm} "OPABASEDIR" "$INSTDIR"
ReadRegStr $R0 ${env_hkcu} "NODE_PATH"
WriteRegExpandStr ${env_hkcu} "NODE_PATH" "$R0;%USERPROFILE%\AppData\Roaming\npm\node_modules;$INSTDIR\lib\opa\stdlib;$INSTDIR\lib\opa\stdlib\stdlib.qmljs;$INSTDIR\lib\opa\static"

# .js are launchable
ReadRegStr $R0 ${env_hklm} "PATHEXT"
WriteRegExpandStr ${env_hklm} "PATHEXT" "$R0;.JS"
# using node
# AppAssocReg::SetAppAsDefault "node.exe" assoc_name type
# Pop $Var

SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

SectionEnd

#LangString MsgEx  ${LANG_ENGLISH} "OPA Examples"
#LangString MsgEx ${LANG_FRENCH} "Exemples OPA"

#Section $(MsgEx)
#SetOutPath "$INSTDIR"
#file /r examples
#SectionEnd


section "Uninstall"

ReadRegStr $0 HKLM "${REG_UNINSTALL}" "InstallSource"
DeleteRegKey /ifempty HKLM "${REG_UNINSTALL}"

Push 0		; remove
Push "$INSTDIR\bin"
Call Un.SetPathVar

# now delete installed files
# for i in $(ls -1d * | grep "/" | tr -d "/"); do echo RMDir /r $i ; done TO AUTOMATE
RMDir /r $0\bin
RMDir /r $0\lib
RMDir /r $0\share
RMDir $0

RMDir /r "$SMPROGRAMS\Opa"

sectionEnd
