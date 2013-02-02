#! /bin/sh

set -e
set -x

# exit 0
# correct the config file camlp4
# correct the config file pathes lib
# correct problem with mlstate_platform, manually build the .h file

INSTALLDIR=/tmp/installwin
#export IOSERVER="IOCP"
#export C_INCLUDE_PATH="/include"
#export WINDOWS_SIGNAL="1"
#export PATHEXT="$PATHEXT;.NATIVE"
#export OPABSL_NODE=1
export OCAMLRUNPARAM="l=128M"

echo Workaround links
./tools/ms_windows/rep_links.sh

echo install_release.sh
./tools/utils/install_release.sh -dir $INSTALLDIR -no-doc -no-man -keep-build -keep-install-sys # after a first run

echo make_package.sh
./tools/utils/make_package.sh -make-winpkg -dir $INSTALLDIR

echo Workaround links
./tools/ms_windows/undo_rep_links.sh
