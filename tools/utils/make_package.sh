#!/usr/bin/env bash

set -e
set -u

#PREFIX=/usr
INSTALLDIR=$PWD/release_install_root

# VERSION_MAJOR must be a version number, not text !!
VERSION_MAJOR=$(cat compiler/buildinfos/version_major.txt)
VERSION_BUILD=build

# the version string will be MAJORNAME+BUILDnnnn with nnnn the build number
# (MAJORNAME if BUILD is empty)

msg () {
    case "$1" in
        -n) shift; echo -n "[32m$*[0m";;
        *) echo "[32m$*[0m";;
    esac
}

. tools/platform_helper.sh

MYDIR=$PWD

OPALANG=`pwd` #$(dirname $0)

TBZ2=""
AUTOINSTALL=""
DEB="false"
PKG="false"
PKG_SIGN=""
WINPKG="false"
NOOCAML="false"

PACK_MAN="/Applications/PackageMaker.app" # PackageManager path for OS X

help() {
    echo "Makes an installation package from an installed Opa (installation"
    echo "should be done through install_release.sh)."
    echo "Options"
    # echo "	-prefix <dir>			Prefix where the package should install opa by"
    # echo "					default (default $PREFIX)"
    echo "	-dir <dir>			Where to get the installed tree (as specified"
    echo "					to install_release.sh; default $INSTALLDIR)"
    echo "	-dst <dir>			Where to put the package (default $MYDIR)"
    echo "	-no-ocaml			Filter out OCaml"
    echo "	-make-autoinstall <filename>	builds an auto-installing archive (you will need"
    echo "					MakeSelf installed). {} in filename is replaced by"
    echo "					the version string"
    echo "	-make-tbz2 <filename.tar.bz2>"
    echo "	-make-deb			Build a debian package"
    echo "	-make-pkg                       Build a Macintosh .pkg"
    echo "	-make-winpkg			Build a pakage for ms-windows (need nsis)"
    echo
}

while [ $# -gt 0 ]; do
    case $1 in
        -make-tbz2)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            TBZ2="$1";;
        -make-autoinstall)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            AUTOINSTALL="$1";;
        -make-deb)
            DEB=true;;
        -make-pkg)
            if [ -z "$OPALANG" ]; then
                echo "Error: you need to define \$OPALANG to build a mac package"
                exit 1
            fi
            PKG=true;;
	-sign-pkg)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            PKG_SIGN="$1";;
        -make-winpkg)
            if [ -z "$OPALANG" ]; then
                echo "Error: you need to define \$OPALANG to build a windows package"
                exit 1
            fi
            MAKE_PACKAGE_NON_ROOT="true"
            WINPKG=true;;
        # -prefix)
        #     if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
        #     shift
        #     PREFIX="$1";;
        -dir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            INSTALLDIR="$1";;
        -dst)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            MYDIR="$1";;
	-no-ocaml)
	    NOOCAML="true";;
        -help|--help|-h)
            help
            exit 0;;
        *)
            echo "Unknown option $1."
            help
            exit 1;;
    esac
    shift
done

if [ $(id -u) -ne 0 ] && [ -z "$IS_MAC" ] && [ -z "${MAKE_PACKAGE_NON_ROOT:-""}" ]; then
    msg "Error: running as user."
    msg "This would generate wrong packages, please run within fakeroot."
    exit 3
fi

if [ ${INSTALLDIR:0:1} != "/" ]; then
    msg "Error: please specify an absolute dir"
    exit 1
fi

BUILDNUM=$($INSTALLDIR/lib/opa/bin/opa-bin --version 2>&1 | sed 's/.*build \([0-9]\+\).*/\1/')
VERSION_STRING=${VERSION_MAJOR}$(if [ -n "$VERSION_BUILD" ]; then echo "+$VERSION_BUILD$BUILDNUM"; fi)

msg "Making package from installation in $INSTALLDIR, with Opa version $VERSION_STRING."

if [ -n "$TBZ2" ]; then
    msg "Making $TBZ2 (to be decompressed at the installation prefix)"
    msg "(after decompression in <prefix> (eg. /usr/local), the user will"
    msg "need to run <prefix>/share/opa/install/sh --dir <prefix>)"
    tar -C $INSTALLDIR -cj * -f $MYDIR/$TBZ2
fi

###################################
# auto-extract package generation #
###################################

if [ -n "$AUTOINSTALL" ]; then
    AUTOINSTALL=${AUTOINSTALL//\{\}/$BUILDNUM}
    msg "Making $AUTOINSTALL"
    cat >"$INSTALLDIR/install.sh" <<EOF
#!/usr/bin/env bash

set -e
set -u

if [ -n "\${1:-}" ]; then
    echo "Installing in \$1"
    INSTALL="\$1"
else
    PREFIX=\$HOME/mlstate-opa
    if [ "\$(id -u)" -eq 0 ]; then PREFIX=/usr/local; fi
    echo "Please specify install prefix. (default \$PREFIX)"
    INSTALL=""
    while [ -z "\$INSTALL" ]; do
      read INSTALL
      if [ -z "\$INSTALL" ]; then INSTALL="\$PREFIX"
      elif [ "\${INSTALL:0:1}" != "/" ]; then
        echo "Please provide an absolute directory"
        INSTALL=""
      else
        mkdir -p "\$INSTALL" || {
          echo "Could not create directory \$INSTALL. Please specify another install prefix"
          INSTALL=""
        }
      fi
    done
fi

echo "Installing, please wait..."

mkdir -p "\$INSTALL"
GLOBIGNORE=install.sh
cp -a * "\$INSTALL"

\$INSTALL/share/opa/install.sh --quiet --dir "\$INSTALL"

echo
echo "Done !"
echo "Documentation is in \$INSTALL/share/doc/opa"
echo "If you want to uninstall, run \$INSTALL/share/opa/uninstall.sh"
EOF
    chmod a+x "$INSTALLDIR/install.sh"

    cat >"$INSTALLDIR/share/opa/uninstall.sh" <<"EOF"
#!/usr/bin/env bash

set -e
set -u

SCRIPT="$0"
if [ "${SCRIPT:0:1}" != "/" ]; then
  SCRIPT=$PWD/$SCRIPT
fi

# Always run from /tmp
TMPDIR="${TMPDIR:-/tmp}"
if [ $# -eq 0 ] || [ "${SCRIPT:0:${#TMPDIR}}" != "$TMPDIR" ]; then
  INSTALLDIR=${SCRIPT%/share/opa/*}
  X=$(mktemp $TMPDIR/uninstall_opa.XXXXX)
  cp --preserve=mode "$0" "$X"
  exec "$X" "$INSTALLDIR" "$@"
else
  if [ $# -ne 1 ]; then echo "Error: bad number of arguments"; exit 1; fi
  INSTALLDIR="$1"
  trap "rm -f $0" EXIT
fi

echo -n "Going to remove Opa from $INSTALLDIR. Proceed ? [y/yes,n/no]"
read yesno
if [ "${yesno:0:1}" != "y" ]; then exit 0; fi

"$INSTALLDIR"/share/opa/install.sh --dir "$INSTALLDIR" --uninstall

P=$PWD; cd "$INSTALLDIR"
rm -f \
EOF
  ( cd $INSTALLDIR; find . ! -type d | sed 's/^\(.*\)$/  "\1" \\/' ) >>$INSTALLDIR/share/opa/uninstall.sh
  echo >>"$INSTALLDIR/share/opa/uninstall.sh"
  cat >>"$INSTALLDIR/share/opa/uninstall.sh" <<"EOF"
cd $P

for DIR in "$INSTALLDIR/share/opa" "$INSTALLDIR/share/man" "$INSTALLDIR/share/doc/opa" "$INSTALLDIR/lib/opa"; do
  R=0
  find $DIR \( -type d -empty \) -delete || R=$?
  if [ $R -ne 0 ] || [ -d $DIR ]; then
    echo "Warning: some files left in $DIR"
  fi
done

set +e
rmdir "$INSTALLDIR/share/man" 2>/dev/null
rmdir "$INSTALLDIR/share/doc" 2>/dev/null
rmdir "$INSTALLDIR/share" 2>/dev/null
rmdir "$INSTALLDIR/lib" 2>/dev/null
rmdir "$INSTALLDIR/bin" 2>/dev/null
rmdir "$INSTALLDIR" 2>/dev/null

echo "Done."
EOF
    chmod a+x "$INSTALLDIR/share/opa/uninstall.sh"

    # if [ $NOOCAML = "true" ]; then
    # 	rm -rf $INSTALLDIR.final
    # 	cp -r $INSTALLDIR $INSTALLDIR.final
    # 	INSTALLDIR=$INSTALLDIR.final
    # fi

    makeself --bzip2 $INSTALLDIR "$MYDIR/$AUTOINSTALL" "$AUTOINSTALL" ./install.sh
    msg "Generated $AUTOINSTALL"
fi

#############################
# MacOSX package generation #
#############################
if [ "$PKG" = "true" ]; then
    OS_VARIANT=`sw_vers -productVersion`
    PKG_NAME="opa-$BUILDNUM"
    echo "Making package '$MYDIR/$PKG_NAME.pkg'"
    # MOREOPTS=""
    # if [ $NOOCAML = "true" ]; then
    # 	 MOREOPTS="--filter lib/opa/static/*"
    # fi
    $PACK_MAN/Contents/MacOS/PackageMaker --root $INSTALLDIR --resources $OPALANG/tools/installer/Mac/Resources --scripts $OPALANG/tools/installer/Mac/Scripts --info $OPALANG/tools/installer/Mac/PackageInfo --id com.mlstate.opa.pkg -o "$MYDIR/$PKG_NAME.pkg" -n $BUILDNUM --domain system --root-volume-only --discard-forks -m --verbose --title "Opa $VERSION_MAJOR" --target 10.5
    echo "Creating image '$MYDIR/$PKG_NAME.dmg'"
    if [ -f "$MYDIR/$PKG_NAME.dmg" ]; then
	rm "$MYDIR/$PKG_NAME.dmg"
    fi
    if [ -n "$PKG_SIGN" ]; then
	mv "$MYDIR/$PKG_NAME.pkg" "$MYDIR/$PKG_NAME.orig.pkg"
	productsign --sign "$PKG_SIGN" "$MYDIR/$PKG_NAME.orig.pkg" "$MYDIR/$PKG_NAME.pkg"
	spctl -a -v --type install "$MYDIR/$PKG_NAME.pkg"
    fi
    hdiutil create "$MYDIR/$PKG_NAME.dmg" -srcfolder "$MYDIR/$PKG_NAME.pkg"
fi


################################
# MsWindows package generation #
################################
clear_bat() {
    cat $1 | grep -v "REM" | grep -v "^$"
}
if [ "$WINPKG" = "true" ]; then
  echo WINDOWS PACKAGE
  PKG_NAME="Opa $VERSION_MAJOR Build $BUILDNUM"
  PATH=/cygdrive/c/OPACOMPILE/NSIS:"/cygdrive/c/Program Files/NSIS":$PATH
  MAKENSIS=makensis
  if ! which $MAKENSIS &>/dev/null; then
    echo Cannot find makensis, correct you PATH variable.
  fi
 # import from OPALANG
  # -nsis script
  cp $OPALANG/tools/ms_windows/opa_pkg/install.nsi $INSTALLDIR

  mkdir -p $INSTALLDIR/bin
  # -opa.bat install
  clear_bat $OPALANG/tools/ms_windows/opa_pkg/opa.bat > $INSTALLDIR/bin/opa.bat

  cp -u $OPALANG/tools/ms_windows/opa_pkg/README.txt $INSTALLDIR/share/opa/READMEWIN.txt
  # -setup local commodities (editor mode ...)
  clear_bat $OPALANG/tools/ms_windows/opa_pkg/post_install.cmd > $INSTALLDIR/share/opa/post_install.cmd
  cp $OPALANG/utils/Opa.sublime-package $INSTALLDIR/share/opa/Opa.sublime-package || true
   
  # Some tidying, adding exe extension
  rm $INSTALLDIR/bin/{filepos,genproto,jsstat,mlidl,mlstate_platform,ocamlbase,ocamlbase.top,odep,odeplink,ofile,passdesign,qmljs,trx,trx_interpreter,wsdl2ml,opadoc,opa-translate,opa2opa} || true

  mv -u $INSTALLDIR/lib/opa/bin/opa-bin  $INSTALLDIR/lib/opa/bin/opa-bin.exe                               || true
  mv -u $INSTALLDIR/lib/opa/bin/opa-plugin-browser-bin  $INSTALLDIR/lib/opa/bin/opa-plugin-browser-bin.exe || true
  mv -u $INSTALLDIR/lib/opa/bin/opa-plugin-builder-bin  $INSTALLDIR/lib/opa/bin/opa-plugin-builder-bin.exe || true
  mv -u $INSTALLDIR/bin/opa-create  $INSTALLDIR/bin/opa-create.exe || true
  mv -u $INSTALLDIR/opatop $INSTALLDIR/lib/opa/bin/opatop.exe || true
  mv -u $INSTALLDIR/opa-db-server $INSTALLDIR/lib/opa/bin/opa-db-server.exe || true
  mv -u $INSTALLDIR/opa-db-tool $INSTALLDIR/lib/opa/bin/opa-db-tool.exe || true
  mv -u $INSTALLDIR/lib/bash_nt/bash $INSTALLDIR/lib/bash_nt/bash.exe || true

  rm $INSTALLDIR/bin/opa || true # has a bat equiv
  rm $INSTALLDIR/bin/opa-plugin-builder || true # should be bat file
  rm $INSTALLDIR/bin/opa-plugin-browser || true # should be bat file
  mv -u $INSTALLDIR/lib/opa/bin/opa-cloud $INSTALLDIR/lib/opa/bin/opa-cloud.sh || true # should have a bat wrapper

  #opapro
    # clear_bat $OPALANG/tools/ms_windows/opa_pkg/opa_node.bat > $INSTALLDIR/bin/opa_node.bat
    # -bash.bat install #dummy bash interpreter for mingw using a true bash interpretor, only set the env
    #cp $OPALANG/tools/ms_windows/opa_pkg/bash.bat $INSTALLDIR/bin/bash.bat
    #cp $OPALANG/tools/ms_windows/opa_pkg/path.sh $INSTALLDIR/bin/path.sh
    #
    #shorten-path
    #mv -u $INSTALLDIR/lib/opa/ocaml $INSTALLDIR/lib/ocaml || true

  #uninstall   
  mkdir -p $INSTALLDIR/bin/uninstall
  cp $OPALANG/tools/ms_windows/opa_pkg/opa_logo*.ico $INSTALLDIR/bin/uninstall
   
  #mv pkg_tools/ms_windows/share/opa/examples pkg_tools/ms_windows/examples
  PACKAGE=opa_${VERSION_STRING}.x86.exe
  rm -f $INSTALLDIR/installer.exe $PACKAGE || true
  $MAKENSIS $(cygpath -d $INSTALLDIR/install.nsi)
  mv $INSTALLDIR/installer.exe $PACKAGE
  echo The package is in $PACKAGE
fi




#############################
# Debian package generation #
#############################

if [ "$DEB" = "true" ]; then
    MAINTAINER="MLstate <package.maintainer@opalang.org>"
    PREFIX=/usr
    WORKDIR=$(mktemp -d /tmp/mkdeb.XXXXX)
    cd $WORKDIR
    DEBROOT=$WORKDIR/debian
    mkdir -p $DEBROOT$PREFIX
    mkdir -p $DEBROOT/DEBIAN
    mkdir _build
    rm -f $INSTALLDIR/install.sh
    cp -a $INSTALLDIR/* $DEBROOT$PREFIX
    ARCH=$(dpkg --print-architecture)
    # if [ $NOOCAML = "true" ]; then
    # 	 rm -rf $DEBROOT$PREFIX/lib/opa/static
    # fi
    find debian -type d | xargs chmod 755

    cat > debian/DEBIAN/control <<EOF
Package: opa
Version: $VERSION_STRING
Section: devel
Priority: optional
Architecture: $ARCH
Installed-Size: $(du -s $INSTALLDIR | cut -f1)
Pre-Depends: debconf
Depends: libc6 (>= 2.3.2), nodejs, npm
Maintainer: $MAINTAINER
Description: Enterprise Framework for JavaScript.
 Opa is an advanced application framework for JavaScript.
 All aspects are directly written in Opa:
 Frontend code, backend code, database queries and configuration.
 And everything is strongly statically typed.
 Learn more: http://opalang.org
EOF

#     cat > debian/DEBIAN/postinst <<EOF
# #!/bin/sh

# # npm install -g mongodb formidable nodemailer simplesmtp imap
# EOF
#     chmod 755 debian/DEBIAN/postinst

    # The wrapper scripts are always bound to $PREFIX, pre-generate them...
    $DEBROOT$PREFIX/share/opa/install.sh --quiet --dir "$DEBROOT$PREFIX" --prefix "$PREFIX"
    rm $DEBROOT$PREFIX/share/opa/install.sh

    mkdir -p $DEBROOT$PREFIX/share/lintian/overrides
    cat > $DEBROOT$PREFIX/share/lintian/overrides/opa <<EOF
# The package itself is in AGPL, but includes other software and lists their licenses
# in the copyright file
opa binary: copyright-should-refer-to-common-license-file-for-lgpl
EOF

    dpkg-deb --build debian _build
    PACKAGE=$(ls _build/*.deb)
    NEW_PACKAGE=$(basename "$PACKAGE")
    if [ "$ARCH" = "amd64" ]; then
	NEW_PACKAGE="opa-$BUILDNUM.x64.deb"
    elif [ "$ARCH" = "i386" ]; then
	NEW_PACKAGE="opa-$BUILDNUM.x86.deb"
    fi
    mv $PACKAGE $MYDIR/$NEW_PACKAGE
    msg "Generated $(basename "$NEW_PACKAGE")"
    cd $MYDIR
    rm -rf /tmp/${WORKDIR#/tmp/}
fi

msg 'Done !'
