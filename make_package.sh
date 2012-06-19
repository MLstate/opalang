#!/usr/bin/env bash

set -e
set -u

#PREFIX=/usr
INSTALLDIR=$PWD/release_install_root

# VERSION_MAJOR must be a version number, not text !!
VERSION_MAJOR=$(cat buildinfos/version_major.txt)
OFFICIAL_VERSION_NAME=$(cat<buildinfos/version_name.txt)
# VERSION_NAME shall be a string of alphanumeric characters or . + ~ (Debian guidelines)
VERSION_NAME=$(tr '[:upper:]' '[:lower:]' <buildinfos/version_name.txt)
VERSION_BUILD=build

# the version string will be MAJORNAME+BUILDnnnn with nnnn the build number
# (MAJORNAME if BUILD is empty)

msg () {
    case "$1" in
        -n) shift; echo -n "[32m$*[0m";;
        *) echo "[32m$*[0m";;
    esac
}

. platform_helper.sh

MYDIR=$PWD

OPAGENERAL=$(dirname $0)

TBZ2=""
AUTOINSTALL=""
DEB="false"
PKG="false"
WINPKG="false"

PACK_MAN="/Applications/PackageMaker.app" # PackageManager path for OS X

help() {
    echo "Makes an installation package from an installed Opa (installation"
    echo "should be done through install_release.sh)."
    echo "Options"
    # echo "	-prefix <dir>			Prefix where the package should install opa by"
    # echo "					default (default $PREFIX)"
    echo "	-dir <dir>			Where to get the installed tree (as specified"
    echo "					to install_release.sh; default $INSTALLDIR)"
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
            if [ -z "$OPAGENERAL" ]; then
                echo "Error: you need to define \$OPAGENERAL to build a mac package"
                exit 1
            fi
            PKG=true;;
        -make-winpkg)
            if [ -z "$OPAGENERAL" ]; then
                echo "Error: you need to define \$OPAGENERAL to build a windows package"
                exit 1
            fi
            WINPKG=true;;
        # -prefix)
        #     if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
        #     shift
        #     PREFIX="$1";;
        -dir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            INSTALLDIR="$1";;
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

if [ $(id -u) -ne 0 ] && [[ -z "$IS_MAC" ]] || [ -n "${MAKE_PACKAGE_NON_ROOT:-""}" ]; then
    msg "Error: running as user."
    msg "This would generate wrong packages, please run within fakeroot."
    exit 3
fi

if [ ${INSTALLDIR:0:1} != "/" ]; then
    msg "Error: please specify an absolute dir"
    exit 1
fi

BUILDNUM=$($INSTALLDIR/lib/opa/bin/opa-bin --version 2>&1 | sed 's/.*build \([0-9]\+\).*/\1/')
VERSION_STRING=${VERSION_MAJOR}${VERSION_NAME}$(if [ -n "$VERSION_BUILD" ]; then echo "+$VERSION_BUILD$BUILDNUM"; fi)

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
    AUTOINSTALL=${AUTOINSTALL//\{\}/$VERSION_STRING}
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

echo -n "Going to remove Opa from $INSTALLDIR. Proceed ? "
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

    makeself --bzip2 $INSTALLDIR "$MYDIR/$AUTOINSTALL" "$AUTOINSTALL" ./install.sh
    msg "Generated $AUTOINSTALL"
fi

#############################
# MacOSX package generation #
#############################
if [ "$PKG" = "true" ]; then
    OS_VARIANT=`sw_vers -productVersion`
    PKG_NAME="Opa $VERSION_MAJOR $OFFICIAL_VERSION_NAME - Build $BUILDNUM for Mac OS X (64-bit)"
    echo "Making package '$MYDIR/$PKG_NAME.pkg'"
   $PACK_MAN/Contents/MacOS/PackageMaker --root $INSTALLDIR --resources $OPAGENERAL/installer/Mac/Resources/ --scripts $OPAGENERAL/installer/Mac/Scripts --info $OPAGENERAL/installer/Mac/Info.plist --id com.mlstate.opa.pkg -o "$MYDIR/$PKG_NAME.pkg" -n $BUILDNUM --domain system --root-volume-only --discard-forks -m --verbose --title "Opa $VERSION_MAJOR $OFFICIAL_VERSION_NAME"
   echo "Creating image '$MYDIR/$PKG_NAME.dmg'"
   if [ -f "$MYDIR/$PKG_NAME.dmg"]; then
       rm "$MYDIR/$PKG_NAME.dmg"
   fi
   hdiutil create "$MYDIR/$PKG_NAME.dmg" -srcfolder "$MYDIR/$PKG_NAME.pkg"
fi


################################
# MsWindows package generation #
################################
if [ "$WINPKG" = "true" ]; then
   PKG_NAME="Opa $VERSION_MAJOR $VERSION_NAME Build $BUILDNUM"
   rm -rf pkg_ms_windows
   mkdir -p pkg_ms_windows
   # Copy
   # -everyting
   cp -a $INSTALLDIR/* pkg_ms_windows/
   # -nsis script
   cp $OPAGENERAL/ms_windows/opa_pkg/install.nsi pkg_ms_windows
   # -opa.bat install
   cat $OPAGENERAL/ms_windows/opa_pkg/opa.bat | grep -v "^REM" | grep -v "^$" > pkg_ms_windows/bin/opa.bat
   # Some tidying
   mkdir -p pkg_ms_windows/bin/ocaml
   mkdir -p pkg_ms_windows/bin/uninstall
   mv pkg_ms_windows/share/opa/examples pkg_ms_windows/examples
   mv pkg_ms_windows/bin/opa-bin  pkg_ms_windows/bin/runopa.exe
   # hidding ocaml executable to be oustide of PATH
   mv pkg_ms_windows/bin/ocamlopt.opt.exe  pkg_ms_windows/bin/ocaml/ocamlopt.opt.exe
   cp $OPAGENERAL/ms_windows/opa_pkg/opa_logo*.ico pkg_ms_windows/bin/uninstall
   /cygdrive/c/Program\ Files/NSIS/makensis.exe pkg_ms_windows/install.nsi
   echo The package is in pkg_ms_windows/installer.exe
fi




#############################
# Debian package generation #
#############################

if [ "$DEB" = "true" ]; then
    MAINTAINER="package.maintainer@opalang.org"
    PREFIX=/usr
    WORKDIR=$(mktemp -d /tmp/mkdeb.XXXXX)
    cd $WORKDIR
    DEBROOT=$WORKDIR/debian
    mkdir -p $DEBROOT$PREFIX
    mkdir -p $DEBROOT/DEBIAN
    mkdir _build
    cp -a $INSTALLDIR/* $DEBROOT$PREFIX
    find debian -type d | xargs chmod 755

    cat > debian/DEBIAN/control <<EOF
Package: opa
Version: $VERSION_STRING
Section: devel
Priority: optional
Architecture: $(dpkg --print-architecture)
Installed-Size: $(du -s $INSTALLDIR | cut -f1)
Pre-Depends: debconf
Depends: libc6 (>= 2.3.2), libgdbm3, libssl0.9.8, libssl-dev, zlib1g, zlib1g-dev, libjpeg62, libpng12-0, libgif4
Maintainer: $MAINTAINER
Description: The unified language for web 2.0 development
 Opa is a unified programming language for web development. This self-contained
 package contains the Opa compiler, that compiles stand-alone web servers
 including AJAX features, database, etc. from single source files.
EOF

    # The wrapper scripts are always bound to $PREFIX, pre-generate them...
    $DEBROOT$PREFIX/share/opa/install.sh --quiet --dir "$DEBROOT$PREFIX" --prefix "$PREFIX"
    rm $DEBROOT$PREFIX/share/opa/install.sh

    mkdir -p $DEBROOT$PREFIX/share/lintian/overrides
    cat > $DEBROOT$PREFIX/share/lintian/overrides/opa <<EOF
# The package itself is in AGPL & APACHE, but includes other software and lists their licenses
# in the copyright file
opa binary: copyright-should-refer-to-common-license-file-for-lgpl
EOF

    dpkg-deb --build debian _build
    PACKAGE=$(ls _build/*.deb)
    mv $PACKAGE $MYDIR
    msg "Generated $(basename "$PACKAGE")"
    cd $MYDIR
    rm -rf /tmp/${WORKDIR#/tmp/}
fi

msg 'Done !'
