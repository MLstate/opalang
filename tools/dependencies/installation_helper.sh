#!/usr/bin/env bash

set -u
set -e

SCRIPTDIR=$(dirname $0)
cd $SCRIPTDIR
SCRIPTDIR=$PWD

SCRIPTNAME=$(basename $0)

# OS independent various variables
# Warning: only put in DEFAULT_PACKAGES what should be included in our binary package
DEFAULT_PACKAGES=(ocaml findlib ulex camlzip ocamlgraph)
PRO_PACKAGES=($DEFAULT_PACKAGES cryptokit ocaml-ssl)
ALL_PACKAGES=(ocaml findlib ulex camlzip ocamlgraph cryptokit openssl ocaml-ssl syslog jpeg libpng giflib camlimages mascot camlidl libnatpmp miniupnpc cairo-ocaml node)

BUILD_DIR=$SCRIPTDIR/packages

# ##
# Bash setting and useful functions
# #####
shopt -s nullglob
msg_green ()  { echo "[32m$@[0m"; }
msg_red ()    { echo "[31m$@[0m"; }
msg_yellow () { echo "[33m$@[0m"; }
help () {
    cat <<EOF

[1minstallation_helper.sh[0m
helps download, patch, build and install the dependencies of Opa.

Usage:
	$0 --prefix <dir> [options] [commands]

Options:
	--prefix <dir>		where ocaml and libs should expect to be installed (mandatory)
	--installdir <dir>	where to actually do the install (by default, the value of --prefix)
	--libdir <dir>	        where to actually do the lib install (by default, the value of --prefix)
	--packages 'pkg list'	only install the specified packages
	--sudo			sudo before installation commands ([1msee disclaimer[0m)

Commands: (default choice underlined)
	[4mfetch[0m			download the archives for the packages to
				$BUILD_DIR
	[4munzip[0m			uncompress and patch the archives
	[4minstall[0m			build and install the packages
	lightclean		make clean in all packages' directories
	clean			remove all contents of the work directory

Available packages: (default choice underlined)
	$(for i in ${!ALL_PACKAGES[@]}; do
            if grep -q "\(^\| \)${ALL_PACKAGES[i]}\( \|$\)" <<<${DEFAULT_PACKAGES[*]}
            then printf "%-18s" "[4m${ALL_PACKAGES[i]}[0m"
            else printf "%-10s" "${ALL_PACKAGES[i]}"
            fi
            [ $((i%6)) -eq 5 ] && echo -ne "\n\t" || echo -ne ' '
          done)

Note:
	All ocaml libs are compiled with and for the ocaml in the
	given prefix. Therefore, ocaml should either be already there,
	or you should include it first in your --packages. Also, some
	packages depend on findlib to compile correctly.

Examples:
	- Install all dependencies of opa to /opt/opa:
        $ [4m$0 --prefix /opt/opa --sudo[0m
	then, pass option -ocamlopt /opt/opa/bin/ocamlopt.opt to
	opalang's ./configure script

	- Install all dependencies in your home, when you don't have
	  administration rights:
        $ [4m$0 --prefix ~/opa[0m
	then, pass option -ocamlopt ~/opa/bin/ocamlopt.opt to
	opalang's ./configure script

Disclaimer:
	This script is provided because we think it may be useful, but
	is provided WITHOUT SUPPORT, and you should know what you are
	doing, especially if you want to install to a privileged
	prefix like /usr/local.  Using this script to install in /usr
	would be a [1mbad idea[0m.

EOF
}

source ../platform_helper.sh

WINTOOLS=${WINTOOLS:-}
WINOCAMLC=$WINTOOLS/windows_ocamlc
WINOCAMLOPT=$WINTOOLS/windows_ocamlopt
WINOCAMLDEP=$WINTOOLS/windows_ocamldep
WINOCAMLMKLIB=$WINTOOLS/windows_ocamlmklib
WINOCAMLLEX=$WINTOOLS/windows_ocamllex

if [ -n "${ENFORCE_WINDOWS:-}" ];
then
    msg_yellow "ENFORCE WINDOWS -> ocaml windows scripts"
    export OCAMLC=$WINOCAMLC
    export OCAMLOPT=$WINOCAMLOPT
    export OCAMLDEP=$WINOCAMLDEP
    export OCAMLMKLIB=$WINOCAMLMKLIB
    export OCAMLLEX=$WINOCAMLLEX
    export CAMLP4=camlp4
else
    OCAMLC=ocamlc.opt
    OCAMLOPT=ocamlopt.opt
    OCAMLDEP=ocamldep.opt
    OCAMLMKLIB=ocamlmklib
    CAMLP4=camlp4
fi


PREFIX=
SUDO=
CLEAN=
FETCH=
UNZIP=
INSTALL=
LIGHTCLEAN=
while [ $# -gt 0 ]; do
    case $1 in
        --help) help; exit 0;;
        --prefix)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            PREFIX="$1"
	    PREFIX=${PREFIX%%/}
            : ${INSTALLDIR:=$PREFIX}
            : ${LIBDIR:=$PREFIX}
            ;;
        --packages)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            PACKAGES=($1)
            ;;
        --installdir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            INSTALLDIR="$1"
	    INSTALLDIR=${INSTALLDIR%%/}
            ;;
        --libdir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            LIBDIR="$1"
	    LIBDIR=${LIBDIR%%/}
            ;;
        --sudo)
            SUDO="sudo"
            ;;
        lightclean) LIGHTCLEAN=1;;
        clean) CLEAN=1;;
        fetch) FETCH=1;;
        install) INSTALL=1;;
        unzip) UNZIP=1;;
        *)
            echo "Error: unknown argument $1"; exit 1
    esac
    shift
done

PACKAGES=("${PACKAGES[@]:-${DEFAULT_PACKAGES[@]}}")

if [ -z "$CLEAN" ] && [ -z "$FETCH" ] && [ -z "$UNZIP" ] && [ -z "$INSTALL" ] && [ -z "$LIGHTCLEAN" ]; then
    FETCH=1
    UNZIP=1
    INSTALL=1
fi
if [ -z "$PREFIX" ] && [ -n "$INSTALL" ]; then
    help
    echo "Error: you must specify the installation prefix" >&2
    exit 1
fi

#FETCH=""
#CLEAN=""
#UNZIP=""

BUILD_DIR=$PWD/packages
mkdir -p $BUILD_DIR

sources () {
    case $1 in
        openssl) echo "http://www.openssl.org/source/openssl-1.0.0a.tar.gz";;
        ocaml) echo "http://caml.inria.fr/pub/distrib/ocaml-4.00/ocaml-4.00.1.tar.gz" ;;
        # Sources obtained thanks to "apt-get --print-uris source <package>"
        findlib) echo "http://download.camlcity.org/download/findlib-1.3.3.tar.gz";;
        ocaml-ssl) echo "http://downloads.sourceforge.net/project/savonet/ocaml-ssl/0.4.6/ocaml-ssl-0.4.6.tar.gz";;
        cryptokit) echo "http://forge.ocamlcore.org/frs/download.php/891/cryptokit-1.6.tar.gz";;
        ocamlgraph) echo "http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.2.tar.gz";;
        camlzip) echo "http://forge.ocamlcore.org/frs/download.php/1037/camlzip-1.05.tar.gz";;
        camlimages) echo "http://caml.inria.fr/distrib/bazar-ocaml/camlimages-3.0.2.tgz";;
        ulex) echo "http://www.cduce.org/download/ulex-1.1.tar.gz";;
        syslog) echo "http://homepage.mac.com/letaris/syslog-1.4.tar.gz";;
        jpeg) echo "http://www.ijg.org/files/jpegsrc.v8b.tar.gz";;
        libpng) echo "http://sourceforge.net/projects/libpng/files/libpng14/older-releases/1.4.3/libpng-1.4.3.tar.gz";;
        giflib) echo "http://downloads.sourceforge.net/project/giflib/giflib%204.x/giflib-4.1.6/giflib-4.1.6.tar.gz";;
        mascot) echo "http://mascot.x9c.fr/distrib/mascot-1.0-alpha.tar.gz";;
        camlidl) echo "http://caml.inria.fr/pub/old_caml_site/distrib/bazar-ocaml/camlidl-1.05.tar.gz";;
        libnatpmp) echo "http://miniupnp.free.fr/files/download.php?file=libnatpmp-20110103.tar.gz";;
        miniupnpc) echo "http://miniupnp.free.fr/files/download.php?file=miniupnpc-1.5.20110418.tar.gz";;
        cairo-ocaml) echo "http://cgit.freedesktop.org/cairo-ocaml/snapshot/cairo-ocaml-1.2.0.tar.gz";;
	node) echo "http://nodejs.org/dist/v0.6.19/node-v0.6.19.tar.gz";;
        *) msg_red "Error: don't know about package $1" >&2; exit 2
    esac
}

package_dir () {
    local p=$1
    local dir=$(find . -maxdepth 1 -type d -name "$p-[v0-9.]*")
    local dir="$dir "$(find . -maxdepth 1 -type d -name "$p-master")
    echo $dir | (read a b; if [ -n "$b" ]; then
            msg_red "Error: found several source directories for $p"
            exit 12
            fi)
    echo $dir
}

install_generic () {
    if [ "$(which $OCAMLC)" != "$INSTALLDIR/bin/$OCAMLC" ]; then
        echo $OCAMLC x $PATH
        msg_red "Error: fresh installed ocaml not found ($(which $OCAMLC) is not $INSTALLDIR/bin/$OCAMLC)"
        exit 1
    fi
    if [ "$($OCAMLC -where)" != "$INSTALLDIR/lib/ocaml" ]; then
        msg_red "Error: fresh installed ocaml not found (ocamlc -where returned $($OCAMLC -where) instead of $INSTALLDIR/lib/ocaml"
        exit 1
    fi
    # Packages install themselves in $(ocamlc -where) automatically
    if [ -x configure ]; then ./configure "$@"; fi
    make all; r=$?
    make allopt || make opt || make all.opt || [ $r -eq 0 ]
    $SUDO make install
    $SUDO make installopt || $SUDO make install-opt || true
}

package_install (){
    if [ -n "${ENFORCE_WINDOWS:-}" ];
    then
	echo "INSTALLSYS WINDOWS"
        WINPREFIX=`cygpath -u $PREFIX`
        WINCAMLLIBDIR="$WINPREFIX/lib"
        OLDPATH=$PATH
        export PATH=/cygdrive/c/ocamlms/bin/:$PATH
        case $1 in
            ocaml)
#         caml_sed_patch
          # PREFIX is hard coded as C:/ocamlms
                cp config/m-nt.h config/m.h
                cp config/s-nt.h config/s.h
                cp config/Makefile.msvc config/Makefile
                make -f Makefile.nt clean
#         rm -rf otherlibs/labltk/
                make -f Makefile.nt world
#         make -f Makefile.nt bootstrap
#         make -f Makefile.nt clean
#         make -f Makefile.nt world
                make -f Makefile.nt opt opt.opt
                make -f Makefile.nt install
          #missing ocamlrun.lib => replaced by libcamlrun.lib
                export PATH=$WINPREFIX/bin:${PATH}
                export OCAMLLIB=$WINCAMLLIBDIR
                export OCAMLC=$WINOCAMLC
                export OCAMLOPT=$WINOCAMLOPT
                export OCAMLDEP=$WINOCAMLDEP
                export OCAMLMKLIB=$WINOCAMLMKLIB
                export OCAMLLEX=$WINOCAMLLEX
                export CAMLP4=camlp4
                ;;
            findlib)
          #valid
                ./configure -sitelib $PREFIX/lib
                make all opt install clean
                ;;
            camlzip)
          #valid
          # library is call camlzip instead of zip
                CAMLLIBDIR=$WINCAMLLIBDIR ZLIB_LIBDIR=/windows_libs/zlib ZLIB_INCLUDE=$PWD/../../../ms_windows/zlib make -f Makefile.nt clean all allopt install
                CAMLLIBDIR=$WINCAMLLIBDIR ZLIB_LIBDIR=/windows_libs/zlib ZLIB_INCLUDE=$PWD/../../../ms_windows/zlib make -f Makefile.nt installopt || true
                install -m 0644 -v zlib.cm* $WINCAMLLIBDIR/zip
                ;;
            ocamlgraph)
                ./configure --prefix $WINPREFIX # should pass all var to have cross compilation
                make -f Makefile.nt clean
          # let the time do its job :) do not merge the two or prepare to a headache
                make -f Makefile.nt byte opt install-byte install-opt
                ;;
            cryptokit)
                make -f Makefile.win32 all allopt
                make -f Makefile.win32 install
                ;;
            ocaml-ssl)
                #valid but ugly
                # just for generating the META file and Makefile
                # ,does not work with PATH because of cl.exe problems with libs
                PATH=$OLDPATH ./configure
                export OCAMLMKLIB="$OCAMLMKLIB -LC:/cygwin/windows_libs/openssl/lib"
                   # seems horrible ? simpler than patching everything
                make -e byte || true
                make -e opt || true
                make -e install || true
                cp src/*.lib $PREFIX/lib/ssl # because ocamlfind ignores .lib files
                ;;
            camlidl)
                sed -ie "s%^OCAMLLIB=.*%OCAMLLIB=$WINPREFIX/lib/ocaml%; s%^BINDIR=.*%BINDIR=$WINPREFIX/bin%" \
                    config/Makefile.win32 > config/Makefile
                make all
                make install
                ;;
            libnatpmp)
                make -f Makefile.windows all
                TMPP=$PREFIX/lib/libnatpmp
                mkdir -p $TMPP
                cp libnatpmp.a $TMPP
                TMPP=$PREFIX/include/libnatpmp
                mkdir -p $TMPP
                cp *.h $TMPP
                ;;
            miniupnpc)
                INSTALLPREFIX=$PREFIX make -f Makefile.windows all
                TMPP=$PREFIX/lib/miniupnpc
                mkdir -p $TMPP
                cp libminiupnpc.a $TMPP
                TMPP=$PREFIX/include/miniupnpc
                mkdir -p $TMPP
                cp *.h $TMPP
                ;;
            ulex)
                rm -f myocamlbuild.ml
                install_generic
                ;;
            cairo-ocaml)
                aclocal -I support
                autoconf
                ./configure --prefix $INSTALLDIR --without-gtk --without-libsvg_cairo --without-libpangocairo
                make
                make install
                ;;
	    node)
		vcbuild.bat
		npm install -g nodemailer
		npm install -g simplesmtp
		npm install -g imap
		npm install -g mongodb
		npm install -g formidable
                # node-iconv doesn't work on Windows yet
                # npm install -g iconv
		;;
            *)
                msg_yellow "Install $1 by hand"
                msg_yellow "TODO : check $1 installation"
                exit 1
                ;;
        esac
    else
        case $1 in
            ocaml)
                [ $IS_WINDOWS ] && CYGOPT="-tk-no-x11 -no-tk"
                ./configure -prefix $PREFIX ${CYGOPT:-}
                if [ $IS_LINUX ] || [ $IS_MAC ] || [ $IS_FREEBSD ]; then
		    make world.opt bootstrap opt.opt
		else
                    make world bootstrap
                fi
                PREFIX=$INSTALLDIR $SUDO make install -e
		make clean
                ;;
            camlzip)
		install_generic
                # make all allopt
                # $SUDO make install installopt
                $SUDO install -m 0644 -v zlib.cm* $($OCAMLC -where)/zip
                ;;
            ocaml-ssl)
                $SUDO $INSTALLDIR/bin/ocamlfind remove -destdir $INSTALLDIR/lib/ocaml ssl || true
                install_generic
                ;;
	    cryptokit)
		# very ugly way to uninstall previous cryptokit...
		msg_yellow "Uninstalling potential previous cryptokit (moving it..)"
		$SUDO mkdir -pv $INSTALLDIR/lib/ocaml/cryptokit.bak
		$SUDO mv -v $INSTALLDIR/lib/ocaml/cryptokit.* $INSTALLDIR/lib/ocaml/cryptokit.bak/ || true
		$SUDO mv -v $INSTALLDIR/lib/ocaml/stublibs/dllcryptokit.so $INSTALLDIR/lib/ocaml/stublibs/dllcryptokit.so.bak  || true
		ocaml setup.ml -configure
		ocaml setup.ml -build
		$SUDO ocaml setup.ml -uninstall
		$SUDO ocaml setup.ml -install
		;;
            findlib)
                install_generic -sitelib $INSTALLDIR/lib/ocaml
                ;;
            camlimages)
                touch src/oXpm.cmi || true # camlimages wants that even if we tell him not...
                ./configure --with-ocamlfind=no --enable-native-library --enable-bytecode-library --with-jpeg  --with-png --with-gif --without-lablgtk --without-lablgtk2 --without-freetype --without-xpm --without-tiff --without-x --without-gs --prefix $INSTALLDIR \
                    CFLAGS=-I$INSTALLDIR/include LDFLAGS=-L$INSTALLDIR/lib
                make
                $SUDO make install
                ;;
            ulex)
                $SUDO $INSTALLDIR/bin/ocamlfind remove -destdir $INSTALLDIR/lib/ocaml ulex || true
                rm -f myocamlbuild.ml
                make all.opt
                # the following is copy pasted from ulex's Makefile
                # because install depends on all (it just does't work
                # when your rights for compiling and installing are not compatible)
                cd _build && $SUDO make -f ../Makefile realinstall
                ;;
            jpeg)
                ./configure --prefix $PREFIX
                make
                $SUDO make install
                ;;
            libpng)
                ./configure --prefix $PREFIX
                make
                $SUDO make install
                ;;
            giflib)
                ./configure --prefix $PREFIX
                make
                $SUDO make install
                ;;
            mascot)
                sh configure -ocaml-prefix $INSTALLDIR
                $SUDO make all install -e
                ;;
            camlidl)
                sed -e "s%^OCAMLLIB=.*%OCAMLLIB=$INSTALLDIR/lib/ocaml%; s%^BINDIR=.*%BINDIR=$INSTALLDIR/bin%" \
                    config/Makefile.unix > config/Makefile
                make -e all
                $SUDO make -e install
                ;;
            libnatpmp)
                make -e
                # custom installation
                $SUDO mkdir -p $INSTALLDIR/lib/ocaml
                $SUDO cp libnatpmp.a $INSTALLDIR/lib/ocaml
                INCLUDE=$INSTALLDIR/include/libnatpmp
                $SUDO mkdir -p $INCLUDE
                $SUDO cp *.h $INCLUDE
                ;;
            miniupnpc)
                INSTALLPREFIX=$INSTALLDIR make -e
                $SUDO mkdir -p $INSTALLDIR/lib/ocaml
                $SUDO cp libminiupnpc.a $INSTALLDIR/lib/ocaml
                INCLUDE=$INSTALLDIR/include/miniupnpc
                $SUDO mkdir -p $INCLUDE
                $SUDO cp *.h $INCLUDE
                ;;
            cairo-ocaml)
                aclocal -I support
                autoconf
                ./configure --prefix $INSTALLDIR --without-gtk --without-libsvg_cairo --without-libpangocairo
                make
                $SUDO make install
                ;;
	    node)
                if [ $IS_MAC ]; then
                    ./configure --prefix $INSTALLDIR --libdir $LIBDIR --openssl-libpath=/usr/lib
		else
                    ./configure --prefix $INSTALLDIR --libdir $LIBDIR
                fi
                make
                $SUDO make install
		$SUDO npm install -g nodemailer imap mongodb formidable iconv
		;;
            *)
                install_generic
        esac
    fi
}

if [ -n "$CLEAN" ]; then
    msg_yellow "$SCRIPTNAME: Cleaning source"
    for p in "${PACKAGES[@]}"; do
        msg_green "[ PACKAGE: $p (clean-up) ]"
        dir=$(package_dir $p)
        rm -rf $BUILD_DIR/$dir
    done
    msg_green "Remove building  directory $BUILD_DIR"
    rm -fr $BUILD_DIR/*
fi

if [ -n "$LIGHTCLEAN" ]; then
    msg_yellow "Cleaning packages configurations"
    cd $BUILD_DIR/
    for p in "${PACKAGES[@]}"; do
        dir=$(package_dir $p)
        if [ -z $dir ]; then
            msg_green "[ PACKAGE: $p (skip clean) ]"
        else
            msg_green "[ PACKAGE: $p (make clean) ]"
            echo "dir: $dir"
            cd $dir
            make clean
            cd -
        fi
    done
    msg_green "make general configuration clean done."
    cd $SCRIPTDIR
fi

if [ -n "$FETCH" ]; then
    msg_green "$SCRIPTNAME: Downloading packages"
    cd $BUILD_DIR
    for p in "${PACKAGES[@]}"; do
        msg_green "[ PACKAGE: $p (download) ]"
        src=$(sources $p)
        wget -N "$src"
        ## some lib are downloaded via a php script, we need to clean the string from 'source' a little
        tgz0=$(basename "$src")
        tgz=${tgz0#download.php\?file=}
        [ "$tgz0" != "$tgz" ] && [ -f "$tgz0" ] && mv "$tgz0" "$tgz"
        ## remove existing extracted directories
        packfile=$(tar -tzf $tgz 2>/dev/null | head -n 1)
        packdir=$(pwd)/${packfile%%/*}
        rm -rf $BUILD_DIR/${packdir#$BUILD_DIR}
    done
    cd $SCRIPTDIR
fi

if [ -n "$UNZIP" ]; then
    msg_green "$SCRIPTNAME: Uncompressing & patching"
    cd $BUILD_DIR
    for p in "${PACKAGES[@]}"; do
        msg_green "[ PACKAGE: $p (uncompressing) ]"
        echo "Uncompressing..."
        ## retrieve the file name
        tgz=$(basename "$(sources $p)")
        tgz=${tgz#download.php\?file=}
        ## uncompress
        tar -xzf $tgz
        ## patch
        packdir=$(pwd)/$(package_dir $p)
        cd $packdir
        if [ $IS_WINDOWS ]; then
            for df in $SCRIPTDIR/patches/${p}_*.patch.windows; do
                echo "Applying patch $df from $packdir"
                patch -Np1 < $df || (msg_red "Error: patch $df did not apply"; exit 3)
            done
        fi
        for df in $SCRIPTDIR/patches/${p}_*.patch; do
            echo "Applying patch $df from $packdir"
            patch -Np1 < $df || (msg_red "Error: patch $df did not apply"; exit 3)
        done
        echo "Uncompressed $SCRIPTNAME to $packdir"
        cd $BUILD_DIR
    done
    cd $SCRIPTDIR
fi

setup_local_ocaml () {
    #
    # Here we attempt to relocate ocaml, which is not an easy task. Beware the hacks.
    # (no $SUDO here, relocation is not supposed to be needed if we are already
    #  installing to a privileged place)
    if [ -d $INSTALLDIR/lib/ocaml ] && [ "$PREFIX" != "$INSTALLDIR" ]; then
        # Update ld.conf for the temporary location
        # (this is reverted last thing in this script)
        HAS_PATCHED_OCAML_LD_CONF=1
        sed -i "s%^$PREFIX%$INSTALLDIR%" $INSTALLDIR/lib/ocaml/ld.conf
        # The following are not needed in our binary package, so we
        # don't care about reverting the changes (we clean them up
        # before making the package)
        #
        # The bytecode exes refer to an interpreter that is not installed (<prefix>/bin/ocamlrun).
        sed -i '1s%#!'$PREFIX'/bin/ocamlrun%#!'$INSTALLDIR'/bin/ocamlrun%' $INSTALLDIR/bin/*
        # ocamlmklib doesn't play nice, we wrap them to add options
        if [ ! -e $INSTALLDIR/bin/ocamlmklib-bin ]; then
            mv $INSTALLDIR/bin/ocamlmklib $INSTALLDIR/bin/ocamlmklib-bin
            echo -e '#!/bin/sh\nexec '$INSTALLDIR/bin/ocamlmklib-bin' -ocamlc '$INSTALLDIR/bin/ocamlc' -ocamlopt '$INSTALLDIR/bin/ocamlopt' "$@"' >$INSTALLDIR/bin/ocamlmklib
            chmod a+x $INSTALLDIR/bin/ocamlmklib
        fi
        # camlp4 needs a shared library (dllunix, what for ??) that
        # isn't yet in normal library search paths
        for f in $INSTALLDIR/bin/camlp4*; do
            if [ $f = ${f%.opt} ] && [ ! -e $f-bin ]; then
                mv $f $f-bin
                echo -e '#!/bin/sh\nLD_LIBRARY_PATH='$INSTALLDIR/lib/ocaml/stublibs':$LD_LIBRARY_PATH exec '$f-bin' "$@"' >$f
                chmod a+x $f
            fi
        done
    fi
    msg_yellow "Extending path to find the proper ocaml (at $INSTALLDIR/bin)"
    export PATH=$INSTALLDIR/bin:$PATH
    # export LD_LIBRARY_PATH=$INSTALLDIR/lib/ocaml:$INSTALLDIR/lib/ocaml/stublibs/$LD_LIBRARY_PATH
    msg_yellow "Setting OCAMLLIB to the proper location ($INSTALLDIR/lib/ocaml)"
    export OCAMLLIB=$INSTALLDIR/lib/ocaml
    export OCAMLOPT=$INSTALLDIR/bin/ocamlopt.opt
    export OCAMLFIND_INSTFLAGS="-destdir $INSTALLDIR/lib/ocaml"
}

if [ -n "$INSTALL" ]; then
    msg_green "$SCRIPTNAME: Building & install"
    for i in ${!PACKAGES[@]}; do
        if [ -z "${setup_done:-}" ] && ! grep -q "\(^\| \)ocaml\( \|$\)" <<<${PACKAGES[*]:$i}; then
            setup_local_ocaml
            setup_done=1
        fi
        p="${PACKAGES[$i]}"
        cd $BUILD_DIR
        msg_yellow "entering build directory $BUILD_DIR"
        msg_green "[ PACKAGE: $p (building and installing) ]"
        dir=$(package_dir $p)
        echo "Directory: $dir"
        cd $dir
        echo "Package: $p"
        package_install $p
        cd $SCRIPTDIR
        msg_yellow "leaving build directory"
    done
fi

if [ -n "${HAS_PATCHED_OCAML_LD_CONF:-}" ]; then
    msg_yellow "Reverting temporary modification of ocaml's ld.conf used for building the libs"
    sed -i "s%^$INSTALLDIR%$PREFIX%" $INSTALLDIR/lib/ocaml/ld.conf
fi

msg_green "$SCRIPTNAME: done"
