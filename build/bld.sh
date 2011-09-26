#!/usr/bin/env bash

set -u
set -e

# README BEFORE EDITING THIS FILE!
#
# This is supposed to be a portable script (Linux, Mac, and even Windows).
# Due to Windows limitations, avoid complex pipes (you can pipe to an
# executable, but not to a `while' loop).
# Don't refer to absolute directories ("/tmp" is not a good idea).
# And don't do tricky low level hacks.

CONFIG_SH=config.sh
if [ -n "${NO_CONFIG_SH:-}" ]; then
    CONFIG_SH=$0
else
    if [ ! -e $CONFIG_SH ]; then
        echo "Error: config.sh not found. Please run ./configure"
        exit 1
    fi
    . $CONFIG_SH
fi

export MLSTATELIBS=$PREFIX


print_help () {
cat <<EOF
bld: the MLstate build system
Usage: bld [options] [special-targets] [targets] [-- exec-parameters]

Options:
        -dir <dir>		set the installation directory (default: \$MLSTATELIBS)
        -help			this help
        -release		enables release mode, disables debug and asserts
        -nocompile		don't check compilation of libs and tools, only install what we can
        -bytecode		try to only use bytecode
        -private                UNDOCUMENTED

Special targets:
        clean			removes compiled files
        [un]install-libs	automatically [un]install libs from "build_libs"
        [un]install-tools	automatically [un]install tools from "build_tools"
        [un]install		[un]install both libs and tools
        libs=lib1[,lib2,...]	subset of libs to install
        tools=tool1[,tool2,...]	subset of tools to install

Targets:
        <target>		compiles targets from file <target>.itarget
        <libname>.doc		builds library documentation in <libname>.docdir
        <target>.exe		finds <target>.ml, builds and copies the native exe
        <target>.<ext>		compiles target with ocamlbuild (see below)

Quick reference on special ocamlbuild targets:
        <source file>	<target>
        %.<any_src>	%.byte, %.native	byte/native program
        %.mllib		%.cma, %.cmxa		byte/native library
        %.mltop		%.top			custom toplevel
        %.odocl		%.docdir		ocamldoc documentation
        %.itarget	%.otarget (virtual)	set of targets

Environment variables:
        \$MLSTATELIBS		prefix where everything should be installed
        \$BLD_NOTIFY		if "1", you'll be notified when bld finished
                                (by means of "notify-send")

Other options are passed to ocamlbuild, which runs the generated binary if "--"
is used.

bld works from the contents of files "build_libs", "build_tools" and "build_rules*.ml".
See the README in libqml for syntax and more information.
EOF
}

# try to find the appropriate directory
RELDIR=""
while [ "$PWD" != "/" ] && [ ! -f build_libs ]; do
    RELDIR="$(basename "$PWD")/$RELDIR"
    cd ..
done
if [ "$PWD" = "/" ]; then
    echo "bld error: can't find 'build_libs' in the current directory or any of its parents"
    exit 1
fi
if [ "$RELDIR" != "" ]; then echo "Working from $PWD, results in $PWD/_build/$RELDIR"; fi

. platform_helper.sh

msg () {
    echo "[33m$*[0m"
}
warn () {
    echo "[1m[33mwarning: $*[0m"
}

# Portability :
# Under cygwin, only tested in bytecode
# Under windows, use cygwin version (=>ENFORCE_WINDOWS="YES")
# ENFORCE_WINDOWS , if set => compile ocaml with ms compiler compatibility

WINDOWS=${WINDOWS:-"$IS_WINDOWS"}

if [ $(uname) = "windows32" ]; then
    tmp=tmp # mktemp is awkward on Windows
else
    tmp=$(mktemp -t tmp.XXXXXX)
    trap "rm -f $tmp" EXIT
fi

: ${BLDDIR:="$MLSTATELIBS"/share/opa/bld}

: ${CONFIG_ML:=config.ml}
if [ ! -e $CONFIG_ML ]; then
    echo "Error: config.ml not found. Please run ./configure"
    exit 1
fi

filter_target(){
    if [ "$WINDOWS" = 1 ]; then
        grep -v '^\(#\|$\)' "$@" | sed s/\\.o$/\\.obj/g | sed -e "s/\\.o /\\.obj /g"
    else
        grep -v '^\(#\|$\)' "$@"
    fi
}

export PPDEBUGDIR=$PWD/utils

COMMANDLINE="$*"
STARTTIME=$(date +%s)
UNINSTALL_LIBS="false"
UNINSTALL_TOOLS="false"
INSTALL_LIBS="false"
INSTALL_TOOLS="false"
TOOLS=""
LIBS=""
BYTECODE="false"
CLEAN="false"
INSTALL_CMD="install -m 0644"
INSTALL_BIN="install -m 0755"
COMPILE="true"
DEBUG="true"
NOCOMPILE="false"
ARGS=""
RENAME=""
INSTALLDIR="$MLSTATELIBS"
BUILD_DIR="_build"
BUILD_LIBS="build_libs"
BUILD_TOOLS="build_tools"

init_install_libs() (
    if [ -z "$LIBS" ]; then
        grep "^ *internal" $BUILD_LIBS > $tmp
        while read x lib y; do echo $lib; done < $tmp
    else
        echo $LIBS
    fi
)
init_install_tools() (
    if [ -z "$TOOLS" ]; then
        grep "^ *internal" $BUILD_TOOLS | filter_target > $tmp
        while read x tool y; do echo $tool; done < $tmp
    else
        echo $TOOLS
    fi
)

R=''
while [ $# -gt 0 ]; do
    case $1 in
        clean)
            CLEAN="true";;
        install-libs)
            INSTALL_LIBS="true"
            LIBS=$(init_install_libs);;
        install-tools)
            INSTALL_TOOLS="true"
            TOOLS=$(init_install_tools);;
        install)
            INSTALL_LIBS="true"; INSTALL_TOOLS="true"
            LIBS=$(init_install_libs)
            TOOLS=$(init_install_tools);;
        uninstall-libs)
            UNINSTALL_LIBS="true"
            LIBS=$(init_install_libs);;
        uninstall-tools)
            UNINSTALL_TOOLS="true"
            TOOLS=$(init_install_tools);;
        uninstall)
            UNINSTALL_LIBS="true"; UNINSTALL_TOOLS="true"
            LIBS=$(init_install_libs)
            TOOLS=$(init_install_tools);;
        libs=*)
            LIBS="$(echo $1 | sed 's/^libs=//' | sed 's/,/ /g')";;
        tools=*)
            TOOLS="$(echo $1 | sed 's/^tools=//' | sed 's/,/ /g')";;
        -private)
            BUILD_LIBS="build_libs*"
            BUILD_TOOLS="build_tools*"
            ;;
        -release)
            ARGS="$ARGS -tag release -tag noassert"
            DEBUG="false";;
        -nocompile)
            NOCOMPILE="true";;
        -bytecode)
            BYTECODE="true";;
        -dir)
           if [ -z "$2" ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift; INSTALLDIR="$1";;
        -build-dir)
            shift; BUILD_DIR="$1"; ARGS="$ARGS -build-dir $1";;
        -help|--help)
            print_help; exit 0;;
        --)
            ARGS="$ARGS $@"; break;;
        *)
            TARGET="${RELDIR}$1"
            if [ -f "$TARGET.itarget" ]; then A="$TARGET.otarget"
            elif echo $TARGET | grep -q '.exe$'; then
                N=$(find */ -name $(basename "$TARGET" .exe).ml | grep -v $BUILD_DIR)
                if [ -z "$N" ]; then echo "Target not found: $TARGET"; exit 1; fi
                if [ -n "$(echo $N | (read a b; echo $b))" ]; then
                    echo "Multiple targets found: $TARGET"; exit 1;
                fi
                if [ "$BYTECODE" = "true" ]; then
                    A="$(dirname "$N")/$(basename "$N" .ml).byte"; else
                    A="$(dirname "$N")/$(basename "$N" .ml).native"
                fi
                R="$A"
            elif echo $TARGET | grep -q '.doc$'; then
                A="${TARGET}dir/index.html"
            else
                A="$TARGET"
            fi
            ARGS="$ARGS $A"
            RENAME="$RENAME $R";;
    esac
    shift
done

INSTALL_LIBS_DIR=$INSTALLDIR/lib/opa/ocaml/opa

if [ "$CLEAN" = "true" ]; then
    rm -rf $BUILD_DIR/* || warn "could not complete clean"
fi

if [ "$DEBUG" = "true" ]; then ARGS="-tag debug $ARGS"; fi

shopt -s nullglob
# causes non-matching patterns to be discarded from the line instead of left as-is
# useful for install commands

#########
# myocamlbuild generation
#########

MYOCAMLBUILD=$BUILD_DIR/myocamlbuild.ml

mkdir -p $BUILD_DIR

libexportfiles() {
    sed 's%^\([^#/]\+\)/\([^/]\+\)$%\1 \2%; t OK; d; :OK' $1 | \
        while read dir module; do
          # Get the real capitalisation of the source file
          f=$(find -L $dir -maxdepth 1 -iname $module.ml)
              # If the source file doesn't exist (generated ?), lowercase first letter
          if [ -z "$f" ]; then f=$dir/$(echo ${module:0:1} | tr '[:upper:]' '[:lower:]')${module:1}.ml; fi
          echo $f
        done
}

if [ ! -f $MYOCAMLBUILD ] ||
    [ $MYOCAMLBUILD -ot $BLDDIR/myocamlbuild_prefix.ml ] ||
    [ $MYOCAMLBUILD -ot $BLDDIR/myocamlbuild_suffix.ml ] ||
    [ $MYOCAMLBUILD -ot $CONFIG_ML ] ||
    [ $MYOCAMLBUILD -ot $CONFIG_SH ] ||
    [ $MYOCAMLBUILD -ot $0 ] ||
    [ -n "$(find -L . -maxdepth 1 -newer $MYOCAMLBUILD -and \
             \( -name build_rules\*.ml -or -name build_tools\* -or -name build_libs\* \) )" ]
then
  {
    echo "(* ****************************************************************************** *)"
    echo "(*                    File generated by bld: DO NOT EDIT.                         *)"
    echo "(*         See build_libs*, build_tools* and build_rules*.ml instead.             *)"
    echo "(* ****************************************************************************** *)"
    echo
    echo "#1 \"$CONFIG_ML\""
    cat $CONFIG_ML
    echo "#1 \"$BLDDIR/myocamlbuild_prefix.ml\""
    cat $BLDDIR/myocamlbuild_prefix.ml
    for i in $BUILD_TOOLS; do
        echo "#1 \"$i\""
        if [ "$BYTECODE" = "true" ]; then sed "s/\.native/.byte/g" $i | filter_target> $tmp
        else cat $i | filter_target> $tmp
        fi
        awk '/^external/ { print "set_tool ~internal:false \""$2"\" \""$3"\";" }
             /^internal/ { print "set_tool ~internal:true \""$2"\" \""$3"\";" }' $tmp
    done
    for i in $BUILD_LIBS; do
        echo "#1 \"$i\""
        awk '/^external/ { print "mlstate_lib ~dir:\"lib/opa/static\" \""$2"\";" }
             /^internal/ { print "internal_lib", $3 ? "~dir:\""$3"\"" : "", "\""$2"\";" }' $i
    done
    for i in build_rules*.ml; do
        echo "#1 \"$i\""
        cat $i
        echo ";"
    done
    echo "#1 \"$BLDDIR/myocamlbuild_suffix.ml\""
    cat $BLDDIR/myocamlbuild_suffix.ml
  } > $MYOCAMLBUILD
fi


if [ ! -f $BUILD_DIR/myocamlbuild ] ||
    [ $BUILD_DIR/myocamlbuild -ot $MYOCAMLBUILD ]
then
    D=$PWD
    [ $IS_WINDOWS ] && unset OCAMLLIB # prevent windows installation to bypass cygwin one
    wh=$($OCAMLBUILD -where)
    # Why CYGWIN : because flexdll is not working with Cygwin, so no dynamic loading can occur
    if [ "$BYTECODE" = "true" ]; then
        echo Compiling myself to bytecode using $wh
        cp $CONFIG_ML ${CONFIG_ML}i $BUILD_DIR
        cd $BUILD_DIR
        ocamlc -w y -I "$wh" unix.cma ocamlbuildlib.cma config.ml myocamlbuild.ml "$wh"/ocamlbuild.cmo -o myocamlbuild
    else
        echo Compiling myself using $wh
        rm -f _build/config.*
        cp $CONFIG_ML ${CONFIG_ML}i $BUILD_DIR
        cd $BUILD_DIR
        $OCAMLOPT -c config.mli
        $OCAMLOPT -c config.ml
        $OCAMLOPT -w y -I "$wh" unix.cmxa ocamlbuildlib.cmxa config.cmx myocamlbuild.ml "$wh"/ocamlbuild.cmx -o myocamlbuild
    fi
    cd $D
fi

#########
# functions handling libs
#########

lib_files () {
    local lib=$1; shift
    local dir=$1; shift
    local pfx=$1; shift
    [ $# -eq 0 ]
    FILES="$lib.$EXT_LIB $lib.cmxa"
    if [ "$BYTECODE" = "true" ]; then FILES="$lib.cma"; fi
    for i in $pfx/$dir/*.cmi; do
        local module=$(basename "$i" .cmi)
        if grep -qi '^\([^#].* \)\? *'$dir/$module'\( \|$\)' $lib.mllib; then
            FILES="$FILES $dir/$module.cmi"
        fi
    done
    echo $FILES
}

install_lib () {
    local lib=$1; shift
    local srcdir=${1:-$lib}
    [ $# -le 1 ]
    FILES=$(lib_files $lib $srcdir $BUILD_DIR)
    echo "$lib >> $FILES"
    echo -n -e "Installing into $INSTALL_LIBS_DIR[K\r"
    for i in $FILES; do
        echo "Installing $i"
        if [ -e "$INSTALL_LIBS_DIR/$(basename $i)" ]; then
            if diff $BUILD_DIR/$i $INSTALL_LIBS_DIR/$(basename $i); then
                echo "Trying to install $i to $INSTALL_LIBS_DIR/$(basename $i), which is already there. Skipping."
            else
                echo "Error: would install file $i from lib $lib, which conflicts with"
                echo "an already installed file ($INSTALL_LIBS_DIR/$(basename $i))"
                exit 1
            fi
        else
            $INSTALL_CMD $BUILD_DIR/$i $INSTALL_LIBS_DIR
        fi
    done
}

#########
# Setup notification
#########

# $1: delay in seconds, '92' for instance
# stdout: '1m32s', for instance
pretty_time() {
    local seconds=$(($1%60))
    local minutes=$(($1/60%60))
    local hours=$(($1/3600%24))
    local days=$(($1/(3600*24)))
    local once=false
    { [ $days -ne  0 ] || $once; } && printf "%ddays " $days && once=true
    { [ $hours -ne  0 ] || $once; } && printf "%dh" $hours && once=true
    { [ $minutes -ne 0 ] || $once; } && printf "%dm" $minutes && once=true
    { [ $seconds -ne 0 ] || $once; } && printf "%ds" $seconds && once=true
}

notify_success () {
    if [ $IS_MAC ] && which growlnotify &> /dev/null; then
        growlnotify -t "$1" -m "$2"
    elif [ $IS_LINUX ] && which notify-send &> /dev/null; then
        notify-send -t 4000 "$1" "$2"
    fi
}

notify_error () {
    if [ $IS_MAC ] && which growlnotify &> /dev/null; then
        growlnotify -t "$1" -m "$2"
    elif [ $IS_LINUX ] && which notify-send &> /dev/null; then
        notify-send -t 4000 -c ERROR "$1" "$2"
    fi
}

notify_exit () {
    local status=$?
    local delay=$(($(date +%s) - $STARTTIME))
    if [ $status -ne 0 ]; then
        notify_error "[!] build unsucessful [!]" "bld $COMMANDLINE failed in $(pretty_time $delay)" || beep || true
    else
        notify_success "build finished" "bld $COMMANDLINE finished in $(pretty_time $delay)" || beep || true
    fi
}
if [ "${BLD_NOTIFY:-""}" = 1 ]; then
    trap notify_exit 0
fi

#########
# Processing targets
#########

if [ "$UNINSTALL_LIBS" = "true" ]; then
    rm -f $INSTALL_LIBS_DIR/*.{cmi,cma,cmxa,a}
fi


if [ "$UNINSTALL_TOOLS" = "true" ]; then
    grep "^internal" $BUILD_TOOLS | filter_target > $tmp
    while read x tool file dir ; do
        if [ -z "$dir" ]; then dir="bin"; fi
        rm -f $INSTALLDIR/$dir/$tool
    done < $tmp
fi

TARGETSFILE=bld_target.itarget
if [ "$NOCOMPILE" = "false" ]; then
    echo "# file generated by bld, DO NOT EDIT" >$TARGETSFILE
    ARGS="${TARGETSFILE%.itarget}.otarget $ARGS"
    for i in $LIBS; do
        echo $i.cma >>$TARGETSFILE
        echo $i.cmxa >>$TARGETSFILE
    done

    for i in $TOOLS; do
        FILE=$(cat $BUILD_TOOLS | filter_target | grep "^internal *$i\( \|$\)" | (read _ tool file dir; echo $file))
        if [ "$BYTECODE" = "true" ]; then FILE=${FILE/%.native/.byte}; FILE=${FILE/%.cmxs/.cma}; fi
        if [ -z "$FILE" ]; then
            cat $BUILD_TOOLS | filter_target
            echo "Tool not found: $i. Make sure it is declared in your $BUILD_TOOLS file"; exit 1
        else
            echo $FILE >>$TARGETSFILE
        fi
    done
fi

if [ -n "$ARGS" ]; then
    msg "bld: ocamlbuild $ARGS"
    $BUILD_DIR/myocamlbuild -no-plugin -j 6 $ARGS
fi

if [ -n "$RENAME" ]; then
    for i in $RENAME; do
        DST=$(dirname $i)/$(basename "$(basename "$i" .native)" .byte).exe
        cp $BUILD_DIR/$i $BUILD_DIR/$DST
        if [ ! -f $DST ]; then
            ln -sf $BUILD_DIR/$DST .
        else
            warn "Not linking $BUILD_DIR/$DST: file exists in current directory"
        fi
    done
fi

if [ "$INSTALL_LIBS" = "true" ]; then
    # First remove all that we are going to install, so that we can check for collisions
    if [ "$UNINSTALL_LIBS" != "true" ]; then
        for lib in $LIBS; do
            grep "^internal *$lib\\( \\|$\\)" $BUILD_LIBS > $tmp
            read x lib srcdir < $tmp;
            if [ -z "$srcdir" ]; then srcdir=$lib; fi
            FILES=$(lib_files $lib $srcdir $BUILD_DIR)
            for i in $FILES; do
                rm -f $INSTALL_LIBS_DIR/$(basename "$i")
            done
        done
    fi

    # Installs the internal libs found in the "build_libs*" file
    mkdir -p $INSTALL_LIBS_DIR
    for i in $LIBS; do
        grep "^internal *$i\\( \\|$\\)" $BUILD_LIBS > $tmp
        read x lib srcdir < $tmp;
        install_lib $lib $srcdir || warn "$lib not found, not installing"
    done
fi

if [ "$INSTALL_TOOLS" = "true" ]; then
    # Installs the internal tools found in the "build_tools*" file
    for i in $TOOLS; do
        cat $BUILD_TOOLS | filter_target | grep "^ *internal *$i\\( \\|$\\)" > $tmp
        read x tool file dir < $tmp
        if [ "$BYTECODE" = "true" ]; then file=${file/%.native/.byte}; fi
        inst="$INSTALL_CMD"
        if [ -z "$dir" ]; then dir="bin"; inst="$INSTALL_BIN"; fi
        echo -n -e "Installing into $INSTALLDIR/$dir[K\r"
        mkdir -p $INSTALLDIR/$dir
        if [ -e $BUILD_DIR/$file ]; then
            $inst $BUILD_DIR/$file $INSTALLDIR/$dir/$tool || warn "Error installing $tool"
        else
            warn "$tool not found, not installing"
        fi
    done
fi
