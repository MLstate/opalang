#!/usr/bin/env bash

set -e
set -u

. ./platform_helper.sh

# these must be absolute
PREFIX=/usr
INSTALLDIR=$PWD/release_install_root

NODOC="false"
NOMAN="false"

# the version string will be MAJORNAME+BUILDnnnn with nnnn the build number
# (MAJORNAME if BUILD is empty)

msg () {
    case "$1" in
        -n) shift; echo -n "[32m$*[0m";;
        *) echo "[32m$*[0m";;
    esac
}

MYDIR=$PWD

OPAGENERAL=$MYDIR

CLEAN=true
KEEP_INSTALL_SYS="false"

help() {
    echo "Installs a stripped stand-alone (including ocaml) version of OPA in the system."
    echo "The installed system can then be used for building packages."
    echo "Options"
    echo "	-prefix <dir>			Build for install in <dir>. Warning, you may need to recompile"
    echo "					ocaml if you change this (default $PREFIX)"
    echo "	-dir <dir>			Install to <dir> (default $INSTALLDIR)"
    echo "	-srcdir <dir>			Your opageneral directory (default current dir)"
    echo "	-keep-install-sys		Don't rebuild ocaml and libs, assume it has already been done"
    echo "	-keep-build			Don't cleanup your _build. Only use if you're sure it was"
    echo "					made with the right version of ocaml (the one this script"
    echo "					builds)"
    echo "	-fetch-git [branch|tag]	Instead of setting a source directory, you can select this"
    echo "					option to freshly clone the sources at branch/tag"
    echo "					(default remotes/origin/master)"
    echo "	-license <prefix>		Use given license (files <prefix>_EN, <prefix>_FR)"
    echo
    echo "GUIDELINES: Run from a proper opageneral or with the fetch-git option."
    echo "In the former case, you'll need an opa-doc repo along your opageneral."
    echo "you could have a syntax error in ocaml-doc, when building, so you can remplace the makefile by empty rule, because it not needed"
}

while [ $# -gt 0 ]; do
    case $1 in
        -no-doc)
            NODOC="true";;
        -no-man)
            NOMAN="true";;
        -force-doc)
            NODOC="false";;
        -fetch-git)
            BRANCH=remotes/origin/master
            if echo $2 | grep -q "^[^-]"; then shift; BRANCH=$1; fi
            cd $MYDIR
            mkdir -p git-masters
            for d in opa-doc reftester; do
                if [ ! -d "$MYDIR/git-masters/$d" ]; then
                    cd "$MYDIR/git-masters"; git clone -n git@ovh4:$d.git;
                fi
                cd "$MYDIR/git-masters/$d"; git fetch; git checkout $BRANCH
            done
            OPAGENERAL="$MYDIR/git-masters/opageneral"
            cd $OPAGENERAL && ./init.sh --link >/dev/null
            CMDLINE="$CMDLINE -srcdir $MYDIR/git-masters/opageneral"
            cd $MYDIR;;
        -keep-build)
            CLEAN="false";;
        -keep-install-sys)
            KEEP_INSTALL_SYS="true";;
        -prefix)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            PREFIX="$1";;
        -dir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            INSTALLDIR="$1";;
        -srcdir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            OPAGENERAL="$1";;
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

if [ ${INSTALLDIR:0:1} != "/" ] || [ ${PREFIX:0:1} != "/" ]; then
    msg "Error: your prefix or install directory is not absolute"
    exit 1
fi

if [ "" != "$(diff $0 $OPAGENERAL/$(basename $0))" ]; then
    echo "[31mWARNING[0m: current version of $(basename $0) different from the one in the repo you're making the distribution from."
fi

if [ "$CLEAN" = "true" ] && [ -e _build ]; then
    msg "Cleaning up your _build..."
    rm -rf _build
fi

if [ "$KEEP_INSTALL_SYS" = "false" ] ; then
# Start fresh
if ! [ $(ls $INSTALLDIR 2>/dev/null |wc -l) -eq 0 ]; then # checks if dir empty
    msg -n "Warning, install directory $INSTALLDIR exists. Cleanup ? (no will abort) [Yn] "
    read yesno
    case "$yesno" in
        n|no|non) msg Not installing to a dirty directory; exit 1;;
        bypass) msg "You seem to know what you're doing...";;
        *) msg Cleaning $INSTALLDIR;
            if [ -z $INSTALLDIR ]; then
                msg 'Bailing out!' ;exit 1 ;
            else
                rm -rf $INSTALLDIR/* ;
            fi
    esac
fi
fi

MLSTATEVARS=$(set | grep -a '^MLSTATE\|^DBGEN\|^OPA' | grep -v "^MLSTATELIBS=\|^OPAGENERAL=\|^OPA_SOURCE_DIR=\|^OPAPRIVATE=" || true )

if [ -n "$MLSTATEVARS" ]; then
    msg "Warning: mlstate env vars detected. Please make sure the following won't"
    msg "have any consequence on the build."
    echo $MLSTATEVARS
    msg -n "Continue ? [Yn] "
    read yesno
    case "$yesno" in
        n|no|non) msg Aborting; exit 1;;
    esac
fi

mkdir -p $INSTALLDIR

INSTALLDIR_LIBOPAOCAML=$INSTALLDIR/lib/opa/ocaml
if [ -n "$IS_MAC" ] ; then
    INSTALLDIR_LIBOPAOCAML=$INSTALLDIR
fi


# Install (our) ocaml
if [ -n "$IS_WINDOWS" ]; then
    msg Local copy of ocaml HACK TEMPORAIRE for debug
    WINOCAML=/cygdrive/c/ocamlms
    mkdir -p $INSTALLDIR/lib/ocaml
    mkdir -p $INSTALLDIR/bin
    mkdir -p $INSTALLDIR/bin/ocaml
    cp -ur $WINOCAML/lib/* $INSTALLDIR/lib/ocaml/
    cp -ur $WINOCAML/bin/* $INSTALLDIR/bin/
else
    msg Installing ocaml and other dependencies
    if [ "$KEEP_INSTALL_SYS" = "false" ] ; then
        rm -rf $OPAGENERAL/ocaml/build
        . platform_helper.sh
        if [ -n "$IS_MAC" ] ; then
            $OPAGENERAL/dependencies/installation_helper.sh --prefix $INSTALLDIR
        else
            $OPAGENERAL/dependencies/installation_helper.sh --prefix $PREFIX/lib/opa/ocaml --installdir $INSTALLDIR_LIBOPAOCAML
        fi
	if [ -n "$IS_MAC" ] ; then
            export OCAMLLIB=$INSTALLDIR/lib/ocaml
            export PATH=$INSTALLDIR/bin:$PATH
	else
            export OCAMLLIB=$INSTALLDIR_LIBOPAOCAML/lib/ocaml
            export PATH=$INSTALLDIR_LIBOPAOCAML/bin:$PATH
	fi
    fi
fi

# mlstate libs and tools (generic)
msg Installing mlstate stuff

if [ "$KEEP_INSTALL_SYS" = "false" ] && [ -z "$IS_WINDOWS" ] && [ -z "$IS_MAC" ] && [ "$(ocamlc.opt -where)" != "$INSTALLDIR_LIBOPAOCAML/lib/ocaml" ]; then
    msg "Error: fresh installed ocaml not found (ocamlc -where returned $(ocamlc.opt -where) instead of $INSTALLDIR_LIBOPAOCAML/lib/ocaml"
    exit 1
fi

export MLSTATELIBS=$INSTALLDIR
# in order to build opa with the ocaml freshly built
if [ "$KEEP_INSTALL_SYS" = "false" ] ; then
    if [ -n "$IS_MAC" ] ; then
	export OCAMLOPT=$INSTALLDIR/bin/ocamlopt.opt
    else
	export OCAMLOPT=$INSTALLDIR_LIBOPAOCAML/bin/ocamlopt.opt
    fi
else
    OCAMLOPT=ocamlopt.opt
fi
cd $OPAGENERAL
SRCDIR=$OPAGENERAL
OPABOOK=$OPAGENERAL/doc/book # the tutorial and book
OPADOCGEN=$OPAGENERAL/_build/opadoc/doc # the generated API doc

./configure -prefix $INSTALLDIR -ocamlopt $OCAMLOPT -release -no-dbm


TARGETS="distrib"
if [ $NOMAN = "false" ]; then
    TARGETS="$TARGETS manpages"
fi

make clean $TARGETS install

mkdir -p $INSTALLDIR/share/opa/
mkdir -p $INSTALLDIR/share/doc/opa/

# generating the book
if [ $NODOC = "false" ]; then
    make opadoc/doc install-doc
    if ! make book-clean book; then
        msg "Error: could not build the doc in $OPABOOK."
        msg "You may want to fix and re-run with -keep-install-sys -keep-build"
        exit 1
    fi
fi
# installing the book
if [ $NODOC = "false" ] && [ -z "$IS_WINDOWS" ]; then
    mkdir -p $INSTALLDIR/share/doc/opa/book
    cp -r $OPABOOK/* $INSTALLDIR/share/doc/opa/book
fi

$SRCDIR/utils/install.sh --uninstall --dir $INSTALLDIR
install -m 0755 -v $SRCDIR/utils/install.sh $INSTALLDIR/share/opa
mkdir -p $INSTALLDIR/share/opa/emacs
install -m 0644 -v $SRCDIR/utils/emacs/{opa-mode.el,opa-js-mode.el,site-start.el} $INSTALLDIR/share/opa/emacs
mkdir -p $INSTALLDIR/share/opa/vim
cp -r $SRCDIR/utils/vim/* $INSTALLDIR/share/opa/vim/

# installing changelog (follows standard conventions)
gzip -c -9 CHANGELOG > $INSTALLDIR/share/doc/opa/changelog.gz

# installing copyright
if ! grep -q "Copyright.*\<$(date +%Y)\>" copyright/copyright; then
    echo "[31mWARNING[0m: copyright doesn't appear to be up-to-date (doesn't mention current year)"
fi
# Careful: copyright file mentions that it must be along <AGPL> and <other_licenses>
# (which _must_ both be included for legal reasons)
install -m 0644 -v $SRCDIR/copyright/copyright $INSTALLDIR/share/doc/opa/
{
    echo "OPA includes parts of the following software, with the given licenses:"
    echo
    for f in $SRCDIR/copyright/[0-9][0-9]_*; do
        name=$(basename $f)
        name=${name#??_}
        echo
        echo "----------------------------------------------------------------------"
        echo "------------------------ License for $name ------------------------"
        echo
        cat $f
    done
} > $INSTALLDIR/share/doc/opa/other_licenses
chmod 644 $INSTALLDIR/share/doc/opa/other_licenses
install -m 0644 -v $SRCDIR/LICENSE $INSTALLDIR/share/doc/opa/AGPL
install -m 0644 -v $SRCDIR/RUNTIME_LICENSE $INSTALLDIR/share/doc/opa/APACHE

# Cleaning up:
msg Removing unneeded ocaml executables
# ocaml
rm -rvf $INSTALLDIR_LIBOPAOCAML/man*
# leave only ocamlc.opt, ocamlopt.opt
for i in {camlp4*,mkcamlp4,ocamlbuild.byte,ocamlcp,ocamldoc,ocamlmklib*,ocamlprof,ocaml,ocamlbuild.native,ocamldebug,ocamldoc.opt,ocamlmktop,ocamlrun,ocamlbrowser,ocamlc,ocamldep,ocamllex,ocamlopt,ocamlyacc,labltk,ocamlbuild,ocamldep.opt,ocamllex.opt,ocamlc.opt,ocamlobjinfo,ocamlfind,findlib,safe_camlp4,camlidl} ; do
        echo "    --  Removing $INSTALLDIR_LIBOPAOCAML/bin/$i"
        rm -fv $INSTALLDIR_LIBOPAOCAML/bin/$i
done
rm -rvf $INSTALLDIR_LIBOPAOCAML/etc
for i in {findlib,etc,camlp4,labltk,ocamldoc,objinfo_helper,toplevellib.cma,addlabels,camlheader,expunge,extract_crc,scrapelabels}; do
        echo "    --  Removing $INSTALLDIR_LIBOPAOCAML/lib/ocaml/$i"
        rm -rvf $INSTALLDIR_LIBOPAOCAML/lib/ocaml/$i
done
rm -rvf $INSTALLDIR/lib/ocaml/mascot
rm -fv $INSTALLDIR/bin/mascot.*

if [ -z "$IS_MAC" ] ; then
    msg "Removing any source, bytecode, headers or other compilation artefacts"
    find $INSTALLDIR \( -name '*.ml' -or -name '*.mli' -or -name '*.cma' -or -name '*.cmo' -or -name '*.p.a' -or -name '*.p.cm*' -or -name '*.h' -or -name 'HEAD' -or -name 'META' -or \( -type d -empty \) \) | grep -v $INSTALLDIR/share/doc/opa/book | while read line ; do if [ -f $line ] ; then rm $line ; fi ; if [ -d $line ] ; then rmdir $line ; fi ; done

fi

msg "Cleaning RPATH of binaries and shared libraries"
if ! which chrpath &> /dev/null; then
    msg "WARNING: chrpath not found, you'll have dirty stuff in your libs/binaries"
    sleep 10
else
    find $INSTALLDIR \( -name '*.so' -or \( -executable ! -type d \) \) -exec chrpath -k -d {} + 2>/dev/null || true
fi

msg Stripping and upx-ing
UPX=upx
STRIP=strip

upxf(){
if [ -n "$IS_WINDOWS" ];
then
    FILETOMOD="$(cygpath -w $1)"
    editbin /NOLOGO /STACK:10000000,10000000 "$FILETOMOD" # bigger stacksize because ocamlopt.opt needs it
    /windows_libs/utils/upx "$FILETOMOD"
    editbin /NOLOGO /STACK:10000000,10000000 "$FILETOMOD" # in case upx malfunctions
elif [ -n "$IS_MAC" ];
then
    echo No upx on mac
else
    upx $1 || true
fi
}


stripf(){
if [ -n "$IS_WINDOWS" ];
then
    echo No strip on windows
elif [ -n "$IS_MAC" ];
then
    # not $STRIP there ?
    strip $1 || true
else
    strip $1 || true
fi
}

for i in $INSTALLDIR/bin/* $INSTALLDIR/lib/opa/bin/*; do
    # Strip BEFORE upx, otherwise the exe is DESTROYED
    stripf $i
    # upxf $i -- upx disabled, last version seems to cause problems
done
# same previous stuff, but on mac this directory does not exists, so we put this extra test to avoid a warning
# if this is a directory (and exist)
if [ -d $INSTALLDIR_LIBOPAOCAML/bin ]; then
    for i in $INSTALLDIR_LIBOPAOCAML/bin/*; do
        # extra protection against an empty 'bin/*'
        if [ -f $i ]; then
            # here we will pass up on ocamlopt.opt, so we must be carefull to not break it
            stripf $i
        fi
    done
fi

## not some on stripf there ?
find $INSTALLDIR -name '*.cmxs' -exec $STRIP {} \;
find $INSTALLDIR -name '*.so' -exec $STRIP {} \;

msg "Installation in $INSTALLDIR finished."
