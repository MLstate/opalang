#!/usr/bin/env bash

set -e
set -u

. tools/platform_helper.sh

# these must be absolute
PREFIX=/usr
INSTALLDIR=$PWD/release_install_root

NODOC="true" # build the book
NOMAN="false" # build manpages
NOOCAML="false" # build and embed ocaml backend

# the version string will be MAJORNAME+BUILDnnnn with nnnn the build number
# (MAJORNAME if BUILD is empty)

msg () {
    case "$1" in
        -n) shift; echo -n "[32m$*[0m";;
        *) echo "[32m$*[0m";;
    esac
}

MYDIR=$PWD

OPALANG=$MYDIR

CLEAN="true"
KEEP_INSTALL_SYS="false"

help() {
    echo "Installs a stripped stand-alone (including ocaml) version of Opa in the system."
    echo "The installed system can then be used for building packages."
    echo "Options"
    echo "	-prefix <dir>			Build for install in <dir>. Warning, you may need to recompile"
    echo "					ocaml if you change this (default $PREFIX)"
    echo "	-dir <dir>			Install to <dir> (default $INSTALLDIR)"
    echo "	-srcdir <dir>			Your opalang directory (default current dir)"
    echo "	-keep-install-sys		Don't rebuild ocaml and libs, assume it has already been done"
    echo "	-keep-build			Don't cleanup your _build. Only use if you're sure it was"
    echo "					made with the right version of ocaml (the one this script"
    echo "					builds)"
    echo "	-fetch-git [branch|tag]	Instead of setting a source directory, you can select this"
    echo "					option to freshly clone the sources at branch/tag"
    echo "					(default remotes/origin/master)"
    echo "	-license <prefix>		Use given license (files <prefix>_EN, <prefix>_FR)"
    echo "	-no-ocaml			Do not build and install OCaml"
    echo
    echo "GUIDELINES: Run from a proper opalang or with the fetch-git option."
    echo "In the former case, you'll need an opa-doc repo along your opalang."
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
            OPALANG="$MYDIR/git-masters/opageneral"
            cd $OPALANG && ./init.sh --link >/dev/null
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
            OPALANG="$1";;
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

if [ ${INSTALLDIR:0:1} != "/" ] || [ ${PREFIX:0:1} != "/" ]; then
    msg "Error: your prefix or install directory is not absolute"
    exit 1
fi

if [ "" != "$(diff $0 $OPALANG/tools/utils/$(basename $0))" ]; then
    echo "[31mWARNING[0m: current version of $(basename $0) different from the one in the repo you're making the distribution from."
fi

if [ "$CLEAN" = "true" ] && [ -e _build ]; then
    msg "Cleaning up your _build..."
    rm -rf _build/*
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

MLSTATEVARS=$(set | grep -a '^MLSTATE\|^DBGEN\|^OPA' | grep -v "^MLSTATELIBS=\|^OPALANG=\|^OPA_SOURCE_DIR=\|^OPAPRIVATE=" || true )

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

INSTALLDIR_LIBOPA=$INSTALLDIR/lib/opa
# if [ -n "$IS_MAC" ] ; then
#     INSTALLDIR_LIBOPA=$INSTALLDIR
# fi


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
        rm -rf $OPALANG/ocaml/build
        . tools/platform_helper.sh
        if [ -n "$IS_MAC" ] ; then
            $OPALANG/tools/dependencies/installation_helper.sh --prefix $INSTALLDIR
        else
            $OPALANG/tools/dependencies/installation_helper.sh --prefix $INSTALLDIR \
		--installdir $INSTALLDIR
        fi
	if [ -n "$IS_MAC" ] ; then
            export OCAMLLIB=$INSTALLDIR/lib/ocaml
            export PATH=$INSTALLDIR/bin:$PATH
	else
            export OCAMLLIB=$INSTALLDIR/lib/ocaml
            export PATH=$INSTALLDIR/bin:$PATH
	fi
    fi
fi

# mlstate libs and tools (generic)
msg Installing mlstate stuff

if [ "$KEEP_INSTALL_SYS" = "false" ] && [ -z "$IS_WINDOWS" ] && [ -z "$IS_MAC" ] && [ "$(ocamlc.opt -where)" != "$INSTALLDIR/lib/ocaml" ]; then
    msg "Error: fresh installed ocaml not found (ocamlc -where returned $(ocamlc.opt -where) instead of $INSTALLDIR/lib/ocaml"
    exit 1
fi

export MLSTATELIBS=$INSTALLDIR
# in order to build opa with the ocaml freshly built
if [ "$KEEP_INSTALL_SYS" = "false" ] ; then
    if [ -n "$IS_MAC" ] ; then
	export OCAMLOPT=$INSTALLDIR/bin/ocamlopt.opt
    else
	export OCAMLOPT=$INSTALLDIR/bin/ocamlopt.opt
    fi
fi
cd $OPALANG
SRCDIR=$OPALANG
OPABOOK=$OPALANG/doc/book # the tutorial and book

# This is absolutely correct that the 2 variables are inversed, we should fix the value inside
./configure -prefix $INSTALLDIR -libdir $PREFIX -release -no-dbm


TARGETS="distrib"
if [ $NOMAN = "false" ]; then
    TARGETS="$TARGETS manpages"
fi

if [ $NOOCAML = "false" ]; then
    make clean $TARGETS install
else
    make clean $TARGETS install-node
fi

mkdir -p $INSTALLDIR/share/opa/
mkdir -p $INSTALLDIR/share/doc/opa/

# make packages-api
# generating the book
# if [ $NODOC = "false" ]; then
#    if ! make book-clean book; then
#        msg "Error: could not build the doc in $OPABOOK."
#        msg "You may want to fix and re-run with -keep-install-sys -keep-build"
#        exit 1
#    fi
# fi
# installing the book
if [ $NODOC = "false" ] && [ -z "$IS_WINDOWS" ]; then
    mkdir -p $INSTALLDIR/share/doc/opa/book
    cp -r $OPABOOK/* $INSTALLDIR/share/doc/opa/book
fi

$SRCDIR/tools/utils/install.sh --uninstall --dir $INSTALLDIR
install -m 0755 -v $SRCDIR/tools/utils/install.sh $INSTALLDIR/share/opa
mkdir -p $INSTALLDIR/share/opa/emacs
install -m 0644 -v $SRCDIR/tools/editors/emacs/{opa-mode.el,opa-js-mode.el,site-start.el} $INSTALLDIR/share/opa/emacs
mkdir -p $INSTALLDIR/share/opa/vim
cp -r $SRCDIR/tools/editors/vim/* $INSTALLDIR/share/opa/vim/

# installing changelog (follows standard conventions)
gzip -c -9 CHANGELOG > $INSTALLDIR/share/doc/opa/changelog.gz

# installing copyright
if ! grep -q "Copyright.*\<$(date +%Y)\>" doc/copyright/copyright; then
    echo "[31mWARNING[0m: copyright doesn't appear to be up-to-date (doesn't mention current year)"
fi
# Careful: copyright file mentions that it must be along <AGPL> and <other_licenses>
# (which _must_ both be included for legal reasons)
install -m 0644 -v $SRCDIR/doc/copyright/copyright $INSTALLDIR/share/doc/opa/
{
    echo "OPA includes parts of the following software, with the given licenses:"
    echo
    for f in $SRCDIR/doc/copyright/[0-9][0-9]_*; do
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

# Cleaning up:
msg Removing unneeded ocaml executables

# ocaml
rm -rvf $INSTALLDIR/man/man*

# leave only ocamlc.opt, ocamlopt.opt
for i in {camlp4*,mkcamlp4,ocamlbuild.byte,ocamlcp,ocamldoc,ocamlmklib*,ocamlprof,ocaml,ocamlbuild.native,ocamldebug,ocamldoc.opt,ocamlmktop,ocamlrun,ocamlbrowser,ocamlc,ocamldep,ocamllex,ocamlopt,ocamlyacc,labltk,ocamlbuild,ocamldep.opt,ocamllex.opt,ocamlc.opt,ocamlobjinfo,ocamlfind,findlib,safe_camlp4,camlidl} ; do
        echo "    --  Removing $INSTALLDIR/bin/$i"
        rm -fv $INSTALLDIR/bin/$i
done

rm -rvf $INSTALLDIR/etc

for i in {findlib,etc,camlp4,labltk,ocamldoc,objinfo_helper,toplevellib.cma,addlabels,camlheader,expunge,extract_crc,scrapelabels}; do
        echo "    --  Removing $INSTALLDIR/lib/ocaml/$i"
        rm -rvf $INSTALLDIR/lib/ocaml/$i
done

rm -rvf $INSTALLDIR/lib/ocaml/mascot
rm -fv $INSTALLDIR/bin/mascot.*

if [ -z "$IS_MAC" ] ; then
    msg "Removing any source, bytecode, headers or other compilation artefacts"
    find $INSTALLDIR \( -name '*.ml' -or -name '*.mli' -or -name '*.cma' -or -name '*.cmo' -or -name '*.p.a' -or -name '*.p.cm*' -or -name '*.h' -or -name 'HEAD' -or -name 'META' -or \( -type d -empty \) \) | grep -v $INSTALLDIR/share/doc/opa/book | while read line ; do if [ -f $line ] ; then rm $line ; fi ; if [ -d $line ] ; then rmdir $line ; fi ; done

fi

if [ $NOOCAML = "true" ]; then
    echo "    --  Removing $INSTALLDIR/bin/ocamlopt.opt"
    rm -fv $INSTALLDIR/bin/ocamlopt.opt
    echo "    --  Removing $INSTALLDIR/lib/ocaml"
    rm -rfv $INSTALLDIR/lib/ocaml
    echo "    --  Removing $INSTALLDIR_LIBOPA/stdlib/*.opp/*ML*"
    rm -fv $INSTALLDIR_LIBOPA/stdlib/*.opp/*ML*
    echo "    --  Removing $INSTALLDIR_LIBOPA/static/*.{cmi,cmxa,a,ml,cmx,o}"
    rm -rfv $INSTALLDIR_LIBOPA/static/*.{cmi,cmxa,a,ml,cmx,o}
fi

msg "Cleaning RPATH of binaries and shared libraries"
if ! which chrpath &> /dev/null; then
    msg "WARNING: chrpath not found, you'll have dirty stuff in your libs/binaries"
    sleep 10
else
    find $INSTALLDIR \( -name '*.so' -or \( -executable ! -type d \) \) -exec chrpath -k -d {} + 2>/dev/null || true
fi

msg "Cleaning bin"
for i in {bslbrowser,filepos,gen_opa_manpage,gen_opatop_manpage,genman,genproto,jsstat,mlidl,mlstate_platform,odep,odeplink,ofile,opa-db-server,opa-db-tool,opa-translate,opa2opa,opadep,opatop,opatrack,passdesign,ppdebug,ppdebug-opa,ppjs,qmljs,trx,trx_interpreter,wsdl2ml} ; do
    echo "    --  Removing $INSTALLDIR/bin/$i"
    rm -fv $INSTALLDIR/bin/$i
done

msg Stripping and upx-ing

upxf(){
    echo "    -- UPX-ing $1"
    if [ -n "$IS_WINDOWS" ];
    then
	FILETOMOD="$(cygpath -w $1)"
	editbin /NOLOGO /STACK:10000000,10000000 "$FILETOMOD" # bigger stacksize because ocamlopt.opt needs it
	/windows_libs/utils/upx "$FILETOMOD"
	editbin /NOLOGO /STACK:10000000,10000000 "$FILETOMOD" # in case upx malfunctions
    elif [ -n "$IS_MAC" ];
    then
	echo "No UPX on Mac for the moment, might bug..."
	#upx $1 || true
    else
	echo "No UPX on linux for the moment, BUGGY"
	#upx $1 || true
    fi
}

stripf(){
    echo "    -- Striping $1"
    if [ -n "$IS_WINDOWS" ];
    then
	echo No strip on windows
    elif [ -n "$IS_MAC" ];
    then
	strip $1 || true
    else
	strip $1 || true
    fi
}

for i in $INSTALLDIR/bin/* $INSTALLDIR_LIBOPA/bin/*; do
    # Strip BEFORE upx, otherwise the exe is DESTROYED
    stripf $i
    upxf $i
done

## not some on stripf there ?
find $INSTALLDIR -name '*.cmxs' -exec strip {} \;
find $INSTALLDIR -name '*.so' -exec strip {} \;

msg "Installation in $INSTALLDIR finished."
