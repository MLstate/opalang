#!/usr/bin/env sh

set -u
set -e

help() {
    echo "Opa installation script: creates wrapper scripts to call opa and tools"
    echo "with an environment setting their correct working directories"
    echo "Options:"
    echo "	--dir <dir>		the OPA installation prefix to work in"
    echo "				(by default, the current directory)"
    echo "	--help			show this help"
    echo "	--link <dir>		also install links to the scripts in the given directory"
    echo "	--no-ocaml		don't force the use of a specific ocaml"
    echo "	--ocaml-prefix		what ocaml prefix to use (default <prefix>/opa)"
    echo "	--ocamllib		ocaml libraries directory (default <ocaml-prefix>/lib/ocaml)"
    echo "	--ocamlopt		ocaml native compiler to use (default <ocaml-prefix>/bin/ocamlopt.opt)"
    echo "	--prefix <dir>		make wrappers for the given installation prefix"
    echo "				(by default, the value of --dir)"
    echo "	--quiet			be quiet"
    echo "	--uninstall		remove installed scripts"
    echo "	--uninstall-wrappers	remove installed wrapper scripts"
}

INSTALLDIR=$PWD
LINKDIR=""
UNINSTALL=false
UNINSTALL_WRAPPERS=false
NO_OCAML=false
QUIET=false

while [ $# -gt 0 ]; do
    case $1 in
        --dir)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            INSTALLDIR=$1
            ;;
        --prefix)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            PREFIX=$1
            ;;
        --ocaml-prefix)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            OCAML_PREFIX=$1
            ;;
        --ocamllib)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            OCAMLLIB=$1
            ;;
        --ocamlopt)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            OCAMLOPT=$1
            ;;
        --link)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            LINKDIR=$1
            ;;
        --quiet)
            QUIET=true
            ;;
        --uninstall)
            UNINSTALL=true
            ;;
        --uninstall-wrappers)
            UNINSTALL_WRAPPERS=true
            ;;
        --no-ocaml)
            NO_OCAML=true
            ;;
        --help)
            help
            exit 0
            ;;
        *)
            echo "Error: unknown option $1"
            help
            exit 1
    esac
    shift
done

: ${PREFIX:=$INSTALLDIR}
: ${OCAML_PREFIX:=$PREFIX/lib/opa/ocaml}
: ${STATIC_PREFIX:=$PREFIX/lib/opa/static}
: ${STDLIB_PREFIX:=$PREFIX/lib/opa/stdlib}
: ${STDLIB_QMLJS_PREFIX:=$PREFIX/lib/opa/stdlib/stdlib.qmljs}

# create_wrapper basefile exename
create_wrapper() {
    local source="$1"; shift
    local name="$1"; shift
    [ $# -eq 0 ]
    local wrapper=$INSTALLDIR/bin/$name

    if [ $UNINSTALL = true ] || [ $UNINSTALL_WRAPPERS = true ]; then
        [ $QUIET = true ] || echo "Removing $wrapper"
        rm -f $wrapper
        if [ -n "$LINKDIR" ]; then
            rm -f $LINKDIR/$name
        fi
    else
        [ $QUIET = true ] || echo "Creating $wrapper"
        mkdir -p $(dirname $wrapper)
        {
            echo '#!/usr/bin/env sh'
            echo "export MLSTATELIBS=$PREFIX"
            if [ $NO_OCAML = false ]; then
                echo "export OCAMLLIB=${OCAMLLIB:-$OCAML_PREFIX/lib/ocaml}"
                echo "export OCAMLOPT=${OCAMLOPT:-$OCAML_PREFIX/bin/ocamlopt.opt}"
                # may need to rewrite $PREFIX/lib/opa/ocaml/lib/ld.conf if ocaml is relocated
            fi
            if [ $name = "opa" ]; then
                echo '
case "$1" in
    create)
        shift
        if [ -n "$1" ] && ! [ "$1" = "--help" ] && ! [ "$1" = "--name" ]; then
          OPT="--name $1"
        else
          OPT="$1"
        fi
        shift
        export NODE_PATH="$NODE_PATH:'$STATIC_PREFIX':'$STDLIB_PREFIX':'$STDLIB_QMLJS_PREFIX'"
        $MLSTATELIBS/bin/opa-create $OPT $@
        ;;
    export)
        shift
        $MLSTATELIBS/lib/opa/bin/opx2js-bin $@
        ;;
    *)
        exec '$source' "$@"
esac
'
            else
                echo 'exec '$source' "$@"'
            fi
        } > $wrapper
        chmod 755 $wrapper
        if [ -n "$LINKDIR" ]; then
            ln -fs $wrapper $LINKDIR/
        fi
    fi
}

# creating wrapper script 'opa'
create_wrapper $PREFIX/lib/opa/bin/opa-bin opa

# creating wrapper script 'opa-plugin-builder'
create_wrapper $PREFIX/lib/opa/bin/opa-plugin-builder-bin opa-plugin-builder

# creating wrapper script 'opa-plugin-browser'
create_wrapper $PREFIX/lib/opa/bin/opa-plugin-browser-bin opa-plugin-browser

if [ $UNINSTALL = true ]; then
    opa_create=$INSTALLDIR/bin/opa-create
    [ $QUIET = true ] || echo "Removing $opa_create"
    rm -f $opa_create
fi
