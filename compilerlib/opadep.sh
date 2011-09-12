#!/bin/bash

# This script is used for generating opa files and packages dependency graphs
# It is meant to be called from any directory containing some .opx/ directories
# it also looks for dot files in any found opx directory (MLSTATELIBS + current directory)
# and call odeplink for adding hyper links.
# it generates a directory _opadep/ in the current directory.

OPADEP=_opadep

# subdirectory of packages
# <!> Keep synchronized with compiler/installDir
OPA_PACKAGES='lib/opa/stdlib'

help() {
    cat <<EOF
opadep.sh: generate OPA packages dependency graphs.
Packages are expected to be found in the following folders :
  .                            all packages previously compiled in the current path
  MLSTATELIBS/$OPA_PACKAGES    all installed packages

it generates a folder $OPADEP/ containing the resulting graphs.

Options:
    --help|-h|help      displays this help
    -o <directory>      specify a custom directory instead of $OPADEP
    -I <directory>      add directory to include packages
    -n                  do not compute dependencies, assume that the index was already generated
    -d                  add some log for debug

EOF
}

COMPUTE=true
DEBUG=false
INCLUDES=''
OPAINCLUDES=''

while [ "$#" -gt 0 ]; do
    case "$1" in
      # h
        -h|--help|help)
            help
            exit 0
            ;;

        -d)
            DEBUG=true
            ;;

        -o)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            OPADEP="$1"
            ;;

        -I)
            if [ -z "$2" ]; then warn "Error: option $1 requires an argument"; exit 1; fi
            shift
            INCLUDES="$INCLUDES $1"
            OPAINCLUDES="$OPAINCLUDES -I $1"
            ;;

        -n)
            COMPUTE=false
            ;;

        *)
            echo "[!] error: unknown option $1"
            help
            exit 1
    esac
    shift
done


if [ "$COMPUTE" = true ] ; then
    tmp=$(mktemp /tmp/fileXXXXX)
    rm $tmp
    all="$tmp".opa

    cat > $all <<EOF
import *
EOF

    echo "odep: Global Dependencies Graph"
    opa --verbose-build $OPAINCLUDES --odep $all
fi

echo "creating directory $OPADEP"
mkdir -p $OPADEP

cp *.dot $OPADEP/.

echo "odep: Get Subgraphs From Packages"
ALLOPX=''
LOCATIONS="$MLSTATELIBS/$OPA_PACKAGES . $INCLUDES"

for loc in $LOCATIONS ; do
    if [ "$DEBUG" = true ] ; then
        echo "checking for opx in loc: $loc"
    fi
    opx=$(find $loc -maxdepth 1 -name '*.opx')
    if [ "$DEBUG" = true ] ; then
        echo "finding: $opx"
    fi
    ALLOPX="$ALLOPX $opx"
done

for opx in $ALLOPX ; do
    if [ "$DEBUG" = true ] ; then
        echo "checking for dot in $opx"
        ls $opx/*.dot
    else
        ls $opx/*.dot 1> /dev/null 2> /dev/null
    fi
    code=$?
    if [ "$code" = 0 ] ; then
        if [ "$DEBUG" = true ] ; then
            cp -v $opx/*.dot $OPADEP/.
        else
            cp $opx/*.dot $OPADEP/.
        fi
    fi
done

cd $OPADEP
ROOT=$(pwd)

echo "opadep: Transitive Reduction"
mkdir -p full
mkdir -p tred
for dot in *.dot ; do
    # transitive reduction
    tred $dot > tred/$dot 2> /dev/null
    mv $dot full/$dot
done

echo "opadep: DOT to SVG (+odeplink)"
cd full
for dot in *.dot ; do
    base=$(basename $dot .dot)
    dot -Tsvg $dot > $base.svg
done
odeplink *.svg

cd $ROOT
cd tred
for dot in *.dot ; do
    base=$(basename $dot .dot)
    dot -Tsvg $dot > $base.svg
done
odeplink *.svg

echo "opadep: Successful generation of graphs in $OPADEP/"
