#!/usr/bin/env bash

# Include in your scripts to detect the host platform and have appropriate
# aliases set for some commands (eg use the GNU versions on a mac)
# Example:
# $ . platform_helper.sh
# $ if [ "$IS_LINUX" ]; then echo "I am Linux"; fi

IS_LINUX=""
IS_MAC=""
IS_WINDOWS=""

case $(uname) in
	MINGW*) IS_WINDOWS=1;;
    CYGWIN*) IS_WINDOWS=1;;
    Darwin*) IS_MAC=1;;
    Linux*|GNU/kFreeBSD) IS_LINUX=1;;
    FreeBSD) IS_FREEBSD=1;;
    *)
        echo "Error: could not detect OS. Defaulting to Linux" >&2
        IS_LINUX=1
esac

# Defining aliases for a few commands

if [ -n "$IS_MAC" ]; then
    SED=gsed
    TAIL=gtail
    MKTEMP=gmktemp
    SORT=gsort
    READLINK=greadlink
else
    SED=sed
    TAIL=tail
    MKTEMP=mktemp
    SORT=sort
    READLINK=readlink
fi

tail() { command "$TAIL" "$@"; }
mktemp() { command "$MKTEMP" "$@"; }
sed() { command "$SED" "$@"; }
sort() { command "$SORT" "$@"; }
readlink() { command "$READLINK" "$@"; }
