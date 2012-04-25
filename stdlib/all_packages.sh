#!/usr/bin/env bash
# script used to generate the list of packages

DIRS=$(find . -type d | sed "s/.\///")

: ${PACKAGE_FILTER:=grep -E -v -f node.todo}

PACKAGES=''
for dir in $DIRS ; do
    files=$(find $dir -maxdepth 1 -name '*.opa')
    if [ -n "$files" ] ; then
        this=$(echo $dir | sed 's/\//./g')
        echo "stdlib.$this"
    fi
done | sort -u | $PACKAGE_FILTER
