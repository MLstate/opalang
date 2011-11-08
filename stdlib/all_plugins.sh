#!/usr/bin/env bash
# script used to generate the list of packages

DIRS=$(find . -type d | sed "s/.\///")

: ${PACKAGE_FILTER:=cat}

PACKAGES=''
for dir in $DIRS ; do
    files=$(find $dir -maxdepth 1 -name '*.opa')
    if [ -n "$files" ] ; then
        ./extract-import-plugin.sh $files
    fi
done | sort -u | $PACKAGE_FILTER
