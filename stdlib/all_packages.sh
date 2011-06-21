#!/bin/bash
# script used to generate the list of packages

DIRS=$(find . -type d | sed "s/.\///")

PACKAGES=''
for dir in $DIRS ; do
    files=$(find $dir -maxdepth 1 -name '*.opa')
    if [ -n "$files" ] ; then
        this=$(echo $dir | sed 's/\//./g')
        echo "stdlib.$this"
    fi
done | sort -u
