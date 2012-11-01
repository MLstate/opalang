#!/usr/bin/env bash
# script used to generate the list of packages

exclude=$1
shift

LOOKED_PLACE=$@

if [ -s "$exclude" ]; then
    : ${PACKAGE_FILTER:=grep -E -v -f $exclude}
else
    : ${PACKAGE_FILTER:=cat}
fi

for d in $LOOKED_PLACE; do
    DIRS=$(find $d -type d | sed "s|$d\/||" | sed "s|$d||")
    for dir in $DIRS ; do
    	files=$(find $d/$dir -maxdepth 1 -name '*.opa')
    	if [ -n "$files" ] ; then
            this=$(echo $dir | sed 's/\//./g')
            echo "stdlib.$this"
    	fi
    done
done | sort -u | $PACKAGE_FILTER
