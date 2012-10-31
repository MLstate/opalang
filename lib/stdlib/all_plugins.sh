#!/usr/bin/env bash
# script used to generate the list of plugins

LOOKED_PLACE=$@

: ${PACKAGE_FILTER:=cat}

# source platform_helper (for sed)
. ${OPA_SOURCE_DIR:-$(dirname $0)/../../}/tools/platform_helper.sh

for d in $LOOKED_PLACE; do
    cd $d
    DIRS=$(find . -type d | sed "s|^.\/||")
    cd -
    for dir in $DIRS ; do
	files=$(find $d/$dir -maxdepth 1 -name '*.opa')
	if [ -n "$files" ] ; then
            for i in $files; do
		sed -n "s%^ *import-plugin  *\\(.*\\) *$%\\1%p" $i | sed -e "s/{//" -e "s/}//" -e "s/, /,/g" | tr ',' '\n'
            done
	fi
    done
done | sort -u | $PACKAGE_FILTER
