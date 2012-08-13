#!/usr/bin/env bash
# script used to extract import plugin line

# source platform_helper (for sed)
. ${OPA_SOURCE_DIR:-$(dirname $0)/../../}/tools/platform_helper.sh

sed -n "s%^ *import-plugin  *\\(.*\\) *$%\\1%p" $1 | sed -e "s/{//" -e "s/}//" -e "s/, /,/g" | tr ',' '\n'
