#!/usr/bin/env bash
# script used to extract import plugin line
. ${OPA_SOURCE_DIR:-$(dirname $0)/..}/platform_helper.sh
sed -n "s%^ *import-plugin  *\\(.*\\) *$%\\1%p" $1 | sed -e "s/{//" -e "s/}//" -e "s/, */\n/g"
