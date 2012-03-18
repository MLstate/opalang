#!/usr/bin/env bash
# script used to extract import plugin line

sed -n "s%^ *import-plugin  *\\(.*\\) *$%\\1%p" $1
