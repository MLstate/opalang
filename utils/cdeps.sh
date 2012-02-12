#!/usr/bin/env sh

perl -n -e 'if (/^ *(#include) +"(\.\.\/)*([^"]+)".*/) { print "$3\n"; }' $@
