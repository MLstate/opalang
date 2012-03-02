#!/usr/bin/env bash

grep '^-include' $@ | perl -p -e 's/^ *([-]include) +"([^ ]+)".*/\2/'
