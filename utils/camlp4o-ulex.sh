#!/usr/bin/env bash
source ../config.sh

camlp4o -I $LIB_ULEX_DIR $@

