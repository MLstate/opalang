#!/usr/bin/env bash

set -e
set -u
# Copyright © 2011 MLstate

# This file is part of OPA.

# OPA is free software: you can redistribute it and/or modify it under the
# terms of the GNU Affero General Public License, version 3, as published by
# the Free Software Foundation.

# OPA is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
# more details.

# You should have received a copy of the GNU Affero General Public License
# along with OPA. If not, see <http://www.gnu.org/licenses/>.

# This script is used to generate the module BuildInfos
# it is called by bld at build time of the repositories.
# Since this module is very high regarding to the dependencies
# it is regenerated only in release mode, or after a clean.

MLSTATE_DIFFING=${MLSTATE_DIFFING:-"0"}

help () {
    cat <<EOF
generating buildInfos.ml
Use:
   $0 opageneral-dir [options]
Options:
        --version file.txt     precise the file to read the name of opa version
        --release              release mode
EOF
}

OPA_VERSION="S?"
ROOTDIR=""
IS_RELEASE="false"

if [ -z "$1" ]; then
    help
    echo "opageneral-dir is not specified"
    exit 1
else
    ROOTDIR="$1"
    shift
fi

while [ "$#" -gt 0 ]; do
    case "$1" in
        -h|--help|help)
            help
            exit 0
            ;;
        --release)
            IS_RELEASE="true"
            ;;
        --version)
            if [ -z "$2" ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift;
            OPA_VERSION="$(cat $1)"
            ;;
        *)
            help
            exit 1
            ;;
    esac
    shift
done

ROOT_REPO="opalang"
REPOS="$ROOT_REPO"
PATH_TO_REPOS="repos"

git_version_cmd () { git log --pretty=oneline | wc -l; }
git_sha_cmd ()     { git log -n1 --pretty=format:%h; }
git_date_cmd ()    { git log -n1 --pretty=format:%ad --date=short; }

# special case for opalang, the origin is the publication
git_opalang_version_cmd () {
    local ORIGIN_SHA='fccc6851ccd2cb4fd9e83ba7b8c8d6d780ed3e13'
    git log --pretty=oneline "$ORIGIN_SHA".. | awk '/^'$ORIGIN_SHA'/ { x=1 } END { print NR+1-x }'
}

is_git_root () {
    LOOKED_FOR_REPO=$1
    [ -d .git ] && git remote show -n origin | grep -q 'URL:.*'$LOOKED_FOR_REPO
}

in_repo () {
    REPO=$1; shift
    P=$PWD
    cd $ROOTDIR
    if [ "$REPO" != "$ROOT_REPO" ]; then
        cd $PATH_TO_REPOS/$REPO
    fi
    if ! is_git_root $REPO; then
        echo "[!] Git repo info for $REPO not found" >&2; exit 1
    fi
    "$@"
    cd $P
}

if [ "$MLSTATE_DIFFING" = 1 ] ; then
    cat <<EOF
(*
  The following informations are not correct, because the toggle var
  MLSTATE_DIFFING was set during the generation of this file.
  This debug var is meant to reduce as much as possible the output
  of the compiler for comparing 2 branches.
*)
EOF
fi

# opa version
echo "let opa_version_name = \"${OPA_VERSION}\""

# git infos
for repo in $REPOS ; do
    if [ "$MLSTATE_DIFFING" = 1 ] ; then
        echo "let ${repo}_git_version = 0"
        echo "let ${repo}_git_sha = \"diffing\""
    elif ! is_git_root $repo; then
        echo "let ${repo}_git_version = 0"
        echo "let ${repo}_git_sha = \"\""
    else
        if [ "$repo" = "$ROOT_REPO" ] ; then
            echo "let ${repo}_git_version = $(in_repo $repo git_opalang_version_cmd)"
        else
            echo "let ${repo}_git_version = $(in_repo $repo git_version_cmd)"
        fi
        echo "let ${repo}_git_sha = \"$(in_repo $repo git_sha_cmd)\""
    fi
done
echo

# date
if [ "$MLSTATE_DIFFING" = 1 ] ; then
    echo "let date = \"diffing\""
    echo "let year = \"diffing\""
else
    echo "let date = \"$(in_repo $ROOT_REPO git_date_cmd)\""
    echo "let year = \"$(date +%Y)\""
fi

# release
echo "let is_release = $IS_RELEASE"
