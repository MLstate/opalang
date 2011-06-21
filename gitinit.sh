#!/bin/bash -e

help () (
    echo "$0:"
    echo "	- Initialise some sane standard git config"
    echo "	- Possibly setup for linking with extra repositories"
    echo "Options:"
    echo "	--extra-repo <name>	add the given repo inside ./repos/"
    echo "	--link [dir]		link to existing repositories in [dir] (by default, ../)"
    echo "				instead of cloning a fresh repo"
    echo "	--help			display this help message"
    echo
)

# This is used for e.g. ./init.sh --extra-repo git://github.com/MLstate/tictactopa.git
# Do not remove this, we actually use it
get_repo_name() { x=${1##*:}; x=${x##*/}; x=${x%.git}; echo $x; }

REPOS=()
LINK=""
LINKDIR=".."

while [ $# -gt 0 ]; do
    case $1 in
        --help|-h)
            help; exit 0;;
        --extra-repo)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi
            shift
            REPOS[${#REPOS[@]}]=$1
            ;;
        --link)
            if [ $# -ge 2 ] && [ "${2#-}" = "$2" ]; then shift; LINKDIR=$1; fi
            LINK=1
            ;;
        *) echo "Error: unknown option $1"
    esac
    shift
done

git config branch.master.rebase true
git config --global branch.autosetuprebase always

echo "OK: git setup complete"

mkdir -p repos

# don't forget to add those to .gitignore to avoid conflicts
for repo_link in "${REPOS[@]}"; do
    r=$(get_repo_name $repo_link)
    if [ -e "repos/$r" ]; then
        echo "repo $r is already there, skipping"
    elif [ -n "$LINK" ]; then
        if [ ! -d "$LINKDIR/$r/.git" ]; then
            echo "Error: could not find a valid git repo at $LINKDIR/$r"
        else
            RELLINK=$LINKDIR
            if [ "${RELLINK#/}" = "$RELLINK" ]; then RELLINK="../$RELLINK"; fi
            ln -s "$RELLINK/$r" repos/
            echo "OK: repo $r linked in repos/"
        fi
    else
        cd repos
        # we assume the full link is given, like github repository,
        # e.g. git://github.com/MLstate/tictactopa.git used by passtracker
        CLONE_PATH="$repo_link"
        git clone $CLONE_PATH
        cd ..
        echo "OK: repo $r cloned in repos/"
    fi
done
