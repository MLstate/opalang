#!/usr/bin/env bash

TMPPREF=/tmp/upchangelog_
DIFFGIT=${TMPPREF}diffgit
DIFFCHANGELOG=${TMPPREF}diffchangelog
DIFFCHANGELOGNOCOMMENT=${TMPPREF}diffchangelog_nocomment
FULLCHANGELOGPROP=${TMPPREF}FULLCHANGELOG.proposal
CHANGELOGPROP=${TMPPREF}FULLCHANGELOG.proposal

CHANGELOG=CHANGELOG
FULLCHANGELOG=tools/utils/FULLCHANGELOG
CHANGELOGTAGF=tools/utils/CHANGELOG.TAG

touch $FULLCHANGELOG

set -u
set -e

help() {
    cat >&2 <<EOF
Usage:
        upchangelog.sh options
with:
        -low TAG                  the tag where to start the extract (defaut is CHANGELOG.TAG)
        -high TAG                the tag where to start the extract (defaut is last vTAG)
        -help			shows this message
EOF
}



normalise_tag(){
    local SHA=$(git log -n 1 --format="%h" $1)
    local TAG=$(git tag --contains $1 | head -n 1)
    if [ "$TAG" = "" ];
    then
        echo $SHA
    else
        echo $TAG
    fi
}

CHANGELOGTAG=$(normalise_tag `cat $CHANGELOGTAGF`)
MASTERTAG=$(git tag --contains $CHANGELOGTAG | tail -n 1)

echo Target $CHANGELOGTAG to $MASTERTAG

while [ $# -gt 0 ]; do
    case "$1" in
        -low)
            shift
            CHANGELOGTAG=$(normalise_tag $1)
            ;;
        -high)
            shift
            MASTERTAG=$(normalise_tag $1)
            ;;
       esac
      shift
done

echo $CHANGELOGTAG "=>" $MASTERTAG
echo $(git log $CHANGELOGTAG..$MASTERTAG | wc -l) commits


header(){
if [ "$(head -n 1 CHANGELOG)" != "$CHANGELOGTAG" ];
then
    echo -n "$CHANGELOGTAG.."
fi
echo $MASTERTAG
}

content(){
    (git log -n 1 $1 | grep CHANGELOG | sed s/CHANGELOG//g | sed '1s/^\(.\)\{3\}//g')
}

changelogcommit(){
    local commit=$1
    local author=$(git log -n 1 --format="%an <%ae> %ad" $commit)
    if [ $(content $commit | wc -w) -eq 0 ];
    then
        echo -n " *   "
        git log -n 1 --format="%s" $commit
    else
        echo -n " * "
        content $commit
    fi
    echo " -- $author"
    echo " -- $commit"
    echo ""
}


git log --format="%h" --grep="^CHANGELOG" $CHANGELOGTAG..$MASTERTAG > $DIFFGIT

echo $(cat $DIFFGIT | wc -w) entries

if [ "$(cat $DIFFGIT | wc -w)" -gt 0 ];
then
    echo Adding the following
    header > $DIFFCHANGELOG
    for commit in $(cat $DIFFGIT);
    do
        changelogcommit $commit >> $DIFFCHANGELOG
    done
    cat $DIFFCHANGELOG | grep -v "^ \-\-"

    cat $DIFFCHANGELOG $FULLCHANGELOG > $FULLCHANGELOGPROP
    cat $FULLCHANGELOGPROP > $FULLCHANGELOG

    grep -v "^ \-\-" $DIFFCHANGELOG > $DIFFCHANGELOGNOCOMMENT
    cat $DIFFCHANGELOGNOCOMMENT $CHANGELOG > $CHANGELOGPROP
    echo $MASTERTAG > $CHANGELOGTAGF

    cat $CHANGELOGPROP > $CHANGELOG


    rm /${TMPPREF}*
else
    echo Nothing to do
fi
