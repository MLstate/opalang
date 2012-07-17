# File meant to be launched with bash - DO NOT EDIT THE FIRST LINE

if ! which node &>/dev/null; then
    echo "--> node.js missing, please install nodejs from: http://nodejs.org"
    exit 1
fi;

if ! which npm &>/dev/null; then
    echo "--> npm missing, please install npm from: http://npmjs.org/"
    exit 1
fi;

function check-node-dependencies() (
    MISSING=""
    for i in $1; do
	# only keep first level modules
        if ! npm list -parseable | sed -e '\#\(.*\)/node_modules/\(.*\)/node_modules/\(.*\)#{d}' | grep -q "$i" && ! npm list -g -parseable | sed -e '\#\(.*\)/node_modules/\(.*\)/node_modules/\(.*\)#{d}' | grep -q "$i"; then
            MISSING="$MISSING $i"
        fi
    done
    if [ "$MISSING" != "" ]; then
        echo "--> some node modules are missing, please run: npm install -g$MISSING"
        exit 1;
    fi
)

check-node-dependencies "mongodb formidable nodemailer simplesmtp imap" || exit $?

if [ $? -ne 0 ]; then exit $?; fi;
node "$0" "$@"; exit $?;
