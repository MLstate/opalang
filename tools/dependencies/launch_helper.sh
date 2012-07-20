###################################
#    Copyright © 2012 MLstate
#
#    This file is part of Opa.
#
#    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
#    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
#    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
###################################

if ! which node &>/dev/null; then
    echo "--> node.js missing, please install nodejs from: http://nodejs.org"
    exit 1
fi;

if ! which npm &>/dev/null; then
    echo "--> npm missing, please install npm from: http://npmjs.org/"
    exit 1
fi;

# function check-node-dependencies() (
#     MISSING=""
#     for i in $1; do
# 	# only keep first level modules
#         if ! npm list -parseable 2> /dev/null | sed -e '\#\(.*\)/node_modules/\(.*\)/node_modules/\(.*\)#d' | grep -q "$i" && ! npm list -g -parseable 2> /dev/null | sed -e '\#\(.*\)/node_modules/\(.*\)/node_modules/\(.*\)#d' | grep -q "$i"; then
#             MISSING="$MISSING $i"
#         fi
#     done
#     if [ "$MISSING" != "" ]; then
#         echo "--> some node modules are missing, please run: npm install -g$MISSING"
#         exit 1;
#     fi
# )

# check-node-dependencies "mongodb formidable nodemailer simplesmtp imap" || exit $?

if [ $? -ne 0 ]; then exit $?; fi;
node "$0" "$@"; exit $?;
