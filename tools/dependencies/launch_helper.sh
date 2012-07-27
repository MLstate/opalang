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

node=`which node 2>&1`
if [ $? -ne 0 ] || [ ! -x "$node" ]; then
    echo "--> node.js missing, please install nodejs from: http://nodejs.org"
    exit 1
fi;

# npm=`which npm 2>&1`
# if [ $? -ne 0 ] || [ ! -x "$npm" ]; then
#     echo "--> npm missing, please install npm from: http://npmjs.org/"
#     exit 1
# fi;

if [ $? -ne 0 ]; then exit $?; fi;
node "$0" "$@"; exit $?;
