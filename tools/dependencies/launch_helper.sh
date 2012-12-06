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

    NODE_VERSION=v0.8.7

    # Detect OS
    IS_LINUX=""
    IS_MAC=""
    IS_WINDOWS=""
    IS_FREEBSD=""
    case $(uname) in
    CYGWIN*) IS_WINDOWS=1;;
    Darwin*) IS_MAC=1;;
    Linux*|GNU/kFreeBSD) IS_LINUX=1;;
    FreeBSD) IS_FREEBSD=1;;
    *)
            echo "Error: could not detect OS. Defaulting to Linux" >&2
            IS_LINUX=1
    esac

    echo "node.js is missing, Download and install it ? (no will abort) [Yn] \c"
    read yesno
    case "$yesno" in
        y|Y|yes|YES)
        if [ -n "$IS_MAC" ]; then
        port=`which port 2>&1`
        if [ $? -eq 0 ] && [ -x "$port" ]; then
            echo "--> Installing via MacPorts...";
            sudo port install nodejs
        else
            brew=`which brew 2>&1`
            if [ $? -eq 0 ] && [ -x "$brew" ]; then
            echo "--> Installing via Homebrew...";
            brew install node # Warning: brew installs are known to be buggy
            else
            if ! [ -f /tmp/node-$NODE_VERSION.pkg ]; then
                NODE_URL=http://nodejs.org/dist/$NODE_VERSION/node-$NODE_VERSION.pkg
                echo "--> Downloading $NODE_URL..."
                curl $NODE_URL > /tmp/node-$NODE_VERSION.pkg
            fi
            echo "--> Opening Node installer $NODE_VERSION, please follow the instructions and then relaunch this application"
            open /tmp/node-$NODE_VERSION.pkg
        exit 1
            fi
        fi
        elif [ -n "$IS_LINUX" ]; then
        case $(uname -v) in
            *Ubuntu*)
            sudo apt-get install python-software-properties
            sudo add-apt-repository ppa:chris-lea/node.js
            sudo apt-get update
            sudo apt-get install nodejs npm
            ;;
            *)
            echo "--> node.js is missing, please install node.js from: http://nodejs.org"
            exit 1
        esac
        else
        echo "--> node.js is missing, please install node.js from: http://nodejs.org"
        exit 1

        fi;;
    *) echo "--> Aborting..."; exit 1
    esac

fi;

if [ $? -ne 0 ]; then exit $?; fi;
node "$0" "$@"; exit $?;
