////////////////////////////////////
//    Copyright © 2012-2013 MLstate
//
//    This file is part of Opa.
//
//    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
//    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
//    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
////////////////////////////////////

var min_node_version = '0.9.9';

/**
 * Compare two versions, string separated with dots
 * @return true if version1 >= version2
 */
function geVersion(version1, version2) {
		var v1 = version1.toString().split('.');
		var v2 = version2.toString().split('.');
    for (var i=0; i<(Math.max(v1.length,v2.length)); i++) {
        if (v1[i]==undefined) v1[i]=0;
        if (v2[i]==undefined) v2[i]=0;
        if (Number(v1[i]) < Number(v2[i])) return false;
				if (Number(v1[i]) > Number(v2[i])) break;
    }
    return true;
}

if (min_node_version && !geVersion(process.versions.node, min_node_version)) {
    console.error('Your version of node seems to be too old. Please upgrade to a more recent version of node (>= '+min_node_version+')');
    process.exit(1);
}
