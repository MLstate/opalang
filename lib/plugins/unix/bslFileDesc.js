/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Primitives for manipulates file descriptors.
 * @side{server}
 * @externType File.descriptor
 * @externType Unix.error
 *
 * @externType binary
 */
/** *****************************/
var FileSystem = require('fs');
/**
 * Open a file descriptor.
 * @register {string, string, option(int), callback(option(Unix.error), option(File.descriptor) -> void) -> void}
 */
var open = FileSystem.open;

/**
 * Close a file descriptor.
 * @register{File.descriptor, callback(option(Unix.error) -> void) -> void}
 */
var close = FileSystem.close;

/**
 * Read into a file descriptor.
 * @register{File.descriptor, int, option(int), callback(option(Unix.error), option(binary) -> void) -> void}
 */
function read(fd, length, pos, callback){
    FileSystem.read(fd, new Buffer(length), 0, length, pos,
      function(err, bytesRead, buffer){
            callback(err, buffer == null ? buffer : binary_of_buffer(buffer, bytesRead))
      }
    );
}

/**
 * Write into a file descriptor.
 * @register{File.descriptor, binary, option(int), callback(option(Unix.error), option(int) -> void) -> void}
 */
function write(fd, bin, pos, callback){
  FileSystem.write(fd, bin.contents, 0, bin.length, pos,
                   function(err, written, _){callback(err, written)});
}
