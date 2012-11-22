/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Define bypass needed to implements the opa logger.
 * @side{server}
 * @externType Logger.out_channel
 * @externType Logger.date
 *
 * Note : This bypass are too much specific, we should replace this bsl by a
 * more general bsl which manipulates sockets as example
 */

var fs = require('fs');
var tty = require('tty');

/**
 * @register { string -> opa[option(Logger.out_channel)]}
 */
function open_out(path) {
  try {
    return js_some(fs.openSync(path, 'a'));
  } catch(err) {
    console.log("BslLogger.open_out: Couldn't open file '"+path+"'");
    return js_none;
  }
}

// This can be done in node but it's complicated, you need to
// fork the process and use stream.pipe on it's stdin/stdout to
// link to a local stream object.  This can be implemented later
// if it is required.
/**
 * @register { string -> opa[option(Logger.out_channel)]}
 */
function open_pipe(cmd) {
  console.log("Not implemented: BslLogger.open_pipe");
  return js_none;
}

/**
 * @register { Logger.out_channel -> void}
 */
function close_out(oc) {
  if (oc === 1 || oc === 2) return js_void;
  try {
    fs.closeSync(oc);
    return js_void;
  } catch(err) {
    console.log("BslLogger.close_out: Error closing file");
    return js_void;
  }
}

/**
 * @register { Logger.out_channel, string -> void}
 */
function output(oc,str) {
  // writeSync doesn't seem to work with process.stderr.fd
  try {
    if (oc === 1) {
      process.stdout.write(str);
      return js_void;
    } else if (oc === 2) {
      process.stderr.write(str); // note: all writes to stderr are blocking
      return js_void;
    } else {
      var written = fs.writeSync(oc, str, 0, str.length, null);
      if (written < str.length) console.log("BslLogger.output: string length "+str.length+" written "+written);
      return js_void;
    };
  } catch(err) {
    console.log("BslLogger.output: Write error "+err);
    return js_void;
  }
}

/**
 * @register { Logger.out_channel -> opa[bool]}
 */
function is_tty(oc) {
  return tty.isatty(oc);
}

/**
 * @register { -> Logger.out_channel}
 */
function get_stderr() {
  return process.stderr.fd;
}

/**
 * @register { -> Logger.out_channel}
 */
function get_stdout() {
  return process.stdout.fd;
}

/**
 * @register { -> string}
 */
function get_cwd() {
  return process.cwd();
}

/**
 * @register { -> string}
 */
function os_type() {
  switch (process.platform) {
  case 'linux': return 'Unix';
  default: return 'Unknown';
  }
}

/**
 * @register { -> Logger.date} now
 */
function logger_now() {
  return new Date();
}

function pc(i) {
  if (i === null || typeof i == 'undefined') return "??";
  var s = (i % 100).toString();
  switch (s.length) {
  case 0: return "00";
  case 1: return "0"+s;
  default: return s;
  };
}

/* Had to do this because of dependency problems in stdlib */
/**
 * @register { Logger.date -> string}
 */
function log_time(t) {
  return "".concat(pc(t.getFullYear() % 100),"/",
                   pc(t.getMonth()+1),"/",
                   pc(t.getDate())," ",
                   pc(t.getHours()),":",
                   pc(t.getMinutes()),":",
                   pc(t.getSeconds()),".",
                   pc(Math.floor(t.getMilliseconds() / 10)));
}

/**
 * @register { -> string}
 */
function log_suffix() {
  var t = new Date();
  var yr = t.getFullYear();
  return "".concat(".",yr.toString(),".",pc(t.getMonth()+1),".",pc(t.getDate()));
}

