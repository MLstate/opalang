/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @side{both}
 * @externType continuation('a)
 **/

/**
 * {1 Arithmetic operations}
 */

/**
 * Add. used for int, and float
 * @param {number} l
 * @param {number} r
**/
/**
 * @register {int, int -> int} int_add op_add
 * @pure
 */
/**
 * @register {float, float -> float} float_add op_add
 * @pure
 */
var op_add = function (l, r) { return l + r; };

/**
 * @register {int, int -> int} int_sub op_sub
 * @pure
 */
/**
 * @register {float, float -> float} float_sub op_sub
 * @pure
 */
var op_sub = function (l, r) { return l - r; };

/**
 * @register {int, int -> int} int_mul op_mul
 * @pure
 */
/**
 * @register {float, float -> float} float_mul op_mul
 * @pure
 */
var op_mul = function (l, r) { return l * r; };

/**
 * @register {float, float -> float} float_div op_div
 * @pure
 */
var op_div = function (l, r) { return l / r; };

/**
 * @register {int, int -> int} int_div divide
 * @pure
 */
function divide(a,b){
    if(b==0){
        error("Exception : Division by zero")
    }
    var res=a/b
    return (res>0)?Math.floor(res):Math.ceil(res)
}


/**
 * @register {int, int -> int} int_mod op_mod
 * @pure
 */
var op_mod = function (l, r) { return l % r}
/**
 * @register {int, int -> int} mod op_mod
 * @pure
 */

/**
 * @register {int, int -> int} int_rem op_rem
 * @pure
 */
var op_rem = function (l, r) {
  var r_pos = Math.abs(r);
  var l_pos = Math.abs(l);
  var res = l_pos - (r_pos * Math.floor(l_pos / r_pos));
  if (r > 0) {
      return res;
  } else {
      return -res;
  }
}

/**
 * @register {int -> int} int_neg op_neg
 * @pure
 */
/**
 * @register {float -> float} float_neg op_neg
 * @pure
 */
var op_neg = function (n) { return -n; };

/**
 * @register {string -> int} int_of_first_char int_of_char
 * @pure
 */
function int_of_char(c){ return c.charCodeAt(0); }

/**
 * Physical equality between JS objects
 */
/**
 * @register {'a, 'b -> bool}
 * @pure
 */
function areSameObject(x, y) {
    return x == y;
}



/**
 * dummy implementation, it's for tests.
 * It is currently defined in [qmlJsfunClientLib.js]
 */
/**
 * @opacapi
 * @register {'a, 'a -> int} compare_raw compare_raw
 * @pure
 */

/**
 * Note: The following always return -1, 0 or 1 -- it's an important invariant
 */
/**
 * @register {int, int -> int} compare_int      compare_native
 * @pure
 */
/**
 * @register {float, float -> int} compare_float    compare_native
 * @pure
 */
/**
 * @register {string, string -> int} compare_string   compare_native
 * @pure
 */
function compare_native(c1, c2)
{
    if(c1<c2) return -1;
    if(c1>c2) return 1;
    return 0;
}

/**
 * @register {int, int -> bool}
 * @pure
 */
function int_cmp_neq(c1, c2) {return c1!=c2;}

/**
 * @register {int, int -> bool}
 * @pure
 */
function int_cmp_eq(c1, c2) {return c1==c2;}

/**
 * @register {int, int -> bool}
 * @pure
 */
function int_cmp_lneq(c1, c2) {return c1< c2;}

/**
 * @register {int, int -> bool}
 * @pure
 */
function int_cmp_leq(c1, c2) {return c1<=c2;}

/**
 * @register {int, int -> bool}
 * @pure
 */
function int_cmp_gneq(c1, c2) {return c1> c2;}

/**
 * @register {int, int -> bool}
 * @pure
 */
function int_cmp_geq(c1, c2) {return c1>=c2;}

/**
 * Type-unsafe identity.
 * Not for casual user.
 * For bypassing only the Opa typer, use rather [\@unsafe_cast]
 * Implementaion note: the qmljsimp back-end will inline this function
 */
/** @module Magic */
/**
 * @opacapi
 * @register {'a -> 'b}
 * @pure
 */
function id(val) {
    return val;
  }
/** @endModule */


/*
 * error is currently defined in [ClientLibLib.js]
 */
/**
 * @register { string, string -> 'a}
 * @opacapi
 */
function fail(message, position) {
  error(position +" @fail: "+ message);
}

var field_fail = static_field_of_name("fail");
var field_position = static_field_of_name("position");

/**
 * @register {string, string, continuation('a) -> void}
 * @cpsBypass
 * @opacapi
 */
function fail_cps(message, position, k) {
  var trace = get_cps_trace(k);
  if(trace!="") trace = "\n*** Stack trace:\n" + trace;
  console.error(""+position+"\nfail: "+message+trace);
  var r = empty_constructor();
  r = add_field(r, field_fail, message);
  r = add_field(r, field_position, position);
  r = make_record(r);
  executexn(k, r);
  return;
}



/**
 * Return the contents of the JS stack.
 *
 * For debugging only.
 */
/**
 * @register { -> string}
 */
function get_stack() {
//Adapted from an extract on Eric Wendelin's blog
  var callstack            = [];
  var isCallstackPopulated = false;
  try {
    throw new Error("Stack trace"); //doesn't exist- that's the point
  } catch(e) {
    if (e.stack) { //Firefox
      var lines = e.stack.split("\n");
      for (var i=0, len=lines.length; i<len; i++) {
        if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
          callstack.push(lines[i]);
        }
      }
      //Remove call to get_stack()
      callstack.shift();
      isCallstackPopulated = true;
    }
    else if (window.opera && e.message) { //Opera
      var lines = e.message.split("\n");
      for (var i=0, len=lines.length; i<len; i++) {
        if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
          var entry = lines[i];
          //Append next line also since it has the file info
          if (lines[i+1]) {
            entry += " at " + lines[i+1];
            i++;
          }
          callstack.push(entry);
        }
      }
      //Remove call to get_stack()
      callstack.shift();
      isCallstackPopulated = true;
    }
  }
  if (!isCallstackPopulated) { //IE and Safari
    var currentFunction = arguments.callee.caller;
    while (currentFunction) {
      var fn = currentFunction.toString();
      var fname = fn.substring(fn.indexOf("function") + 8, fn.indexOf("(")) || "anonymous";
      callstack.push(fname);
      currentFunction = currentFunction.caller;
    }
  }
  return callstack.join("\n");
}



/********************************************************************/
// deprecated
/** @register { string -> void} jlog jlog_old_style */
var jlog_old_style;
if(IS_OPA_SERVER){
    jlog_old_style = function(v){
        process.stderr.write(v+"\n");
        return js_void;
    }
} else {
    var jlog_with_colors=function(foreground, background, message)
        {
            var jlog_item;
            var jlog_id= "__internal__log";
            jlog_item=document.getElementById(jlog_id);
            if (!jlog_item)
                {
                    var item = "position: absolute; right: 0px; top: 0px; z-index: 100; font-size: .7em; ";
                    item += "background-color: "+background+"; color: "+foreground+"; width: 300px; border: 2px solid green; ";
                    item += "white-space: nowrap; overflow-x: auto";
                    var close = document.createElement("div");
                    close.setAttribute("style", "float: right;");
                    var ggg="\'#"+jlog_id+"\'";
                    var x ="<a onclick=\"(new jQuery("+ggg+")).remove()\">X</a>";
                    close.innerHTML=x;
                    jlog_item = document.createElement("div");
                    jlog_item.setAttribute("style", item);
                    jlog_item.setAttribute("id", jlog_id);
                    jlog_item.appendChild(close);
                    document.body.appendChild(jlog_item);
                }
            var txt = document.createElement("div");
            txt.setAttribute("style", "clear:both");
            txt.appendChild(document.createTextNode(message));
            jlog_item.appendChild(txt);
        }

    jlog_old_style = function(v){
        jlog_with_colors("green", "white", v);
        return js_void;
    }
}

var print_endline, prerr_endline, js_print, js_prerr, jlog;
if(IS_OPA_SERVER){
    print_endline = function(s){
        process.stdout.write(s);
        process.stdout.write("\n");
    }
    prerr_endline = function(s){
        process.stderr.write(s);
        process.stderr.write("\n");
    }
    js_print = function(s){ process.stdout.write(s) }
    js_prerr = function(s){ process.stderr.write(s) }
} else {
    print_endline = prerr_endline = js_print = js_prerr = jlog = jlog_old_style;
}

/**
 * {1 Printing}
 */

/** @register { string -> void} print_endline print_endline */
/** @register { string -> void} prerr_endline prerr_endline */

/** @register { string -> void} print_string js_print */
/** @register { string -> void} prerr_string js_prerr */
/** @register { int -> void} print_int js_print */

var dumper = {
    'number' : function(u,_i){ return ""+u; },
    'string' : function(u,_i){ return u; },
    'object' : function(u,i){
    if(u.toSource)
      return u.toSource();
    else {
     var s = "{ ";
     for(var field in u)
         s+=field+": "+dump_(u[field],i+1)+", "
     s+="}"
     return s;
     }
   },
    'function': function(u,_i){
     if(u.toSource)
       return u.toSource();
     else return "function() { ...}"
   },
    'boolean':  function(u,_i){ return ""+u; }
  }

/**
 * @register {'a -> string} dump
 */
function dump(x) { return dump_(x,0); }

function dump_(x,i){
  if( i>100) return "...DUMP..."; // avoid stack overflow
  if( !x ) return ""+x;
  var t = typeof(x);
  if (t in dumper) return dumper[t](x,i);
  else return ""+x;
}

function code(i) {
  // Ridiculous, there has to be a better way than this to prevent Javascript from casting ints to strings.
  var tab =
    ["\x00","\x01","\x02","\x03","\x04","\x05","\x06","\x07","\x08","\x09","\x0a","\x0b","\x0c","\x0d","\x0e","\x0f",
     "\x10","\x11","\x12","\x13","\x14","\x15","\x16","\x17","\x18","\x19","\x1a","\x1b","\x1c","\x1d","\x1e","\x1f",
     "\x20","\x21","\x22","\x23","\x24","\x25","\x26","\x27","\x28","\x29","\x2a","\x2b","\x2c","\x2d","\x2e","\x2f",
     "\x30","\x31","\x32","\x33","\x34","\x35","\x36","\x37","\x38","\x39","\x3a","\x3b","\x3c","\x3d","\x3e","\x3f",
     "\x40","\x41","\x42","\x43","\x44","\x45","\x46","\x47","\x48","\x49","\x4a","\x4b","\x4c","\x4d","\x4e","\x4f",
     "\x50","\x51","\x52","\x53","\x54","\x55","\x56","\x57","\x58","\x59","\x5a","\x5b","\x5c","\x5d","\x5e","\x5f",
     "\x60","\x61","\x62","\x63","\x64","\x65","\x66","\x67","\x68","\x69","\x6a","\x6b","\x6c","\x6d","\x6e","\x6f",
     "\x70","\x71","\x72","\x73","\x74","\x75","\x76","\x77","\x78","\x79","\x7a","\x7b","\x7c","\x7d","\x7e","\x7f",
     "\x80","\x81","\x82","\x83","\x84","\x85","\x86","\x87","\x88","\x89","\x8a","\x8b","\x8c","\x8d","\x8e","\x8f",
     "\x90","\x91","\x92","\x93","\x94","\x95","\x96","\x97","\x98","\x99","\x9a","\x9b","\x9c","\x9d","\x9e","\x9f",
     "\xa0","\xa1","\xa2","\xa3","\xa4","\xa5","\xa6","\xa7","\xa8","\xa9","\xaa","\xab","\xac","\xad","\xae","\xaf",
     "\xb0","\xb1","\xb2","\xb3","\xb4","\xb5","\xb6","\xb7","\xb8","\xb9","\xba","\xbb","\xbc","\xbd","\xbe","\xbf",
     "\xc0","\xc1","\xc2","\xc3","\xc4","\xc5","\xc6","\xc7","\xc8","\xc9","\xca","\xcb","\xcc","\xcd","\xce","\xcf",
     "\xd0","\xd1","\xd2","\xd3","\xd4","\xd5","\xd6","\xd7","\xd8","\xd9","\xda","\xdb","\xdc","\xdd","\xde","\xdf",
     "\xe0","\xe1","\xe2","\xe3","\xe4","\xe5","\xe6","\xe7","\xe8","\xe9","\xea","\xeb","\xec","\xed","\xee","\xef",
     "\xf0","\xf1","\xf2","\xf3","\xf4","\xf5","\xf6","\xf7","\xf8","\xf9","\xfa","\xfb","\xfc","\xfd","\xfe","\xff"
     ];
  return tab[i];
}

/**
 * A useful routine for debugging the contents of binary buffers
 *
 * @register { string -> string}
 */
function memdump(s) {
  var i = 0, d = "";
  while (i < s.length) {
    var h = "", a = "", pos = i.toString(16);
    while (pos.length < 4) { pos = "0" + pos; };
    for (var j = 0; i < s.length && j < 16; i++,j++) {
      var ch = s.charCodeAt(i);
      var ac = (ch >= 0x20 && ch < 0x7f) ? s[i] : ".";
      var hx = ch.toString(16);
      if (hx.length == 1) hx = "0" + hx;
      var sp = (i == s.length - 1) ? "" : " ";
      h += hx + sp;
      a += ac;
    };
    while (h.length < 3*16) { h += " "; };
    d += pos + " " + h + " " + a + "\n";
  };
  return d;
}
//console.log(memdump("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));

/**
 * @register {binary -> string}
 */
function bindump(b) {
  var i = 0, d = "";
  while (i < b.length) {
    var h = "", a = "", pos = i.toString(16);
    while (pos.length < 4) { pos = "0" + pos; };
    for (var j = 0; i < b.length && j < 16; i++,j++) {
      var ch = b.contents[i];
      var ac = (ch >= 0x20 && ch < 0x7f) ? code(ch) : ".";
      var hx = ch.toString(16);
      if (hx.length == 1) hx = "0" + hx;
      var sp = (i == b.length - 1) ? "" : " ";
      h += hx + sp;
      a += ac;
    };
    while (h.length < 3*16) { h += " "; };
    d += pos + " " + h + " " + a + "\n";
  };
  return d;
}

/** @opaType Order.comparison */
/** @opaType Order.ordering */

var result_lt  = make_simple_record("lt")
var result_eq  = make_simple_record("eq")
var result_neq = make_simple_record("neq")
var result_gt  = make_simple_record("gt")
