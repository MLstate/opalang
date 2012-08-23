/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

//////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARING BEETWEEN THE JAVASCRIPT AND NODE BSL //
//////////////////////////////////////////////////////////////////////

/** @externType continuation('a) */

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
  console.error(""+position+"\nfail: "+message);
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

#<Ifstatic:OPABSL_NODE>
function jlog_old_style(v){
    process.stderr.write(v+"\n");
    //process.stdout.flush(); // node doesn't have flush
    // Suggestion, try:
    //  var done = process.stderr.write("STDERR:"+v);
    //  while (!done) { process.stderr.on('drain', function (_) { done = true; }); }
    // or use fs.fsyncSync?
    return js_void;
}
#<Else>
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

function jlog_old_style(v){//A version of jlog for browsers
    jlog_with_colors("green", "white", v);
    return js_void;
}
#<End>

#<Ifstatic:OPABSL_NODE>
var print_endline = function(s){
  process.stdout.write(s);
  process.stdout.write("\n");
}
var prerr_endline = function(s){
  process.stderr.write(s);
  process.stderr.write("\n");
}
var js_print = function(s){
  process.stdout.write(s)
}
var js_prerr = function(s){
  process.stderr.write(s)
}
#<Else>
var print_endline = jlog_old_style;
var prerr_endline = jlog_old_style;
var js_print      = jlog_old_style;
var js_prerr      = jlog_old_style;
var jlog          = jlog_old_style;
#<End>

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
function dump_(x,i){
  if( i>100) return "...DUMP..."; // avoid stack overflow
  if( !x ) return ""+x;
  var t = typeof(x);
  if (t in dumper) return dumper[t](x,i);
  else return ""+x;
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

/** @opaType Order.comparison */
/** @opaType Order.ordering */

var result_lt  = make_simple_record("lt")
var result_eq  = make_simple_record("eq")
var result_neq = make_simple_record("neq")
var result_gt  = make_simple_record("gt")
