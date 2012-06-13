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

##extern-type continuation('a)

/**
 * {1 Arithmetic operations}
 */

/**
 * Add. used for int, and float
 * @param {number} l
 * @param {number} r
**/
##register [pure] int_add \ op_add : int, int -> int
##register [pure] float_add \ op_add : float, float -> float
var op_add = function (l, r) { return l + r; };

##register [pure] int_sub \ op_sub : int, int -> int
##register [pure] float_sub \ op_sub : float, float -> float
var op_sub = function (l, r) { return l - r; };

##register [pure] int_mul \ op_mul : int, int -> int
##register [pure] float_mul \ op_mul : float, float -> float
var op_mul = function (l, r) { return l * r; };

##register [pure] float_div \ op_div : float, float -> float
var op_div = function (l, r) { return l / r; };

##register [pure] int_div \ divide : int, int -> int
function divide(a,b){
    if(b==0){
        error("Exception : Division by zero")
    }
    var res=a/b
    return (res>0)?Math.floor(res):Math.ceil(res)
}


##register [pure] int_mod \ op_mod : int, int -> int
var op_mod = function (l, r) { return l % r}
##register [pure] mod \ op_mod : int, int -> int

##register [pure] int_rem \ op_rem : int, int -> int
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

##register [pure] int_neg \ op_neg : int -> int
##register [pure] float_neg \ op_neg : float -> float
var op_neg = function (n) { return -n; };

##register [pure] int_of_first_char \ int_of_char : string -> int
function int_of_char(c){ return c.charCodeAt(0); }

/**
 * Physical equality between JS objects
 */
##register [pure] areSameObject : 'a, 'b -> bool
##args(x, y)
{
    return x == y;
}



/**
 * dummy implementation, it's for tests.
 * It is currently defined in [qmlJsfunClientLib.js]
 */
##register [pure] compare_raw \ compare_raw : 'a, 'a -> int

/**
 * Note: The following always return -1, 0 or 1 -- it's an important invariant
 */
##register [pure] compare_int      \ compare_native  : int, int -> int
##register [pure] compare_float    \ compare_native  : float, float -> int
##register [pure] compare_string   \ compare_native  : string, string -> int
function compare_native(c1, c2)
{
    if(c1<c2) return -1;
    if(c1>c2) return 1;
    return 0;
}

##register [pure] int_cmp_neq  : int, int -> bool
##args(c1, c2)
{return c1!=c2;}

##register [pure] int_cmp_eq   : int, int -> bool
##args(c1, c2)
{return c1==c2;}

##register [pure] int_cmp_lneq : int, int -> bool
##args(c1, c2)
{return c1< c2;}

##register [pure] int_cmp_leq  : int, int -> bool
##args(c1, c2)
{return c1<=c2;}

##register [pure] int_cmp_gneq : int, int -> bool
##args(c1, c2)
{return c1> c2;}

##register [pure] int_cmp_geq  : int, int -> bool
##args(c1, c2)
{return c1>=c2;}

/**
 * Type-unsafe identity.
 * Not for casual user.
 * For bypassing only the Opa typer, use rather [\@unsafe_cast]
 * Implementaion note: the qmljsimp back-end will inline this function
 */
##module Magic
  ##register [pure] id : 'a -> 'b
  ##args(val)
  {
    return val;
  }
##endmodule


/*
 * error is currently defined in [ClientLibLib.js]
 */
##register [opacapi] fail : string, string -> 'a
##args(message, position)
{
  error(position +" @fail: "+ message);
}

var field_fail = static_field_of_name("fail");
var field_position = static_field_of_name("position");

##register [opacapi, cps-bypass] fail_cps : string, string, continuation('a) -> void
##args(message, position, k)
{
  console.error(""+position+"\nfail: %s"+message);
  var r = empty_constructor();
  r = add_field(r, field_fail, message);
  r = add_field(r, field_position, position);
  r = make_record(r);
  k.executexn(r);
  return;
}



/**
 * Return the contents of the JS stack.
 *
 * For debugging only.
 */
##register get_stack : -> string
##args()
{
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
##register jlog \ jlog_old_style : string -> void

#<Ifstatic:OPABSL_NODE>
function jlog_old_style(v){
    process.stderr.write(v);
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

##register print_endline  \ print_endline : string -> void
##register prerr_endline  \ prerr_endline : string -> void

##register print_string \ js_print : string -> void
##register prerr_string \ js_prerr : string -> void
##register print_int    \ js_print : int -> void

##register dump : 'a -> string
##args(x)
{
  var dumper = {
  'number' : function(u){ return ""+u; },
  'string' : function(u){ return u; },
  'object' : function(u){
    if(u.toSource)
      return u.toSource();
    else {
     var s = "{ ";
     for(var i in u)
       s+=i+": "+%%BslPervasives.dump%%(u[i])+", "
     s+="}"
     return s;
     }
   },
  'function': function(u){
     if(u.toSource)
       return u.toSource();
     else return "function() { ...}"
   },
  'boolean':  function(u){ return ""+u; }
  }
  var t = typeof(x);
  if (t in dumper) return dumper[t](x);
  else return t;
}

//' A useful routine for debugging the contents of binary buffers
##register memdump : string -> string
##args(s)
//function memdump(s)
{
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

##opa-type Order.comparison
##opa-type Order.ordering

var result_lt  = make_simple_record("lt")
var result_eq  = make_simple_record("eq")
var result_neq = make_simple_record("neq")
var result_gt  = make_simple_record("gt")
