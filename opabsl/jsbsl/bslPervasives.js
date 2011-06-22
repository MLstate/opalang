/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * {1 Arithmetic operations}
 */

/**
 * Add. used for int, and float
 * @param {number} l
 * @param {number} r
**/
##register int_add \ op_add : int, int -> int
##register float_add \ op_add : float, float -> float
var op_add = function (l, r) { return l + r; };

##register int_sub \ op_sub : int, int -> int
##register float_sub \ op_sub : float, float -> float
var op_sub = function (l, r) { return l - r; };

##register int_mul \ op_mul : int, int -> int
##register float_mul \ op_mul : float, float -> float
var op_mul = function (l, r) { return l * r; };

##register float_div \ op_div : float, float -> float
var op_div = function (l, r) { return l / r; };

##register int_div \ divide : int, int -> int
function divide(a,b){
    if(b==0){
        error("Exception : Division by zero")
    }
    var res=a/b
    return (res>0)?Math.floor(res):Math.ceil(res)
}


##register int_mod \ op_mod : int, int -> int
var op_mod = function (l, r) { return l % r}
##register mod \ op_mod : int, int -> int

##register int_rem \ op_rem : int, int -> int
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

##register int_neg \ op_neg : int -> int
##register float_neg \ op_neg : float -> float
var op_neg = function (n) { return -n; };

##register abs \ `Math.abs` : int -> int

/**
 * {1 Numeric conversions}
 */

##register float_of_int : int -> float
##args(x)
{
    return x;
}

##register int_of_float: float -> int
##args(x)
{
    return x;
}


/**
 * {1 Logical operations}
 */

##register bool_and : bool, bool -> bool
##args(l, r)
{ return l && r; };

##register bool_or : bool, bool -> bool
##args(l, r)
{ return l || r; };


/**
 * {1 String/char conversions}
 */

##register string_of_char : char -> string
##args(c)
{ return c.toString(); }

##register string_to_char : string -> char
##args(s)
{ return s[0]; }

##register int_of_char       \ int_of_char : char -> int
##register int_of_first_char \ int_of_char : string -> int
function int_of_char(c){ return c.charCodeAt(0); }

##register int_of_string: string -> option(int)
##args(s)
{
    try {
        return js_some(parseInt(s, 0));
    } catch(e) {
        return js_none
    }
}

##register bool_of_string : string -> option(bool)
##args(a)
{
    if (a == "true")
        return js_some(true);
    else if (a == "false")
        return js_some(false);
    else
        return js_none;
}

##register string_of_bool : bool -> string
##args(a)
{ if(a) { return "true"; } else {return "false"; }}

##register float_of_string : string -> option(float)
##args(a)
{
    return ( "" + (a + 0) == a ) ? ( js_some(a+0) ) : js_none ;
}

##register string_of_int \ string_of_num: int -> string
function string_of_num(a){ return ""+a; }

##register string_of_float \ string_of_float : float -> string
function string_of_float(v)  {
      var str = ""+v;
    if (str.indexOf('.') >= 0 || str.indexOf('e') >= 0 || str[0] == 'N' || str[0] == 'I' || str[1] == 'I') {
          return str; //Printing corresponds to server-side printing
      } else {
          return str + ".0";//Printing needs to be adjusted
      }
  }


/**
 * Physical equality between JS objects
 */
##register areSameObject : 'a, 'b -> bool
##args(x, y)
{
    return x == y;
}


/**
 * Determine if the code is executed server-side
 *
 * @return false in this implementation
 */
##register webutils_server_side : -> bool
##args()
{ return false; }


/**
 * dummy implementation, it's for tests.
 * It is currently defined in [qmlJsfunClientLib.js]
 */
##register compare_raw \ compare_raw : 'a, 'a -> int

/**
 * Note: The following always return -1, 0 or 1 -- it's an important invariant
 */
##register compare_int      \ compare_native  : int, int -> int
##register compare_float    \ compare_native  : float, float -> int
##register compare_string   \ compare_native  : string, string -> int
##register compare_char     \ compare_native  : char, char -> int
function compare_native(c1, c2)
{
    if(c1<c2) return -1;
    if(c1>c2) return 1;
    return 0;
}

##register int_cmp_neq  : int, int -> bool
##args(c1, c2)
{return c1!=c2;}

##register int_cmp_eq   : int, int -> bool
##args(c1, c2)
{return c1==c2;}

##register int_cmp_lneq : int, int -> bool
##args(c1, c2)
{return c1< c2;}

##register int_cmp_leq  : int, int -> bool
##args(c1, c2)
{return c1<=c2;}

##register int_cmp_gneq : int, int -> bool
##args(c1, c2)
{return c1> c2;}

##register int_cmp_geq  : int, int -> bool
##args(c1, c2)
{return c1>=c2;}

// deprecated
##register jlog \ jlog_old_style : string -> void
var jlog_old_style;
var jlog_id= "__internal__log";
// because close_jlog is used as a string in the onclick below
// it must not be renamed by the compiler
// since the compiler doesn't rename fields, we defined close_jlog as a field
// DON'T use window instead of this, because it won't be defined in command line
this.close_jlog= function () { (new jQuery("#" + jlog_id)).remove() };
var jlog_item;
var jlog_with_colors= function(foreground, background, message)
{
  new $(function(){
    if (!document.getElementById(jlog_id))
    {
      var item = "position: absolute; right: 0px; top: 0px; z-index: 100; font-size: .7em; ";
      item += "background-color: "+background+"; color: "+foreground+"; width: 300px; border: 2px solid green; ";
      item += "white-space: nowrap; overflow-x: auto";
      var close = document.createElement("div");
      close.setAttribute("style", "float: right;");
      close.innerHTML='<a onclick="close_jlog()">X</a>';
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
  });
}

function jlog_for_browser(v){//A version of jlog for browsers
    jlog_with_colors("green", "white", v);
    return js_void;
}
function jlog_for_command_line(v){//A version of jlog for command-line testers
    print("STDERR:"+v);//STDERR is a hack to keep the reftester happy
    return js_void;
}

if(typeof window != "object" || command_line_execution)//If we're not in a browser
{
    jlog_old_style = jlog_for_command_line;
} else {
    jlog_old_style = jlog_for_browser
}

//The log function that should be used internally in the jsbsl
function jlog(s) { (%%BslSyslog.info%%)("BSL", s); }

/**
 * Type-unsafe identity.
 * Not for casual user.
 * For bypassing only the Opa typer, use rather [\@unsafe_cast]
 * Implementaion note: the qmljsimp back-end will inline this function
 */
##module Magic
  ##register id : 'a -> 'b
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


/**
 * Return the contents of the JS stack.
 *
 * For debugging only.
 */
##register get_stack \ `get_stack` : -> string
function get_stack(){//Adapted from an extract on Eric Wendelin's blog
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

var print_endline;
var prerr_endline = function(s)
{
    jlog_old_style(s);
}
var js_print;
function print_noline(s)
{
    print(s+"NONEWLINE"); //NONEWLINE is a hack to keep both the command-line JS and reftester happy
}
if(typeof window != "object" || command_line_execution)//If we're not in a browser
{
    print_endline = print;
    js_print      = print_noline
} else {
    print_endline = jlog_for_browser;
    js_print      = jlog_for_browser;
}

/**
 * {1 Printing}
 */

##register println_char   \ print_endline : char -> void
##register println_float  \ print_endline : float -> void
##register println_int    \ print_endline : int -> void
##register print_endline  \ print_endline : string -> void
##register prerr_endline  \ prerr_endline : string -> void

##register print_string \ js_print : string -> void
##register print_float  \ js_print : float -> void
##register print_int    \ js_print : int -> void
##register print_char   \ js_print : char -> void

##register dump\ dump_value : 'a -> string
var dump_value = function (x) {
  var dumper = {
  'number' : function(u){ return ""+u; },
  'string' : function(u){ return u; },
  'object' : function(u){
    if(u.toSource)
      return u.toSource();
    else {
     var s = "{ ";
     for(var i in u)
       s+=i+": "+dump_value(u[i])+", "
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
##register debug_print \ dump_value: 'a -> string

##opa-type Order.comparison
##opa-type Order.ordering

##register order_lt     \ result_lt  : Order.ordering
##register order_eq     \ result_eq  : Order.ordering
##register order_gt     \ result_gt  : Order.ordering

##register compare_lt   \ result_lt  : Order.comparison
##register compare_eq   \ result_eq  : Order.comparison
##register compare_gt   \ result_gt  : Order.comparison
##register compare_neq  \ result_neq : Order.comparison

var result_lt  = make_simple_record("lt")
var result_eq  = make_simple_record("eq")
var result_neq = make_simple_record("neq")
var result_gt  = make_simple_record("gt")
