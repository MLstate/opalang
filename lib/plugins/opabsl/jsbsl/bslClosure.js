/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* module byClosure
   @author Sebastien Briais
   @author David Rajchenbach-Teller (made the code more JS idiomatic)
   @author Quentin Bourgerie
 */
##extern-type Closure.t
##extern-type Closure.args

#<Ifstatic:OPABSL_NODE>
#<Else>
global = this;
#<End>

/**
 * <!> Part of the JsInterface (used by funaction serialization)
**/
##register on_distant : string -> bool
##args(str)
{
  return (str in global) && global[str].distant || false
}

function set_distant(str, b)
{
  var l = str.split(",")
  for (var i = l.length; i--;) {
    (l[i] in global) && (global[l[i]].distant = b)
  }
}
##register set_distant_false : string -> void
##args(str)
{
    set_distant(str, false);
    return js_void;
}

function get_local_unsafe(str) {
  return global[str];
}

##register get_local : string -> option(Closure.t)
##args(str)
{
  return (str in global) ? js_some(global[str]) : js_none;
}

##module Args
  ##register create : int -> Closure.args
  ##args(n)
  {
      return new Array(n);
  }

  ##register length : Closure.args -> int
  ##args(a)
  {
      return a.length;
  }
  ##register set : Closure.args, int, 'a -> void
  ##args(a,i,x)
  {
      a[i] = x;
  }

  ##register get : Closure.args, int -> 'a
  ##args(a,i)
  {
      return a[i];
  }
##endmodule

##register create_anyarray : 'impl, int, 'ident -> Closure.t
##args(f,n,identifier)
{
    var new_closure = function() { return f.call(null,arguments) };
    new_closure.identifier = identifier;
    return new_closure;
}

##register [cps-bypass] create_anyarray_cps \ `create_anyarray_cps` : 'impl, int, 'ident, continuation(Closure.t) -> void
function create_anyarray_cps(f,n,identifier,k)
{
    var any_cps = function(){
        var args = Array.prototype.slice.call(arguments);
        var k = args.pop();
        return f(args, k);
    }
    any_cps.identifier = identifier;
    return_(k,any_cps);
}

/**
 * Part of JsInterface (funaction)
**/
function args_apply(closure,args) {
  return closure.apply(null,args)
}
##register apply \ `args_apply` : Closure.t, Closure.args -> 'a

##register [cps-bypass] apply_cps : Closure.t, Closure.args, continuation('a) -> void
##args(closure, args, k)
{
    // Warning : partial apply was broken
    args.push(k);
    args_apply(closure, args);
}

function get_closure_name(closure) {
    if ('identifier' in closure) return closure.identifier; // opa
    if ('name' in closure) return closure.name; // chrome
    return closure.toString().match(/function *([^(]*)/)[1]; // all
}

##register get_identifier : 'c -> option('a)
##args(closure)
{
    var name = get_closure_name(closure);
    return (global[name] == closure) ? js_some(make_onefield_record(static_field_of_name("closure_name"),name)) : js_none;
}
##register set_identifier : Closure.t, 'a -> void
##args(closure, identifier)
{
    closure.identifier = js_some(identifier);
}
/**
 * Part of JsInterface (funaction)
**/
function _env_apply_with_ty(closure,args,ty_args)
{
   // if env is empty, we should not apply, since it would triger the function computation
   // we only want to apply env, not to do any computation
   // in zero arity no extra lambda is waiting this empty env
   if( args.length ==0 ){
       //      closure.opa_args  = [];
       //      closure.opa_ty_args  = [];
      return closure;
    } else {
      var new_closure = closure.apply(null,args); // breaks tail-rec (I know it is not optimised in streets browsers but it will be some day)
      new_closure.identifier = get_closure_name(closure);
      new_closure.opa_args      = args;
      new_closure.opa_ty_args   = ty_args;
      return new_closure;
    }
}
##register env_apply_with_ty  \ `_env_apply_with_ty` : Closure.t, Closure.args,Closure.args -> Closure.t

##register get_args : Closure.t -> Closure.args
##args(closure)
{
    var res = closure.opa_args;
    return (res?res:[]);
}

##register get_ty_args : Closure.t -> Closure.args
##args(closure)
{
    var res = closure.opa_ty_args;
    return (res?res:[]);
}

##register is_empty : 'b -> bool
##args(closure)
{
    var name = get_closure_name(closure)
    return (global[name] == closure) // toplevel function are the only opa closures that are empty
}

##register import \ `clos_import` : 'a, int -> Closure.t
##register export \ `clos_export` : Closure.t -> 'a
function clos_import(clos,_arity) { return clos }
function clos_export(clos) { return clos }

/** Should print closure-related info. Ignored for the moment.*/
##register closure_debug_from_opa: string,string -> void
##args(topic, message)
{
    return ;
}

##register replace_identifier: string,string -> void
##args(prev, next)
{
    var tmp = global[prev];
    if(tmp != null){
      global[next] = tmp;
    }
    return js_void;
}
