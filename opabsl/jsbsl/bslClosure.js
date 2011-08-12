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

/* module byClosure
   @author Sebastien Briais
   @author David Rajchenbach-Teller (made the code more JS idiomatic)
 */
##extern-type Closure.t
##extern-type Closure.args

var global = this;

/**
 * <!> Part of the JsInterface (used by funaction serialization)
**/
##register on_distant : string -> bool
##args(str)
{
  return (str in global) && global[str].distant || false
}

function set_distant(str)
{
  var l = str.split(",")
  for (var i = l.length; i--;) {
    (l[i] in global) && (global[l[i]].distant = true)
  }
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
    /*
    var any_cps = function(args){
        var k = args.pop();
        return f(args, k);
    }
    return %BslClosure.create%(any_cps, n+1, identifier);
    */
    error("TODO create_any_array_cps")
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
   error("TODO: apply_cps")
   /*if(closure.arity - 1 == args.length + closure.args.length){
      args.push(k);
   }
   return %BslClosure.apply%(closure, args);*/
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
