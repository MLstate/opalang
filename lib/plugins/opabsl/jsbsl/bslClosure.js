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

/**
 * This module provides primitives to manipulates Opa Closure.
 *
 * @author Sebastien Briais
 * @author David Rajchenbach-Teller (made the code more JS idiomatic)
 *
 * @author Quentin Bourgerie
 */

##extern-type Closure.t
##extern-type Closure.args

##opa-type Closure.info

#<Ifstatic:OPABSL_NODE>
#<Else>
global = this;
#<End>

//////////////////////////////////////////////////
// Closure arguments
//////////////////////////////////////////////////

##module Args
  ##register [pure] create : int -> Closure.args
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

//////////////////////////////////////////////////
// Closure Information
//////////////////////////////////////////////////

/**
 * Retreive a closure from an identifier.
 */
##register get_local : string -> opa[option(Closure.t)]
##args(str)
{
  var f = global[str];
  return (f == null) ? js_none : js_some(f);
}

/**
 * Indicates closure presence/absence on the other side from its identifier.
 */
##register on_distant : string -> bool
##args(str)
{
  var f = global[str];
  return (typeof f != 'undefined') && f.distant || false
}

function set_distant(str, b)
{
  var l = str.split(",")
  for (var i = l.length; i--;) {
    (l[i] in global) && (global[l[i]].distant = b)
  }
}
##register [opacapi] set_distant : string, bool -> void
##args(str, b)
{
    set_distant(str, b);
    return js_void;
}

function get_closure_name(closure) {
    var tmp;
    // opa - see type Closure.info
    tmp = closure.info ;
    if (typeof tmp != 'undefined') return tmp.closure_name;

    // chrome
    tmp = closure.name;
    if (tmp) return tmp;

    // all
    return closure.toString().match(/function *([^(]*)/)[1];
}

##register get_info : 'c -> opa[option(Closure.info)]
##args(closure)
{
    var info = closure.info;
    if (typeof info != 'undefined') {
       return js_some(info);
    } else {
      var name = get_closure_name(closure);
      return (global[name] == closure) ? js_some(make_onefield_record(static_field_of_name("closure_name"),name)) : js_none;
    }
}

##register set_info : Closure.t, Closure.info -> void
##args(closure, info)
{
    closure.info = info;
}

/**
 * Get arguments stored in the closure.
 * @see : env_apply_with_ty
 */
##register get_args : Closure.t -> Closure.args
##args(closure)
{
    var res = closure.opa_args;
    return (res?res:[]);
}

/**
 * Get type of arguments stored in the closure.
 * @see : env_apply_with_ty
 */
##register get_ty_args : Closure.t -> Closure.args
##args(closure)
{
    var res = closure.opa_ty_args;
    return (res?res:[]);
}

/**
 * Returns the emptyness of the closure.
 * As we use JavaScript closure, only toplevel function are empty closure.
 */
##register is_empty : Closure.t -> bool
##args(closure)
{
    var name = get_closure_name(closure)
    return (global[name] == closure) //
}

//////////////////////////////////////////////////
// Closure
//////////////////////////////////////////////////

/**
 * Create an Opa closure
 * @param [f] an native implementation
 * @param [n] the arity of the closure
 * @param [info] the information of the closure
 */
##register [pure] create_anyarray : 'impl, int, Closure.info -> Closure.t
##args(f, n, info)
{
  var new_closure = function() { return f.call(null,arguments) };
  new_closure.info = info;
  return new_closure;
}

/**
 * Cps version of [create_anyarray].
 */
##register [pure, cps-bypass] create_anyarray_cps \ `create_anyarray_cps` : 'impl, int, Closure.info, continuation(Closure.t) -> void
function create_anyarray_cps(f, n, info, k)
{
    var any_cps = function(){
        var args = Array.prototype.slice.call(arguments);
        var k = args.pop();
        return f(args, k);
    }
    any_cps.info = info;
    return_(k, any_cps);
}

/**
 * Create an Opa closure from a native implementation and the closure arity.
 * Note : Identity because Opa use JavaScript Closure.
 */
##register [pure] import : 'a, int -> Closure.t
##args(clos,_arity)
{
  return clos;
}

/**
 * Get from an Opa closure to the native implementation.
 * Note : Identity because Opa use JavaScript Closure.
 */
##register [pure] export : Closure.t -> 'a
##args(clos)
{
  return clos;
}

/**
 * Apply arguments [args] to [closure].
 */
##register apply : Closure.t, Closure.args -> 'a
##args(closure,args)
{
  return closure.apply(null,args)
}

/**
 * Cps version of [apply]
 */
##register [cps-bypass] apply_cps : Closure.t, Closure.args, continuation('a) -> void
##args(closure, args, k)
{
    // Warning : partial apply was broken
    args.push(k);
    var r = %%BslClosure.apply%%(closure, args);
    if (r && r !== js_void) push(task_from_return(r[0], [r[1]]));
}

/**
 * Part of JsInterface (funaction)
 */
function _env_apply_with_ty(closure,args,ty_args)
{
   // if env is empty, we should not apply, since it would triger the function computation
   // we only want to apply env, not to do any computation
   // in zero arity no extra lambda is waiting this empty env
   if( args.length ==0 ){
      return closure;
    } else {
      var new_closure           = closure.apply(null,args);
      new_closure.info          = option2js(%%BslClosure.get_info%%(closure));
      new_closure.opa_args      = args;
      new_closure.opa_ty_args   = ty_args;
      return new_closure;
    }
}
##register env_apply_with_ty  \ `_env_apply_with_ty` : Closure.t, Closure.args,Closure.args -> Closure.t

/**
 * This function is used by the runtime renaming of client js ast, it's goal is
 * to have same identifer on client and server side.
 * Moreover set the field 'distant' of the closure to true, indeed if the
 * closure is renamed, it means they are a client implementation.
 */
##register replace_identifier: string, string -> void
##args(prev, next)
{
    var tmp = global[prev];
    // [tmp == null] if it's a function on client side only.
    if(tmp != null&&tmp.constructor==Function){
      // We known only renamed closure will be on the client side, all other
      // closure are just not present on client side or be cleaned.
      tmp.distant  = true;
      // Force the new name of closure (see get_closure_name) this construction
      // have the highest priority
      tmp.info = {closure_name : next};
      global[next] = tmp;
    }
    return js_void;
}





/** Should print closure-related info. Ignored for the moment.*/
##register closure_debug_from_opa: string,string -> void
##args(topic, message)
{
  return ;
}
