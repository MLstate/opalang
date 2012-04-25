/*
    Copyright © 2011, 2012 MLstate

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
   Bypasses for CPS

   @author Maxime Audouin <maxime.audouin@mlstate.com>
   @author Rudy Sicard
   @review David Rajchenbach-Teller (started Aug 20th, 2010)
*/



##extern-type Cps.future('a)
##extern-type continuation('a)
##extern-type [normalize] func('a, 'b)
##extern-type [normalize] black_future

##extern-type [normalize] _unit_continuation
##extern-type [normalize] _unit
    /* the type unit that doesn't get projected */

/**
 * Defined in [qmlCpsClientLib.js]
 * it use js_debug, with a prefix identifying 'CPS'
**/
##register debug \ cps_debug : int, string -> void
##register [opacapi, no-projection, restricted : cps] before_wait : -> void
##args()
{
    // Nothing todo
    return js_void;
}



//////////////////////////////////////////////////
// BARRIER ///////////////////////////////////////
//////////////////////////////////////////////////

/**
 * @param {(number|string)=} name An optional name, used for debugging purposes
 * @return {!Barrier}
 */
function make_barrier(name)
{
    return new Barrier(name);
}
##register [no-projection, restricted : cps] make_barrier       \ make_barrier    : string -> Cps.future('a)
##register [no-projection, restricted : cps] black_make_barrier \ make_barrier    : string -> black_future
//'

/**
 * Non-blocking wait for a barrier to be [release]d
 *
 * @param {!Barrier} barrier
 * @param {!Continuation} k
 * @return {!*}
 */
function wait_barrier(barrier, k){
    barrier.wait(k);
}
##register [no-projection, restricted : cps] wait \ wait_barrier : Cps.future('a), continuation('a) -> void


/**
 * Release a [Barrier]
 *
 * @param {!Barrier} barrier
 * @param {!*} x The value to release
 * @return {!*}
 */
function release_barrier(barrier, x){
    barrier.release(x);
}
##register [no-projection, restricted : cps] release_barrier \ release_barrier : Cps.future('a), 'a -> void

function toplevel_wait(barrier){
    return blocking_wait(barrier);
}
##register [opacapi, no-projection, restricted : cps] toplevel_wait \ toplevel_wait : Cps.future('a) -> 'a
##register [opacapi, no-projection, restricted : cps] black_toplevel_wait \ toplevel_wait : black_future -> 'a


//////////////////////////////////////////////////
// EXCEPTION /////////////////////////////////////
//////////////////////////////////////////////////
##register [opacapi, no-projection, restricted : cps] handler_cont \ `QmlCpsLib_handler_cont` : continuation('a) -> continuation('c)



/**
 * Defined in [qmlCpsClientLib.js]
**/
##register [no-projection : cps, restricted : cps] spawn \ spawn : (_unit, continuation('a) -> _unit) -> Cps.future('a)

##register [opacapi, no-projection : cps, restricted : cps] callcc_directive \ `QmlCpsLib_callcc_directive` : (continuation('a), _unit_continuation -> _unit), continuation('a) -> _unit


/**
 * Thread_context does not really makes sens on the client side.
 * Not clear what this function should return.
 * Currently, the cps mode on the client is not used.
**/
##register [no-projection : cps] thread_context: continuation('a) -> option(opa['thread_context])
##args(b)
{
    cps_assert(false, "[thread_context] client-side thread context not implemented yet");
}

/**
 * Fake implementation of [with_thread_context].
 * Currently implemented as [snd]
**/
##register [no-projection, restricted : cps] with_thread_context : opa['b], continuation('a) -> continuation('a)
##args(tc, b)
{
  //TODO restore options
  //return new Continuation(b.options, b._payload, tc);
  return b;
}


##register [no-projection:cps, restricted:cps] cont_native \ `cont` : ('a -> _unit) -> continuation('a)
function cont(f){
  //return new Continuation(default_options, f, default_thread_context);
    //TODO restore options
    return new Continuation(f);
}

##register [no-projection:cps, restricted:cps] ccont_native \ `ccont` : continuation('b), ('a -> _unit) -> continuation('a)
function ccont(b, f){
  //return b.with_payload(f);
    //TODO restore options
    return new Continuation(f);
}

##register [no-projection, restricted : cps] return \ return_ : continuation('a), 'a -> void





##register [no-projection, restricted : cps] magic_func : func('a, 'c) -> func('e, 'f)
##args(a)
{ return a; }


##register [no-projection, restricted : cps] black_release_barrier \ release_barrier : black_future, 'a -> void

/**
 * Defined in [qmlCpsClientLib.js]
**/
##register [no-projection, restricted : cps] loop_schedule \ loop_schedule : opa['d] -> void


/* User ByPass (see, cps.opa) - used by the user, need a projection */
##register user_return \ return_ : continuation('a), 'a -> void


##register user_cont : ('a -> void) -> continuation('a)
##args(f)
{
  // Note cps was not implemented on client side but user_cont it's
  // used it can't be an assert false.
  return cont(f);
}

##register execute : continuation('a), 'a -> void
##args(k, x)
{
  execute(k, x);
}

/**
 * The thread context for this VM
 */
var global_thread_context;
if (command_line_execution) {
  global_thread_context = js_none
} else {
  var cookie = getStableCookie();
  cookie = ('some' in cookie)?cookie.some:"BADCOOKIE";
  global_thread_context =
    js_some(normalize_obj(
    {
        key:{
            client:{
                client: cookie,
                page    : page_server
            }},
        request: js_none,
        details:{
            some:{
                locale:  js2list([window.navigator.language]),
                browser: {
                    environment: { Unidentified: js_void },
                    renderer:    { Unidentified: js_void }
                }
            }
        },
        constraint: {free:js_void}
    }
    ))
}

##module Notcps_compatibility
##register [opacapi] thread_context : opa['d] -> option(opa['b])
  ##args(a)
  {
      return global_thread_context;
  }

##register with_thread_context : opa['d], 'a -> 'a
  ##args(a, b)
  {
    return b;
  }

##endmodule


var _directive_thread_context_ = %%BslCps.Notcps_compatibility.thread_context%%;
var _directive_with_thread_context_ = function(context, expression){
    return expression
}

##register print_trace : continuation(_) -> void
##args(_)
{
    return ;
}
