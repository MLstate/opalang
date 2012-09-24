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
   Bypasses for CPS

   @author Maxime Audouin <maxime.audouin@mlstate.com>
   @author Rudy Sicard
   @review David Rajchenbach-Teller (started Aug 20th, 2010)

   @author Quentin Bourgerie 2012
*/

/** @externType Cps.future('a) */
/**
 * @externType func('a, 'b)
 * @normalize
 */
/**
 * @externType black_future
 * @normalize
 */

/**
 * @externType _unit_continuation
 * @normalize
 */
/**
 * @externType _unit
 * @normalize
 */
    /* the type unit that doesn't get projected */

/** @register {int, string -> void} debug cps_debug */


//////////////////////////////////////////////////
// BARRIER ///////////////////////////////////////
//////////////////////////////////////////////////
/**
 * @register { -> void}
 * @opacapi
 * @noProjection
 * @restricted cps
 */
function before_wait() {
  // Nothing todo
  return js_void;
}

/**
 * @param {(number|string)=} name An optional name, used for debugging purposes
 * @return {!Barrier}
 */
function make_barrier(name)
{
  return new Barrier(name);
}

/**
 * @register  {string -> Cps.future('a)} make_barrier make_barrier
 * @noProjection
 * @restricted cps
 */
/**
 * @register  {string -> black_future} black_make_barrier make_barrier
 * @noProjection
 * @restricted cps
 */

/**
 * Non-blocking wait for a barrier to be [release]
 *
 * @param {!Barrier} barrier
 * @param {!Continuation} k
 * @return {!*}
 */
function wait_barrier(barrier, k){
  barrier.wait(k);
}
/**
 * @register {Cps.future('a), continuation('a) -> void} wait wait_barrier
 * @noProjection
 * @restricted cps
 */


/**
 * Release a [Barrier]
 *
 * @param {!Barrier} barrier
 * @param {!*} x The value to release
 * @return {!*}
 */
function release_barrier(barrier, x) {
  barrier.release(x);
}
/**
 * @register {Cps.future('a), 'a -> void} release_barrier release_barrier
 * @noProjection
 * @restricted cps
 */

/**
 * @register {Cps.future('a) -> 'a}
 * @opacapi
 * @noProjection
 * @restricted cps
 */
function toplevel_wait(barrier) {
  return blocking_wait(barrier);
}

/**
 * FIXME: We can't refer to the aliased bypass directly, since
 * it'll be renamed
 *
 * @register {black_future -> 'a} black_toplevel_wait BslCps_toplevel_wait
 * @opacapi
 * @noProjection
 * @restricted cps
 */

//////////////////////////////////////////////////
// EXCEPTION /////////////////////////////////////
//////////////////////////////////////////////////

/**
 * FIXME: Use actual function name
 *
 * @register {continuation('a) -> continuation('c)} handler_cont QmlCpsLib_handler_cont
 * @opacapi
 * @noProjection
 * @restricted cps
 */

/**
 * @register {(opa['c], continuation('a) -> _unit), continuation('a) -> continuation('a)}
 * @pure
 * @opacapi
 * @noProjection cps
 * @restricted cps
 */
function catch_native(h, k) {
  return catch_(k, h);
}

/**
 * @register {(_unit, continuation('a) -> _unit) -> Cps.future('a)} spawn spawn
 * @noProjection cps
 * @restricted cps
 */

/**
 * @register {(continuation('a), _unit_continuation -> _unit), continuation('a) -> _unit} callcc_directive callcc_directive
 * @pure
 * @opacapi
 * @noProjection cps
 * @restricted cps
 */

/**
 * @register {continuation('a) -> opa[option('thread_context)]} thread_context
 * @pure
 * @noProjection cps
 */
function bslcps_thread_context(k) {
  return js2option(thread_context(k));
}

/**
 * @register {opa['b], continuation('a) -> continuation('a)} with_thread_context with_thread_context
 * @pure
 * @noProjection
 * @restricted cps
 */

/**
 * @register {('a -> _unit) -> continuation('a)} cont_native cont
 * @pure
 * @noProjection cps
 * @restricted cps
 */

/**
 * @register {continuation('b), ('a -> _unit) -> continuation('a)} ccont_native ccont
 * @pure
 * @noProjection cps
 * @restricted cps
 */

/**
 * @register {continuation('a), 'a -> void} return return_tc
 * @pure
 * @noProjection
 * @restricted cps
 */

/**
 * @register {black_future, 'a -> void} black_release_barrier release_barrier
 * @noProjection
 * @restricted cps
 */

/**
 * @register {continuation('a), 'a -> void} user_return return_
 * @pure
 */


/**
 * @register {('a -> void) -> continuation('a)}
 * @pure
 */
function user_cont(f) {
  return cont(f);
}

/**
 * @register {('a, continuation(opa[void]) -> void ), continuation(continuation('a)) -> void}
 * @pure
 * @cpsBypass
 */
function user_cont_cps(f, k) {
  var fk = ccont(k, function(a){f(a, ccont(fk, function(){}))});
  return_(k, fk);
}

/**
 * @register {continuation('a), 'a -> void} execute
 */
function bslcps_execute(k, x) {
  execute(k, x);
  return js_void;
}

var topval, top0 = 0, top1 = 1;

/**
 * @register {opa['a] -> void}
 * @opacapi
 * @restricted cps
 * @noProjection
 */
function topk(v){
  topval = v;
  top1++;
}

/**
 * @register {_unit -> opa['a]}
 * @opacapi
 * @restricted cps
 * @noProjection
 */
function topwait(r){
  if (r) push(task_from_return(r[0], [r[1]]));
  loop_schedule(true);
  if(top0 == top1){
    throw new Error("Your toplevel contains value which can't be synchronously computed")
  }
  top0 = top1;
  return topval;
}

/**
 * The thread context for this VM
 */
#<Ifstatic:OPABSL_NODE>
  var global_thread_context = js_none
#<Else>
var global_cookie = getStableCookie();
global_cookie = ('some' in global_cookie)?global_cookie.some:"BADCOOKIE";
var global_thread_context =
  js_some(normalize_obj(
  {
      key:{
          client:{
              client: global_cookie,
              page  : page_server
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
  ));
#<End>

/**
 * @register{string, continuation('d), (continuation('a) -> _unit) -> 'a}
 * @opacapi
 * @restricted cps
 * @noProjection
 */
function uncps_native(id, k, f){
  return uncps(k, f, id);
}

/** @module Notcps_compatibility */

/**
 * @register {opa['d] -> opa[option('b)]} thread_context
 * @opacapi
 */
function notcps_thread_context(a) {
    return global_thread_context;
}

/**
 * @register {opa['d], 'a -> 'a} with_thread_context
 */
function bslcps_with_thread_context(a, b) {
  return b;
}

/**
 * @register {(-> 'b) -> (continuation('b) -> void)}
 * @opacapi
 */
function cps0_native(f) {
  return cps(f)
}

/**
 * @register {('a -> 'b) -> ('a, continuation('b) -> void)}
 * @opacapi
 */
function cps1_native(f) {
  return cps(f)
}

/**
 * @register { ('a, 'b -> 'c) -> ('a, 'b, continuation('c) -> void)}
 * @opacapi
 */
function cps2_native(f) {
  return cps(f)
}

/**
 * @register { ('a, 'b, 'c -> 'd) -> ('a, 'b, 'c, continuation('d) -> void)}
 * @opacapi
 */
function cps3_native(f) {
  return cps(f)
}

/**
 * @register { ('a, 'b, 'c, 'd -> 'e) -> ('a, 'b, 'c, 'd, continuation('e) -> void)}
 * @opacapi
 */
function cps4_native(f) {
  return cps(f)
}

/**
 * @register { ('a, 'b, 'c, 'd, 'e -> 'f) -> ('a, 'b, 'c, 'd, 'e, continuation('f) -> void)}
 * @opacapi
 */
function cps5_native(f) {
  return cps(f)
}

/**
 * @register {continuation(void)}
 * @opacapi
 * @noProjection
 * @restricted cps
 */
var dummy_cont = cont(function(x){return;})

/** @endModule */

/**
 * @register { continuation(_) -> void}
 */
function print_trace(_) {
    console.log("BslCps.print_trace", "NYI");
    return ;
}
