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

##extern-type Cps.future('a)
##extern-type [normalize] func('a, 'b)
##extern-type [normalize] black_future

##extern-type [normalize] _unit_continuation
##extern-type [normalize] _unit
    /* the type unit that doesn't get projected */

##register debug \ cps_debug : int, string -> void


//////////////////////////////////////////////////
// BARRIER ///////////////////////////////////////
//////////////////////////////////////////////////
##register [opacapi, no-projection, restricted : cps] before_wait : -> void
##args()
{
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
##register [no-projection, restricted : cps] make_barrier       \ make_barrier    : string -> Cps.future('a)
##register [no-projection, restricted : cps] black_make_barrier \ make_barrier    : string -> black_future
//'

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

##register [pure, opacapi, no-projection : cps, restricted : cps] catch_native : \
(opa['c], continuation('a) -> _unit), continuation('a) -> continuation('a)
##args(h, k)
{
  return catch_(k, h);
}


##register [no-projection : cps, restricted : cps] spawn \ spawn : (_unit, continuation('a) -> _unit) -> Cps.future('a)

##register [pure, opacapi, no-projection : cps, restricted : cps] callcc_directive \ callcc_directive : \
(continuation('a), _unit_continuation -> _unit), continuation('a) -> _unit

##register [pure, no-projection : cps] thread_context : continuation('a) -> option(opa['thread_context])
##args(k)
{
  return js2option(thread_context(k));
}

##register [pure, no-projection, restricted : cps] with_thread_context \ with_thread_context : opa['b], continuation('a) -> continuation('a)

##register [pure, no-projection:cps, restricted:cps] cont_native \ `cont` : ('a -> _unit) -> continuation('a)

##register [pure, no-projection:cps, restricted:cps] ccont_native \ `ccont` : continuation('b), ('a -> _unit) -> continuation('a)

##register [pure, no-projection, restricted : cps] return \ return_tc : continuation('a), 'a -> void

##register [no-projection, restricted : cps] black_release_barrier \ release_barrier : black_future, 'a -> void

##register [no-projection, restricted : cps] loop_schedule \ loop_schedule : opa['d] -> void

##register [pure] user_return \ return_ : continuation('a), 'a -> void


##register [pure] user_cont : ('a -> void) -> continuation('a)
##args(f)
{
  return cont(f);
}

##register [pure, cps-bypass] user_cont_cps  : ('a, continuation(opa[void]) -> void ), continuation(continuation('a)) -> void
##args(f, k)
{
  var fk = ccont(k, function(a){f(a, ccont(fk, function(){}))});
  return_(k, fk);
}

##register execute : continuation('a), 'a -> void
##args(k, x)
{
  execute(k, x);
  return js_void;
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

  ##register [opacapi] cps0_native : (-> 'b) -> (continuation('b) -> void)
  ##args(f)
  {
    return cps(f)
  }

  ##register [opacapi] cps1_native : ('a -> 'b) -> ('a, continuation('b) -> void)
  ##args(f)
  {
    return cps(f)
  }

  ##register [opacapi] cps2_native : ('a, 'b -> 'c) -> ('a, 'b, continuation('c) -> void)
  ##args(f)
  {
    return cps(f)
  }

  ##register [opacapi]cps3_native : ('a, 'b, 'c -> 'd) -> ('a, 'b, 'c, continuation('d) -> void)
  ##args(f)
  {
    return cps(f)
  }

  ##register [opacapi] cps4_native : ('a, 'b, 'c, 'd -> 'e) -> ('a, 'b, 'c, 'd, continuation('e) -> void)
  ##args(f)
  {
    return cps(f)
  }

  ##register [opacapi] cps5_native : ('a, 'b, 'c, 'd, 'e -> 'f) -> ('a, 'b, 'c, 'd, 'e, continuation('f) -> void)
  ##args(f)
  {
    return cps(f)
  }

##endmodule

##register print_trace : continuation(_) -> void
##args(_)
{
    console.log("BslCps.print_trace", "NYI");
    return ;
}
