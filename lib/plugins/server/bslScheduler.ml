(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(** TODO - plugins dependencies *)
##property[mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##property[endmli]
(** *****************************)

##extern-type Scheduler.key = Scheduler.async_key

module BslUtils = OpabslgenMLRuntime.BslUtils

let opa = Scheduler.default

let push f = Scheduler.push opa f

let at_exit f = Scheduler.at_exit opa f

let asleep t f =
  Scheduler.sleep opa (Time.milliseconds t) f

let sleep t f = ignore(asleep t f)

let timer t f = Scheduler.timer opa (Time.milliseconds t) f

let abort a = Scheduler.abort opa a

let set_max_compute_successive n = Scheduler.set_max_compute_successive n

let set_nb_step_apply n = QmlCpsServerLib.set_nb_step_apply n

(* BSL REGISTERING *)
##register sleep : int, (-> void) -> void

##register asleep : int, (-> void) -> Scheduler.key

##register abort : Scheduler.key -> void

##register [cps-bypass] sleep_cps : int, (continuation(opa[void]) -> void), continuation(opa[void]) -> void
let sleep_cps t f k =
  sleep t (BslUtils.proj_cps0 k f);
  QmlCpsServerLib.return k ServerLib.void

##register [cps-bypass] asleep_cps : int, (continuation(opa[void]) -> void), continuation(opa[Scheduler.key]) -> void
let asleep_cps t f k =
  QmlCpsServerLib.return k (asleep t (BslUtils.proj_cps0 k f))


##register timer : int, (-> void) -> void

##register [cps-bypass] timer_cps : int, (continuation(opa[void]) -> void), continuation(opa[void]) -> void
let timer_cps t f k =
  timer t (BslUtils.proj_cps0 k f);
  QmlCpsServerLib.return k ServerLib.void


##register push : (-> void) -> void

##register [cps-bypass] push_cps : (continuation(opa[void]) -> void), continuation(opa[void]) -> void
let push_cps f k =
  push (BslUtils.proj_cps0 k f);
  QmlCpsServerLib.return k ServerLib.void

##register at_exit : (-> void) -> void

##register [cps-bypass] at_exit_cps : (continuation(opa[void]) -> void), continuation(opa[void]) -> void
let at_exit_cps f k =
  at_exit (BslUtils.proj_cps0 k f);
  QmlCpsServerLib.return k ServerLib.void

##register finalize : ('a -> void), 'a -> void
let finalize f v = Scheduler.finalise opa f v

##register [cps-bypass] finalize_cps : (opa['a], continuation(opa[void]) -> void), opa['a], continuation(opa[void]) -> void
let finalize_cps f v k =
  finalize (BslUtils.proj_cps k f) v;
  QmlCpsServerLib.return k ServerLib.void

##register set_max_compute_successive : int -> void

##register set_nb_step_apply : int -> void
