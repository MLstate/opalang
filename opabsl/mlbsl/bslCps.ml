(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(**
   CPS runtime server bsl-binding.

   Bypass used by the generated code after the QmlCpsRewriter pass
   some of them should not be projected (in particular functionnal bypass)
   that is the meaning of the bsltag [no-projection]

   This is a binding to QmlCpsServerLib.
   this module is not inlined there in the hope to keep the opportunity to test it
   ouside the bsl. This can anytime later be changed by removing the QmlCpsServerLib module
   by bringing the code there.
*)

(**
   Runtime debug output. Based on {b MLSTATE_CPS_DEBUG}.
   @see 'debugVariables.ml' for details
*)
##register [opacapi] debug : int, string -> void
let debug __minlevel __s =
  #<If:CPS_DEBUG$minlevel __minlevel>
    Printf.fprintf stderr "[34m[Cps][0m %s\n%!" __s
  #<End>


##extern-type Cps.future('a) = 'a QmlCpsServerLib.future

##extern-type [normalize] func('a, 'b) = ('a, 'b) QmlCpsServerLib.func
##extern-type [normalize] black_future = QmlCpsServerLib.black_future

##extern-type [normalize] _unit_continuation = unit QmlCpsServerLib.continuation
##extern-type [normalize] _unit = unit (* the type unit that doesn't get projected *)

let qml_unit = ServerLib.make_record ServerLib.empty_record_constructor

##register [opacapi, no-projection, restricted : cps] wait \ `QmlCpsServerLib.wait` : Cps.future('a), continuation('a) -> void
##register [opacapi, no-projection : cps, restricted : cps] spawn \ `QmlCpsServerLib.spawn` : (_unit, continuation('a) -> _unit) -> Cps.future('a)

(* Fixme: Check with Valentin. extern _unit ?? *)
##register [opacapi, no-projection : cps, restricted : cps] callcc_directive \ `QmlCpsServerLib.callcc_directive` : \
  (continuation('a), _unit_continuation -> _unit), continuation('a) -> _unit

(* thread_context needs a projection because of the returned option *)
##register [opacapi, no-projection : cps] thread_context \ `QmlCpsServerLib.thread_context` : continuation('a) -> option(opa['thread_context])
##register [opacapi, no-projection, restricted : cps] with_thread_context \ `QmlCpsServerLib.with_thread_context` : opa['b]\
    ,continuation('a) -> continuation('a)
##register [opacapi, no-projection, restricted : cps] handler_cont \ `QmlCpsServerLib.handler_cont` : continuation('a) -> continuation('c)
##register [opacapi, no-projection : cps, restricted : cps] catch_native \ `QmlCpsServerLib.catch_ml` : \
  (opa['c], continuation('a) -> _unit), continuation('a) -> continuation('a)
##register [opacapi, no-projection : cps, restricted : cps] catch \ `QmlCpsServerLib.catch` : Closure.t, continuation('a) -> continuation('a)
##register [opacapi, no-projection : cps, restricted : cps] cont_native \ `QmlCpsServerLib.cont_ml` : ('a -> _unit) -> continuation('a)
##register [opacapi, no-projection : cps, restricted : cps] cont \ `QmlCpsServerLib.cont` : Closure.t -> continuation('a)

##register [opacapi, no-projection : cps, restricted : cps] ccont_native \ `QmlCpsServerLib.ccont_ml` : continuation('d), ('a -> _unit) -> continuation('a)
##register [opacapi, no-projection : cps, restricted : cps] ccont \ `QmlCpsServerLib.ccont` : continuation('d), Closure.t -> continuation('a)
##register [opacapi, no-projection, restricted : cps] return \ `QmlCpsServerLib.return` : continuation('a), 'a -> void
##register [opacapi, no-projection, restricted : cps] magic_func \ `QmlCpsServerLib.magic_func` : func('a, 'd) -> func('e, 'f)
##register [opacapi, no-projection, restricted : cps] make_barrier \ `QmlCpsServerLib.make_barrier` : string -> Cps.future('a)
##register [opacapi, no-projection, restricted : cps] black_make_barrier \ `QmlCpsServerLib.black_make_barrier` : string -> black_future
##register [opacapi, no-projection, restricted : cps] release_barrier \ `QmlCpsServerLib.release_barrier` : Cps.future('a), 'a -> void
##register [opacapi, no-projection, restricted : cps] black_release_barrier \ `QmlCpsServerLib.black_release_barrier` : black_future, 'a -> void

##register [opacapi, no-projection, restricted : cps] uncps_native : string, continuation('d), (continuation('a) -> _unit) -> 'a
##register [no-projection, restricted : cps] uncps : string, continuation('d), Closure.t -> 'a
let uncps_ml = QmlCpsServerLib.uncps_ml
let uncps_native = uncps_ml
let uncps = QmlCpsServerLib.uncps

##register [opacapi, no-projection, restricted : cps] before_wait \ `QmlCpsServerLib.before_wait` : -> void

##register [opacapi, no-projection, restricted : cps] toplevel_wait : Cps.future('a) -> 'a
let toplevel_wait b =
  let is_released () = QmlCpsServerLib.is_released b in
  #<If:CPS_BLOCKING_WAIT>
    Format.eprintf "LOOP-UNTIL: %a\n%!" QmlCpsServerLib.print_barrier b;
  #<End>;
  let () = Scheduler.loop_until BslScheduler.opa is_released in
  QmlCpsServerLib.toplevel_wait b

##register [opacapi, no-projection, restricted : cps] black_toplevel_wait : black_future -> 'a
let black_toplevel_wait (black : QmlCpsServerLib.black_future) =
  let b = (( Obj.magic black ) : _ QmlCpsServerLib.future ) in
  toplevel_wait b

(* User ByPass (see, cps.opa) - used by the user, need a projection *)
##register user_return \ `QmlCpsServerLib.return` : continuation('a), 'a -> void

##register execute \ `QmlCpsServerLib.execute` : continuation('a), 'a -> void

##register [cps-bypass] user_cont_cps : ('a, continuation(opa[void]) -> void), continuation(continuation('a)) -> void
let user_cont_cps f k =
  let fk = QmlCpsServerLib.ccont_ml k
    (fun x -> f x (QmlCpsServerLib.ccont_ml k (fun _ -> ()))) in
  QmlCpsServerLib.return k fk

##register user_cont : ('a -> void) -> continuation('a)
let user_cont f = QmlCpsServerLib.cont_ml (fun a -> ignore (f a))

##register [opacapi, no-projection] bt_add \ `QmlCpsServerLib.bt_add` : string -> void
##register [no-projection] bt_take \ `QmlCpsServerLib.bt_take` : -> string
##register [opacapi, no-projection] fun_args2string \ `QmlCpsServerLib.fun_args2string` : string, 'a -> string

(* The argument is the first world displayed, e.g. "Raised" or "Interrupted" *)
##register [no-projection] display_backtrace \ `QmlCpsServerLib.display_backtrace` : string -> void

##register update_cont \ `QmlCpsServerLib.update_cont` : continuation('a), option(continuation(_)), string, string, _ -> continuation('a)
##register print_trace \ `QmlCpsServerLib.print_trace` : continuation('a) -> void

##register [no-projection, restricted : cps] loop_schedule : opa['a] -> void
let loop_schedule _ = Scheduler.run BslScheduler.opa

(** Notcps_compatibility : add a dummy implementation for some primitive,
    so that the same (qml/opa) code using cps features can be compiled
    using them, without changing its semantic without --cps mode *)

##module Notcps_compatibility
  let fatal_error = fun s -> Logger.critical "%s" s; BslSys.do_exit 1

  ##register [opacapi, no-projection, restricted : cps] dummy_cont : continuation(void)
  let dummy_cont = QmlCpsServerLib.cont_ml (fun x -> x)

  (** return always None *)
  ##register [opacapi] thread_context : opa['g] -> option(opa['c])
  let thread_context _ = None

  (** identity, ignore the context *)
  ##register with_thread_context : opa['g], 'a -> 'a
  let with_thread_context _ a = a

  ##register callcc_directive : (continuation('a) -> void) -> 'a
  let callcc_directive f =
    QmlCpsServerLib.uncps_ml "CheatedCALLCC"
      (QmlCpsServerLib.cont_ml (fun () -> ()))
      f

  ##register [opacapi] cps0_native : (-> 'b) -> (continuation('b) -> void)
  let cps0_native f = (fun k -> QmlCpsServerLib.return k (f ()))

  ##register [opacapi] cps1_native : ('a -> 'b) -> ('a, continuation('b) -> void)
  let cps1_native f = (fun x k -> QmlCpsServerLib.return k (f x))

  ##register [opacapi] cps2_native : ('a, 'b -> 'c) -> ('a, 'b, continuation('c) -> void)
  let cps2_native f = (fun x0 x1 k -> QmlCpsServerLib.return k (f x0 x1))

  ##register [opacapi]cps3_native : ('a, 'b, 'c -> 'd) -> ('a, 'b, 'c, continuation('d) -> void)
  let cps3_native f = (fun x0 x1 x2 k -> QmlCpsServerLib.return k (f x0 x1 x2))

  ##register [opacapi] cps4_native : ('a, 'b, 'c, 'd -> 'e) -> ('a, 'b, 'c, 'd, continuation('e) -> void)
  let cps4_native f = (fun x0 x1 x2 x3 k -> QmlCpsServerLib.return k (f x0 x1 x2 x3))

  ##register [opacapi] cps5_native : ('a, 'b, 'c, 'd, 'e -> 'f) -> ('a, 'b, 'c, 'd, 'e, continuation('f) -> void)
  let cps5_native f = (fun x0 x1 x2 x3 x4 k -> QmlCpsServerLib.return k (f x0 x1 x2 x3 x4))

##endmodule
