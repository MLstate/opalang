(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(**
   Binding with QOS for managing collectable resources
   @author Mathieu Barbin
   @date Tue Jan  4 19:17:05 CET 2011
*)

module BslNativeLib = OpabslgenMLRuntime.BslNativeLib (* refactoring in progress *)
(** TODO - plugins dependencies *)
##property[mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##property[endmli]

##opa-type tuple_2('a, 'b)
(** *****************************)


(**
   Waiting for the real implementation of QOS, we introduce
   a dummy implementation with a sub part of the interface,
   just for testing purpose.
*)

type signal =
  | Expiration

type void = ServerLib.ty_void

type ('state, 'message, 'result) on_message =
    'state -> 'message -> ('state, 'result) opa_tuple_2 QmlCpsServerLib.continuation -> unit
type 'state expire = 'state -> (signal ServerLib.ty_option) QmlCpsServerLib.continuation -> unit
type 'state collect = 'state -> signal -> void QmlCpsServerLib.continuation -> unit

(**
   For storing heterogenous manager
*)
type state

type ('message, 'result) manager = {
  index : int ;
  mutable state : state ;
  on_message : (state, 'message, 'result) on_message ;
  expire : state expire ;
  collect : state collect ;
}

type black_manager
external black_manager : (_, _) manager -> black_manager = "%identity"
external unblack_manager : black_manager -> (_, _) manager = "%identity"

##extern-type ResourceTracker.signal = signal
##extern-type ResourceTracker.manager('message, 'result) = ('message, 'result) manager

##module signal
  ##register expiration : ResourceTracker.signal
  let expiration = Expiration
##endmodule

let fresh =
  let r = ref 0 in
  (fun () -> incr(r) ; !r)

let manager_map = ref ( IntMap.empty : black_manager IntMap.t )

##register [cps-bypass] create : \
 'state, \
 ('state, 'message, continuation(opa[tuple_2('state, 'result)]) -> void), \
 ('state, continuation(opa[option(ResourceTracker.signal)]) -> void), \
 ('state, ResourceTracker.signal, continuation(opa[void]) -> void), \
 continuation(ResourceTracker.manager('message, 'result)) \
 -> void

let create (state : 'state) on_message expire collect k =
  let state = (Obj.magic (state : 'state) : state) in
  let on_message = (Obj.magic (on_message : ('state, 'message, 'result) on_message) : (state, 'message, 'result) on_message) in
  let expire = (Obj.magic (expire : 'state expire) : state expire) in
  let collect = (Obj.magic (collect : 'state collect) : state collect) in
  let index = fresh () in
  let manager = {
    index ;
    state ;
    on_message ;
    expire ;
    collect ;
  } in
  manager_map := IntMap.add index (black_manager manager) !manager_map ;
  QmlCpsServerLib.return k manager

##register [cps-bypass] call : \
    ResourceTracker.manager('message, 'result), \
      'message, continuation('result) -> void

let call manager message cont =
  let cont_tuple opa_tuple =
    let state, result = BslNativeLib.ocaml_tuple_2 (BslNativeLib.wrap_opa_tuple_2 (unwrap_opa_tuple_2 opa_tuple)) in
    manager.state <- state ;
    QmlCpsServerLib.return cont result
  in
  manager.on_message manager.state message (QmlCpsServerLib.ccont_ml cont cont_tuple)

##register [cps-bypass] term : \
    ResourceTracker.manager(_, _), \
    ResourceTracker.signal, continuation(opa[void]) -> void

let term manager signal cont =
  manager.collect manager.state signal cont

let rec filter_cont
    (filter : int -> 'a -> bool QmlCpsServerLib.continuation -> unit)
    (map : 'a IntMap.t)
    (cont : 'a IntMap.t QmlCpsServerLib.continuation) =
  if IntMap.is_empty map
  then QmlCpsServerLib.return cont map
  else
    let left, key, val_, right = IntMap.decons map in
    let cont_bool bool =
      let cont_left left =
        let cont_right right =
          let left = if bool then IntMap.add key val_ left else left in
          let map = IntMap.merge (fun _ _ -> assert false) left right in
          QmlCpsServerLib.return cont map
        in
        filter_cont filter right (QmlCpsServerLib.ccont_ml cont cont_right)
      in
      filter_cont filter left (QmlCpsServerLib.ccont_ml cont cont_left)
    in
    filter key val_ (QmlCpsServerLib.ccont_ml cont cont_bool)

##register [cps-bypass] garbage_collector : continuation(opa[void]) -> void
let garbage_collector cont =
  let filter_val _ manager cont =
    let manager = unblack_manager manager in
    let state = manager.state in
    let cont_signal opt =
      match ServerLib.unwrap_option opt with
      | None -> QmlCpsServerLib.return cont true
      | Some signal ->
          manager.collect state signal (QmlCpsServerLib.ccont_ml cont ignore) ;
          QmlCpsServerLib.return cont false
    in
    manager.expire state (QmlCpsServerLib.ccont_ml cont cont_signal) ;
  in
  let cont_map map =
    manager_map := map
  in
  filter_cont filter_val !manager_map (QmlCpsServerLib.ccont_ml cont cont_map) ;
  QmlCpsServerLib.return cont ServerLib.void
