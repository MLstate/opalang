(*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

module Actor = (val Llsession.make BslScheduler.opa : Llsession.M)

(* Projection continuation('a) => 'a -> void *)
let cont_to_fun k =
  fun x ->
    QmlCpsServerLib.return k x

let acont_to_fun f k =
  fun x ->
    QmlCpsServerLib.return k (f x)

let ocont_to_fun k =
  acont_to_fun ServerLib.wrap_option k

(* Projection 'a -> void => continuation('a) *)
let fun_to_cont f = QmlCpsServerLib.cont_ml f

let afun_to_cont p f =
  QmlCpsServerLib.cont_ml (fun x -> f (p x))

let ofun_to_cont f =
  afun_to_cont ServerLib.unwrap_option f

let vfun_to_cont f =
  afun_to_cont (fun (_:ServerLib.ty_void) -> ()) f

(* cps(-> unit) -> (unit -> unit)*)
let void_cps_to_unit f =
  fun _ -> f (QmlCpsServerLib.cont_ml (fun _ -> ()))

##extern-type Actor.t('ctx, 'msg) = ('msg, 'ctx) Actor.t



##register make : 'state, \
('state, 'msg, option('ctx) -> option('state)), \
option(-> void), \
option('ctx), \
bool \
-> Actor.t('ctx, 'msg)
let make _ _ _ _ _ = assert false

##register [cps-bypass] make_cps : 'state, \
('state, 'msg, option('ctx), continuation(opa[option('state)]) -> void), \
option(continuation(opa[void]) -> void), \
option('ctx), \
bool, \
continuation(Actor.t('ctx, 'msg)) -> void
let make_cps state handler ondelete ctx concurrent k =
  let handler st msg ctx f =
    let k = ofun_to_cont f in
    handler st msg ctx k in
  let ondelete =
    Option.map
      (fun ondelete -> void_cps_to_unit ondelete)
      ondelete in
  let a = Actor.make_cps state ctx handler ondelete concurrent in
  QmlCpsServerLib.return k a

##register post : option('ctx), 'msg , Actor.t('ctx, 'msg) -> void
let post ctx msg actor = Actor.send actor msg ctx
