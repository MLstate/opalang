(*
    Copyright © 2011 MLstate

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
(** This module provide some common utils and types for bsl modules : projection, ...*)

(* Field names, used when creating OPA-understandable records.*)
let fnone    = ServerLib.static_field_of_name "none"
let fsome    = ServerLib.static_field_of_name "some"

let fkey     = ServerLib.static_field_of_name "key"
let freq     = ServerLib.static_field_of_name "request"
let fcon     = ServerLib.static_field_of_name "connexion"
let fdetails = ServerLib.static_field_of_name "details"

let fclient  = ServerLib.static_field_of_name "client"
let fserver  = ServerLib.static_field_of_name "server"
let fnothing = ServerLib.static_field_of_name "nothing"

let rnone    = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fnone) (ServerLib.make_record ServerLib.empty_record_constructor))
let rsome x  = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fsome) x)

let rclient x  = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fclient) x)
let rserver x  = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fserver) x)
let rnothing    = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fnone) (ServerLib.make_record ServerLib.empty_record_constructor))

##opa-type ThreadContext.t

##opa-type ThreadContext.client

(** Project an ['a -> void] opa function rewrited by cps to an ['a ->
    unit] ml function, usefull for [cps-bypass].

    ['a, continuation(opa[void]) -> void] => ['a -> unit]

    @param pk Parent continuation
*)
let proj_cps pk f =
  fun x -> f x (QmlCpsServerLib.ccont_ml pk (fun (_:ServerLib.ty_void) -> ()))

let proj_cps0 pk f =
  fun () -> f (QmlCpsServerLib.ccont_ml pk (fun (_:ServerLib.ty_void) -> ()))

(** Create an opa thread context *)
let create_ctx key request =
  let rkey = match key with
  | `client client -> rclient client
  | `server server -> rserver server
  | `nothing -> rnothing in
  let rreq = match request with
  | None -> rnone
  | Some (req, con) ->
      let req (*opa HttpRequest.request *) =
        ServerLib.make_record (
          ServerLib.add_field (
            ServerLib.add_field
              ServerLib.empty_record_constructor
              freq req)
            fcon con)
      in
      rsome req in
  let rdetails = rnone in
  ServerLib.make_record (
    ServerLib.add_field(
      ServerLib.add_field
        (ServerLib.add_field ServerLib.empty_record_constructor
           (fkey)  rkey
        )  (freq)  rreq
    )      (fdetails) rdetails
  )

let get_serverkey context =

  let rkey = ServerLib.unsafe_dot context fkey in
  ServerLib.dot rkey fserver
