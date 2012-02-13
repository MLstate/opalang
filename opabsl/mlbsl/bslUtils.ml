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
(** This module provide some common utils and types for bsl modules : projection, ...*)

(* Field names, used when creating OPA-understandable records.*)
let fnone    = ServerLib.static_field_of_name "none"
let fsome    = ServerLib.static_field_of_name "some"

let fkey     = ServerLib.static_field_of_name "key"
let freq     = ServerLib.static_field_of_name "request"
let fcon     = ServerLib.static_field_of_name "connexion"
let fdetails = ServerLib.static_field_of_name "details"
let fconstraint = ServerLib.static_field_of_name "constraint"

let fclient  = ServerLib.static_field_of_name "client"
let fserver  = ServerLib.static_field_of_name "server"
let fnothing = ServerLib.static_field_of_name "nothing"

let fsuccess = ServerLib.static_field_of_name "success"
let ffailure = ServerLib.static_field_of_name "failure"

let ffree    = ServerLib.static_field_of_name "free"
let fno_client_calls = ServerLib.static_field_of_name "no_client_calls"

let rnone    = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fnone) (ServerLib.make_record ServerLib.empty_record_constructor))
let rsome x  = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fsome) x)

let rclient x  = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fclient) x)
let rserver x  = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fserver) x)
let rnothing    = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (fnone) (ServerLib.make_record ServerLib.empty_record_constructor))

let rfree = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor (ffree) (ServerLib.make_record ServerLib.empty_record_constructor))

##opa-type ThreadContext.t

##opa-type ThreadContext.client

##opa-type outcome('a, 'b)

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
(* ThreadContext: Be careful, Thread context is hard coded here. Change it if you change type *)
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
  let rconstraint = rfree in
  ServerLib.make_record
    (ServerLib.add_field
      (ServerLib.add_field
        (ServerLib.add_field
          (ServerLib.add_field ServerLib.empty_record_constructor (fkey)  rkey)
          (freq) rreq)
        (fdetails) rdetails)
      (fconstraint) rconstraint)

let get_serverkey context =
  let rkey = ServerLib.unsafe_dot context fkey in
  ServerLib.dot rkey fserver

let create_outcome = function
  | `success success ->
      wrap_opa_outcome
        (ServerLib.make_record
           (ServerLib.add_field ServerLib.empty_record_constructor
              fsuccess success))
  | `failure failure ->
      wrap_opa_outcome
        (ServerLib.make_record
           (ServerLib.add_field ServerLib.empty_record_constructor
              ffailure failure))
