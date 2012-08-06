(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

##opa-type option('a)

##opa-type list('a)

(**
   caml_tuple_* as known by OCaml
*)
##property[mli]
##extern-type caml_tuple_2('a,'b) = ('a*'b)
##extern-type caml_tuple_3('a,'b,'c) = ('a*'b*'c)
##extern-type caml_tuple_4('a,'b,'c,'d) = ('a*'b*'c*'d)
##extern-type caml_tuple_5('a,'b,'c,'d,'e) = ('a*'b*'c*'d*'e)
##property[endmli]

(**
   tuple_* as known by OPA
*)
##opa-type tuple_2('a,'b)
##opa-type tuple_3('a,'b,'c)
##opa-type tuple_4('a,'b,'c,'d)
##opa-type tuple_5('a,'b,'c,'d,'e)

##opa-type exception
##opa-type exception_common


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

let wrap_option proj = function
  | Some a -> wrap_opa_option (ServerLib.some (proj a))
  | None -> wrap_opa_option ServerLib.none

let unwrap_option proj opa =
  let record = unwrap_opa_option opa in
  let opt = ServerLib.unwrap_option record in
  match opt with
  | None -> opt
  | Some v -> Some (proj v)


let field_nil = ServerLib.static_field_of_name "nil"
let field_hd  = ServerLib.static_field_of_name "hd"
let field_tl  = ServerLib.static_field_of_name "tl"
let caml_list_to_opa_list converter l =
  let empty = ServerLib.make_simple_record field_nil in
  let rcons tl hd =
    let cons = ServerLib.empty_record_constructor in
    let cons = ServerLib.add_field cons field_hd (converter hd) in
    let cons = ServerLib.add_field cons field_tl tl in
    ServerLib.make_record cons
  in
  wrap_opa_list (List.fold_left rcons empty (List.rev l))


let opa_list_to_ocaml_list f l =
  let r = unwrap_opa_list l in
  let rec aux(r,acc) =
    match ServerLib.dot r field_hd with
    | None -> List.rev acc
    | Some a ->
        let tl = ServerLib.unsafe_dot r field_tl in
        aux(tl,(f a)::acc)
  in aux(r,[])

let f1 = ServerLib.static_field_of_name "f1"
let f2 = ServerLib.static_field_of_name "f2"
let f3 = ServerLib.static_field_of_name "f3"
let f4 = ServerLib.static_field_of_name "f4"
let f5 = ServerLib.static_field_of_name "f5"

let ocaml_tuple_2 opa =
  let record = unwrap_opa_tuple_2 opa in
  let a = ServerLib.unsafe_dot record f1 in
  let b = ServerLib.unsafe_dot record f2 in
  (a, b)

let opa_tuple_2 (a, b) =
  let record =
    let acc = ServerLib.empty_record_constructor in
    let acc = ServerLib.add_field acc f1 a in
    let acc = ServerLib.add_field acc f2 b in
    ServerLib.make_record acc
  in
  wrap_opa_tuple_2 record

let ocaml_tuple_3 opa =
  let record = unwrap_opa_tuple_3 opa in
  let a = ServerLib.unsafe_dot record f1 in
  let b = ServerLib.unsafe_dot record f2 in
  let c = ServerLib.unsafe_dot record f3 in
  (a, b, c)

let opa_tuple_3 (a, b, c) =
  let record =
    let acc = ServerLib.empty_record_constructor in
    let acc = ServerLib.add_field acc f1 a in
    let acc = ServerLib.add_field acc f2 b in
    let acc = ServerLib.add_field acc f3 c in
    ServerLib.make_record acc
  in
  wrap_opa_tuple_3 record

let ocaml_tuple_4 opa =
  let record = unwrap_opa_tuple_4 opa in
  let a = ServerLib.unsafe_dot record f1 in
  let b = ServerLib.unsafe_dot record f2 in
  let c = ServerLib.unsafe_dot record f3 in
  let d = ServerLib.unsafe_dot record f4 in
  (a, b, c, d)

let opa_tuple_4 (a, b, c, d) =
  let record =
    let acc = ServerLib.empty_record_constructor in
    let acc = ServerLib.add_field acc f1 a in
    let acc = ServerLib.add_field acc f2 b in
    let acc = ServerLib.add_field acc f3 c in
    let acc = ServerLib.add_field acc f4 d in
    ServerLib.make_record acc
  in
  wrap_opa_tuple_4 record

let ocaml_tuple_5 opa =
  let record = unwrap_opa_tuple_5 opa in
  let a = ServerLib.unsafe_dot record f1 in
  let b = ServerLib.unsafe_dot record f2 in
  let c = ServerLib.unsafe_dot record f3 in
  let d = ServerLib.unsafe_dot record f4 in
  let e = ServerLib.unsafe_dot record f5 in
  (a, b, c, d, e)

let opa_tuple_5 (a, b, c, d, e) =
  let record =
    let acc = ServerLib.empty_record_constructor in
    let acc = ServerLib.add_field acc f1 a in
    let acc = ServerLib.add_field acc f2 b in
    let acc = ServerLib.add_field acc f3 c in
    let acc = ServerLib.add_field acc f4 d in
    let acc = ServerLib.add_field acc f5 e in
    ServerLib.make_record acc
  in
  wrap_opa_tuple_5 record

module OpaExc =
struct
  (**
     Keep synchronized with stdlib.core/exception.opa
  *)

  let f_fail = ServerLib.static_field_of_name "fail"
  let f_position = ServerLib.static_field_of_name "position"
  let fail ~message ~position =
    let r = ServerLib.empty_record_constructor in
    let r = ServerLib.add_field r f_fail (ServerLib.wrap_string message) in
    let r = ServerLib.add_field r f_position (ServerLib.wrap_string position) in
    wrap_opa_exception (ServerLib.make_record r)

  let f_transaction_failure = ServerLib.static_field_of_name "Transaction_failure"
  let transaction_failure = ServerLib.make_simple_record f_transaction_failure

  let f_ocaml_exc = ServerLib.static_field_of_name "ocaml_exc"
  let f_bslkey = ServerLib.static_field_of_name "bslkey"
  let ocaml_exc bslkey exc =
    let message = Printexc.to_string exc in
    let r = ServerLib.empty_record_constructor in
    let r = ServerLib.add_field r f_ocaml_exc (ServerLib.wrap_string message) in
    let r = ServerLib.add_field r f_bslkey (ServerLib.wrap_string bslkey) in
    wrap_opa_exception (ServerLib.make_record r)


  (**
     Keep synchronized with stdlib.core.rpc.core/oparpc.opa
  *)
  let f_OpaRPC_Server = ServerLib.static_field_of_name "OpaRPC_Server"
  let f_timeout = ServerLib.static_field_of_name "timeout"
  let f_client = ServerLib.static_field_of_name "client"
  let f_fun_id = ServerLib.static_field_of_name "fun_id"

  module OpaRPC =
  struct
    (*
      client : Client.key from BslRPC
      fun_id : the name of the distant function
    *)
    let timeout client fun_id =
      let timeout = ServerLib.empty_record_constructor in
      let timeout = ServerLib.add_field timeout f_client client in
      let timeout =
        ServerLib.add_field timeout f_fun_id (ServerLib.wrap_string fun_id) in
      let timeout = ServerLib.make_record timeout in
      let rpc = ServerLib.empty_record_constructor in
      let rpc = ServerLib.add_field rpc f_timeout timeout in
      let rpc = ServerLib.make_record rpc in
      let exc = ServerLib.empty_record_constructor in
      let exc = ServerLib.add_field exc f_OpaRPC_Server rpc in
      ServerLib.make_record exc
  end
end
