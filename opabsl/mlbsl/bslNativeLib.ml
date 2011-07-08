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
(*
   @author Mathieu Barbin
   @author Louis Gesbert
   @author David Rajchenbach-Teller, 2010
**)


(**
   Conversion between usual OPA data structures and their OCaml equivalent.

   @author Mathieu Barbin
   @author Louis Gesbert
   @author David Rajchenbach-Teller, 2010
*)

(**
   {1 Option}
*)

##opa-type option('a)

let wrap_option proj = function
  | Some a -> wrap_opa_option (ServerLib.some (proj a))
  | None -> wrap_opa_option ServerLib.none

let unwrap_option proj opa =
  let record = unwrap_opa_option opa in
  let opt = ServerLib.unwrap_option record in
  match opt with
  | None -> opt
  | Some v -> Some (proj v)

(**
   {1 Lists}
*)

(**
   Type [list('a)], as known by OPA
*)
##opa-type list('a)

(**
   Type ['a list], as known by OCaml
*)
##extern-type caml_list('a) = 'a list


##register [opacapi] cons : 'a, caml_list('a) -> caml_list('a)
  let cons x l = x :: l

##register [opacapi] empty_list : caml_list('a)
  let empty_list : 'a caml_list = []

##register hd : caml_list('a) -> option('a)
 let hd = function |x::_ -> Some x | _ -> None

##register tl : caml_list('a) -> option(caml_list('a))
 let tl = function |_::y -> Some y | _ -> None

##register caml_list_to_opa_list: ('a -> 'b), caml_list('a) -> opa[list('b)]
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

 ##register opa_list_to_ocaml_list: ('a -> 'b), opa[list('a)] -> caml_list('b)
 let opa_list_to_ocaml_list f l =
  let r = unwrap_opa_list l in
  let rec aux(r,acc) =
      match ServerLib.dot r field_hd with
      | None -> List.rev acc
      | Some a ->
          let tl = ServerLib.unsafe_dot r field_tl in
          aux(tl,(f a)::acc)
  in aux(r,[])

(**
   {1 Tuples}
*)

(**
   caml_tuple_* as known by OCaml
*)
##extern-type caml_tuple_4('a,'b,'c,'d) = ('a*'b*'c*'d)

(**
   tuple_* as known by OPA
*)
##opa-type tuple_2('a, 'b)
##opa-type tuple_3('a, 'b, 'c)
##opa-type tuple_4('a,'b,'c,'d)

let f1 = ServerLib.static_field_of_name "f1"
let f2 = ServerLib.static_field_of_name "f2"
let f3 = ServerLib.static_field_of_name "f3"
let f4 = ServerLib.static_field_of_name "f4"



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

##register ocaml_tuple_4 : opa[tuple_4('a,'b,'c,'d)] -> caml_tuple_4('a,'b,'c,'d)
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

(**
   {1 Continuations}
*)
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation

(**
   {1 Standard Exceptions}
*)

##opa-type exception
##opa-type exception_common

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
