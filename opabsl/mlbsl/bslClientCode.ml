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
(**
   This module contains the deserialization of the client code
   from a string directly into the opa client ast
   The corresponding serialization is in Qmljs_serializer
 *)

##register unser_adhoc : \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[llarray(string)] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string], opa[string] -> opa[_]), \
    (opa[_], opa[_], opa[_], opa[_] -> _), \
    string -> \
    _
(* rpc, rpcdef, rpcuse, type, typedef, typeuse, set_distant, verbatim, ident, key, key_ident, code_elt*)
##register unser_server : \
    (opa[option(string)], \
     opa[_], \
     opa[option(string)], \
     opa[llarray(string)], \
     opa[_], \
     opa[llarray(string)], \
     opa[llarray(string)] -> \
     opa[_]), \
    (opa[string] -> opa[_]), \
    (opa[string] -> opa[_]), \
    string -> \
    opa[llarray(_)]
(* code_elt, rpc_def, type_def *)
##register[opacapi] serialize_string_length : string -> string

let ser_int b i = (* DIRTY DIRTY copy pasting *)
  for j = 64 / 8 - 1 downto 0 do
    Buffer.add_char b (Option.get (BslChar.chr ((i lsr (j*8)) mod 256)));
  done

let serialize_string_length s =
  let b = Buffer.create 10 in
  ser_int b (String.length s);
  Buffer.contents b

let s_nothing = ServerLib.make_simple_record (ServerLib.static_field_of_name "nothing")

type input = {string : string; mutable pos : int}
let end_of_input input =
  input.pos = String.length input.string
let input_char input =
  let c = input.string.[input.pos] in
  input.pos <- input.pos + 1;
  c
let input_byte input = BslChar.code (input_char input)
let really_input input s n len =
  String.blit input.string input.pos s n len;
  input.pos <- input.pos + len
let unser_int input =
  let acc = ref 0 in
  for j = 0 to 8 - 1 do
    acc := !acc * 256 + input_byte input
  done;
  !acc
let unser_string input =
  let length = unser_int input in
  let s = String.create length in
  really_input input s 0 length;
  ServerLib.wrap_string s
let unser_bool_ref input =
  match input_char input with
  | '\000' -> BslReference.create ServerLib.false_
  | '\001' -> BslReference.create ServerLib.true_
  | _ -> assert false
let unser_array unser_a input =
  let length = unser_int input in
  let acc = LowLevelArray.create length (Obj.magic 0) in
  for i = 0 to length - 1 do
    LowLevelArray.set acc i (unser_a input)
  done;
  acc
let unser_option unser_a input =
  match input_char input with
  | '\000' -> ServerLib.none
  | '\001' -> ServerLib.some (unser_a input)
  | _ -> assert false
let unser unser_a input =
  try
    let r = unser_a input in
    assert (end_of_input input);
    r
  with e ->
    Printexc.print_backtrace stdout;
    Printf.printf "BslClientCode: Parsing error at %d (strings outputted in ./parsed_string)\n%!" input.pos;
    let a = open_out "parsed_string" in
    Printf.fprintf a "%s" input.string;
    Printf.fprintf a "\n\n\n\n";
    Printf.fprintf a "%S" input.string;
    close_out a;
    raise e

let unser_adhoc rpc rpcdef rpcuse type_ typedef typeuse set_distant verbatim ident key key_ident code_elt string =

  let input = {string; pos = 0} in
  let unser_root = unser_bool_ref in

  let unser_key_ident input =
    match input_char input with
    | '\000' -> Obj.magic (key (unser_string input))
    | '\001' -> Obj.magic (ident (unser_string input))
    | '\002' ->
        let key = unser_string input in
        let ident = unser_string input in
        key_ident key ident
    | _ -> assert false in
  let unser_mini_expr input =
    match input_char input with
    | '\000' -> Obj.magic (verbatim (unser_string input))
    | '\001' -> Obj.magic (ident (unser_string input))
    | '\002' -> Obj.magic (verbatim (unser_string input))
    | '\003' -> Obj.magic (set_distant (unser_array unser_string input))
    | '\004' -> Obj.magic (rpcdef (unser_string input))
    | '\005' -> Obj.magic (rpcuse (unser_string input))
    | '\006' -> Obj.magic (typedef (unser_string input))
    | '\007' -> Obj.magic (typeuse (unser_string input))
    | _ -> assert false in
  let unser_content input =
    unser_array unser_mini_expr input in
  let unser_definition input =
    match input_char input with
    | '\000' -> Obj.magic s_nothing
    | '\001' -> Obj.magic (rpc (unser_string input))
    | '\002' -> Obj.magic (type_ (unser_string input))
    | _ -> assert false in
  let unser_code_elt input =
    let content = unser_content input in
    let definition = unser_definition input in
    let ident = unser_key_ident input in
    let root = unser_root input in
    (Obj.magic code_elt : _ -> _ -> _ -> _ -> _) content definition ident root in
  let unser_code input =
    unser_array unser_code_elt input in

  Obj.magic (unser unser_code input)

let unser_server code_elt rpc type_ string =
    let input = {string; pos = 0} in
    let unser_root = unser_bool_ref in
    let unser_rpc_key = unser_string in
    let unser_type_key = unser_string in
    let unser_ident = unser_string in
    let unser_ident_field input = unser_option unser_ident input in
    let unser_defines input =
      match input_char input with
      | '\000' -> Obj.magic s_nothing
      | '\001' -> Obj.magic (rpc (unser_rpc_key input))
      | '\002' -> Obj.magic (type_ (unser_type_key input))
      | _ -> assert false in
    let unser_ident_deps input =
      unser_array unser_ident input in
    let unser_rpc_deps input =
      unser_array unser_rpc_key input in
    let unser_type_deps input =
      unser_array unser_type_key input in
    let unser_code_elt input =
      let client_equivalent = unser_ident_field input in
      let defines = unser_defines input in
      let ident = unser_ident_field input in
      let ident_deps = unser_ident_deps input in
      let root = unser_root input in
      let rpc_deps = unser_rpc_deps input in
      let type_deps = unser_type_deps input in
      (Obj.magic code_elt : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _) client_equivalent defines ident ident_deps root rpc_deps type_deps in
    let unser_code input =
      unser_array unser_code_elt input in

    unser unser_code input
