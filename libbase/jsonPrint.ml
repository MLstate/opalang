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
(* depends *)
module List = BaseList

(* shorthands *)
module J = JsonTypes

(* -- *)

module type Printer =
sig
  type t
  val json : t -> JsonTypes.json -> unit
end

let escape_non_utf8_special s =
  let reg_rep_list = [
    Str.regexp "\\", "\\\\\\\\";
    Str.regexp "\"", "\\\\\"";
    Str.regexp "\n" , "\\\\n";
    Str.regexp "\r" , "\\\\r";
    Str.regexp "\t" , "\\\\t";
  ] in
  List.fold_left
    (fun str (reg,rep) ->
       Str.global_replace reg rep str) s reg_rep_list

let print add arg formule =
  let add x = add arg x in
  let rec aux = function
    | J.Int n -> add (string_of_int n)
    | J.Float f -> add (Printf.sprintf "%f" f)
    | J.String s -> add ("\""^(escape_non_utf8_special s)^"\"")
    | J.Bool b -> add (string_of_bool b)
    | J.Void -> add "{}"
    | J.Array jlst ->
        add "[";
        let length = List.length jlst - 1 in
        List.iteri (fun x i -> aux x; if i < length then add ",") jlst;
        add "]";
    | J.Record sjlst ->
        add "{";
        let aux_field (n, x) =
          add "\""; add n; add "\":"; aux x in
        let rec aux = function
          | [x] -> aux_field x
          | t::q -> aux_field t; add ","; aux q
          | _ -> ()
        in aux sjlst; add "}"
  in
  aux formule

let to_string json =
  let fb = Buffer.create 50 in
  print Buffer.add_string fb json ;
  Buffer.contents fb

module Output =
struct
  type t = out_channel
  let json oc json = print Pervasives.output_string oc json
end

module Buffer =
struct
  type t = Buffer.t
  let json buf json = print Buffer.add_string buf json
end
