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
(*
   @author Rudy Sicard
   @author Raja Boujbel
**)

(** Main Json module for serialize and unserialize Json. *)

module J = JsonTypes

(**
   @deprecated use [JsPrint.escape_non_utf8_special]
*)
let escape_non_utf8_special = JsonPrint.escape_non_utf8_special

(*
  extract from opa
  let rpl_list = [
           ( "\\" , "\\\\");
           ( "\n" , "\\n" );
           ( "\r" , "\\r" );
           ( "\t" , "\\t" );
           ( "\"" , "\\\"" )
   ] in
   Base.List.foldl ( fun (pat, rpl) str -> String.replace pat rpl str)  rpl_list v
*)

(** Unserialize string to json. *)
let from_string str =
  (*DEBUG - print_tokens str;*)
  let buffer = Ulexing.from_utf8_string str in
  let res =
    (* Trick from Alain Frisch to use Ulex with OCamlyacc *)
    (* http://caml.inria.fr/pub/ml-archives/caml-list/2005/01/52cbc2cd2be4fc7ea0f00c39a760bf59.en.html *)
    JsonParse.json (fun _ -> JsonLex.get_token buffer) (Lexing.from_string "dummy")
  in res

(**
   @deprecated use [JsPrint.Buffer.json]
*)
let to_buffer = JsonPrint.Buffer.json

(**
   @deprecated use [JsPrint.to_string]
*)
let to_string = JsonPrint.to_string
