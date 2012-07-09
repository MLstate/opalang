(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
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
