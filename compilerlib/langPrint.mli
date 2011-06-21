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
   Common tools for LangPrint modules.
   @author Mathieu Barbin
*)

(** {6 Common types} *)
type 'a pprinter = 'a Base.Format.pprinter

(** {6 Common utils} *)


(**
   Used for outputing parametrized types.
   e.g.
   {[
   (* if params is a list of opa types : int, string, 'a *)

   Format.printf "type %a" (pp_parameters pp_opa "toto") params ;

   (* will print : "type toto(int, string, 'a)" *)
   ]}

   For outputting with the ocaml like syntax (prefix), use
   [OcamlPrint.pp_parameters]
*)
val pp_parameters :
  'a pprinter ->
  string ->
  'a list pprinter

(**
   {[
   Format.fprintf fmt "{ %a }" (pp_field_cp " : " pp_value) ("toto", value)
   ]}
*)
val pp_field_cp :
  ('a, Format.formatter, unit) format ->
  'b pprinter ->
  Format.formatter -> (string * 'b) -> unit

(**
   {[
   Format.fprintf fmt "{ %a }" (StringMap.pp " ; " (pp_field " = " pp_value)) fields
   ]}
*)
val pp_field :
  ('a, Format.formatter, unit) format ->
  'b pprinter ->
  Format.formatter -> string -> 'b -> unit


(**
   TODO: a pretty printer for fields (common to Qml, Opa, Ocaml, BslTypes, QmlTopValue)

   For long records :
   {[
   type toto('t1, 't2, ...) = {
     v1 : 't1 ;
     v2 : 't2 ;
     ...
   }

   let toto = {
     v1 = x1 ;
     v2 = x2 ;
     ...
   }
   ]}

   For short records
   {[
   type toto('t1, 't2) = { v1 : 't1 ; v2 : 't2 }
   let toto = { v1 = x1 ; v2 = x2 }
   ]}

   pp_fields sep etc...
*)
