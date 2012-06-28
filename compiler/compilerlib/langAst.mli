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
   Common tools for LangAst modules.
   @author Mathieu Barbin
*)

(** {6 Positions} *)

(**
   If you ast follows the guidelines about positions,
   you can use this functions.

   The AST should be define with the pos in position 0,
   in each case of the sum.

   AST following this guidelines are :
   + BslTypes.t
   + QmlTopValue.t

   If your ast comports also annotation, you can use functions
   defined in module [Annot].
*)
(** *)
type pos = FilePos.pos
(** *)
val pos : 'a -> pos
val reset_pos : 'a -> pos -> 'a
val merge_pos : 'a -> pos -> 'a

(** {6 Pattern Matching on AST} *)

(**
   You should provide in comments just near the AST definition,
   for each sum type with several constructor a template for
   matching the AST, with choosen names, on verbose, and one short.

   You should try to update old code step by step,
   but you MUST respect this in new code.

   AST following this guidelines are :
   + BslTypes.t
   + QmlTopValue.t
*)

(**
   {6 Guidelines. Request for comments and contributions.}

   Whenever you encounter a bad case where guidelines are not
   so easy to follow, use your imagination to find something cool :)
   to apply instead, and maybe add a remark there.

   {9 Variable Names}

   If you variable has the type of a named type, it should be called
   like the type.

   [ty], [expr], [code_elt], [code], etc....

   hd, tl

   Collections rules for name :

   x list : x -> x_list or xs
   x map : x -> x_map, or xs
   x set : x -> x_set, or xs

   + const : [k]
   + ident : [id]
   + field : [fd]
   + formatter : [fmt]
   + param : [pm]
   + pos : [p]

   e.g.

   a list of field, shorter ==> fds
   a map of (ident list) ==> ids_map

   Printers : pp

   Avoid too short names for large scope.
   The length of the variable is often proportional to the size of its scope.
*)
