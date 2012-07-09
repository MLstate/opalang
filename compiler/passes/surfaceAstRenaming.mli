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
(**
   Alpha renaming on the SurfaceAst
*)

(**
   The warning produced by this pass.
   Applications are free to load are not these warnings.
*)
val warning_set : WarningClass.Set.t

val get_tuple_int_map : unit -> Ident.t IntMap.t
(**
   [get_tuple_int_map] returns a map containing a binding from each [n]
   to the the identifier [tuple_n] (since these types are defined implicitely)
*)

val set_tuple_int_map : Ident.t IntMap.t -> unit
(**
   temporary, meant for separate compilation while the linking is between
   checkDuplication and insertTupleTypes
*)

type env
(**
   The renaming environment
*)

val init_env : string list -> string list -> env
(**
   [init_env names types] initializes the environment with the given list of names
   and list of types.
   It allows to use names in the standard library and insert their defininitions
   during the compilation (like the version number of the compiler for instance)
*)

val code :
  env -> (* the initial environment *)
  (string, SurfaceAst.renaming_directive) SurfaceAst.code -> (* the code to be renamed*)
    env *  (* the final environment *)
    (Ident.t, SurfaceAst.dependency_directive) SurfaceAst.code (* the renamed code *)
(**
   The main function that renames all the identifiers in the code: identifiers, type identifiers,
   type variables.
*)

val save_env : env -> unit
val load_env : env -> env
val get_exported_values : env -> IdentSet.t

module ObjectType : ObjectFiles.R with type t = (Ident.t * FilePos.pos) StringMap.t and type 'a wrapper = 'a
val extract_types_in_scope : env -> ObjectType.t
