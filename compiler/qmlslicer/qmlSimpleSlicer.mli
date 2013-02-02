(*
    Copyright © 2011, 2012 MLstate

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
type splitted_code = {
  code : QmlAst.code ;

  published : Pass_ExplicitInstantiation.published_map;
  (**
     the set of identifiers that can be called from the other side
     [None] means they weren't rewritten by explicit instantiation
     [Some (ident,annot)] means they were rewritten by explicit instantiation
     and [ident] is the "unoptimized" identifier that takes all the type variables
     of its schema instead of only the necessary type variables (and the annotation
     contains its type of course)
  *)

  original_renaming : QmlRenamingMap.t ;

  renaming : QmlRenamingMap.t ;
}

module Options : sig
  (** command line options specs for the slicer *)
  val list : (string * Base.Arg.spec * string) list
end

val get_renaming : ObjectFiles.package -> side:[`client|`server] -> Ident.t IdentMap.t

val process_code :
  test_mode:bool ->
  dump:bool ->
  typer_env:QmlTyper.env ->
  stdlib_gamma:QmlTypes.gamma ->
  client_bsl_lang:BslLanguage.t ->
  server_bsl_lang:BslLanguage.t ->
  bymap:BslLib.BSL.ByPassMap.t ->
  modules:IdentSet.t ->
  code:QmlAst.code ->
  QmlTypes.gamma
  * QmlTyper.env
  * splitted_code (* client_code *)
  * splitted_code (* server_code *)

(**
   {5 Warnings}
*)
val warning_set : WarningClass.Set.t
