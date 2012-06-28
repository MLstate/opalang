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
   Flat Compiler : Compiler Environment
   @author Mathieu Barbin
*)

(**
   The env is used for having access during the compilation to static informations,
   such as bypass map, or typing annotation.
*)

type env = {
  options: Qml2ocamlOptions.argv_options ;
  bymap: Flat_Bsl.FlatBSL.ByPassMap.t ;
  typing: QmlTyper.env ;
}

val initial :
  Qml2ocamlOptions.argv_options ->
  Flat_Bsl.FlatBSL.ByPassMap.t ->
  QmlTyper.env ->
  env

(**
   Return a static string to be inserted
   in the generated code for refering opa source.
   Used e.g. for match failure messages.
*)
val string_of_pos : FilePos.pos -> string

(**
   Map a context for adding the annotmap from the env
*)
val env_context : env -> QmlError.context -> QmlError.context

(**
   Internal error.
*)
val context_error : QmlError.Context.context -> ('a, 'error) OManager.oformat -> 'a
val internal_error : env -> QmlAst.expr -> ('a, 'error) OManager.oformat -> 'a
