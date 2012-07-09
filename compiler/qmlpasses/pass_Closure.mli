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
   Rewrite the directive `closure_create and `closure_apply

   @author Sebastien Briais
   @author Valentin Gatien-Baron
*)

val process_code:
  typed:bool -> (* same remark as Pass_LambdaLifting *)
  side:[`client|`server] ->
  renaming_client:QmlRenamingMap.t ->
  renaming_server:QmlRenamingMap.t ->
  QmlTypes.bypass_typer ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code

(** [args_apply ~typed (gamma, annotmap) clos args]
    Apply the closure [clos] with [args], which must be its last arguments *)
val args_apply : typed:bool ->
  QmlAstCons.TypedExpr.gamma * QmlAstCons.TypedExpr.annotmap ->
  QmlAst.expr -> QmlAst.expr list ->
  QmlAstCons.TypedExpr.annotmap * QmlAst.expr

val generate_applys_js : ?at_least:int -> unit -> (string * JsAst.statement) list
val generate_applys : ?at_least:int -> [`js|`caml] -> string
  (**
     To be called by backends after a call to [process_code]
     to generate the definitions that are used in the code returned
     by [process_code] (it contains the directive @backend_ident)
     At least indicate the least number of clos_applyX functions to
     be generated
  *)

(** Return the closure name from an ident.
    @raise Not_found when the identifier is not in the images of the renaming map
                     when safe is false (which is the default)
*)
val make_closure_name :
  ?safe:bool ->
  side:[`client|`server] ->
  renaming_server:QmlRenamingMap.t ->
  renaming_client:QmlRenamingMap.t ->
  Ident.t -> string
