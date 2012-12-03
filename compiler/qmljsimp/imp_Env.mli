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

(* the environment that does not vary while compiling *)
type env = {
  options : Qml2jsOptions.t;
  gamma : QmlTypes.gamma;
  annotmap : QmlAst.annotmap;
  val_ : ?side:[`client|`server] -> string -> Ident.t;
  private_bymap : Imp_Bsl.JsImpBSL.ByPassMap.t;
  bsl_lang : BslLanguage.t;
  srenaming : QmlRenamingMap.t;
  is_distant : Ident.t -> bool;
}

(* the environment that is passed around as an accumulator *)
type private_env = {
  local_vars : JsAst.ident list;
  renaming : JsAst.ident IdentMap.t; (* a local renaming of the parameters of functions
                                      * used when squashing together the body of mutually
                                      * recursive functions *)
  no_warn_x : unit;
}

(**
   Dealing with the generation of local variables
*)
val next_exprident : private_env -> Ident.t -> private_env * JsAst.ident
val next : private_env -> string -> private_env * JsAst.ident

(**
   Generation of parameters
*)
val next_param : string -> JsAst.ident

val declare_local_vars : private_env -> private_env * JsAst.statement
val maybe_declare_local_vars : private_env -> private_env * JsAst.statement option

val reset_renaming : private_env -> private_env
