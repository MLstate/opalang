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
(** This modules provides function to share redundant code.

    @author Quentin Bourgerie
*)

type env

(** Rewrite the JavaScript code according to the given environment. *)
val rewrite : env -> JsAst.code -> JsAst.code

(** Rewrite the JavaScript code according to the given environment. *)
val rewrite_expr : env -> JsAst.expr -> JsAst.expr

(** A very simple code sharing. It shares function which are "syntactically
    identical".
*)
val process_code : pass:string -> JsAst.code -> (JsAst.code * env)

(** Get the substitute ident of the given one *)
val get_substitute : env -> JsIdent.t -> JsIdent.t option
