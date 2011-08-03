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
val code : JsLex.token Stream.t -> JsAst.code
val expr : JsLex.token Stream.t -> JsAst.expr
val stm : JsLex.token Stream.t -> JsAst.statement

type error
exception Exception of error
val pp : Format.formatter -> error -> unit

module String :
sig
  val code : ?throw_exn:bool -> string -> JsAst.code
  val expr : ?throw_exn:bool -> ?globalize:bool -> string -> JsAst.expr
  val stm : ?throw_exn:bool -> string -> JsAst.statement
end
module File :
sig
  val code : ?throw_exn:bool -> string -> JsAst.code
  val expr : ?throw_exn:bool -> ?globalize:bool -> string -> JsAst.expr
  val stm : ?throw_exn:bool -> string -> JsAst.statement
end
