(*
    Copyright © 2011, 2012 MLstate

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
   Performs 'inlining' of local variables
   (the usage of variables may be inlined at the point of use
    of said variables, but doesn't inline any function)
   Local aliases are also removed
*)
val local_inline_stm : JsAst.statement -> JsAst.statement
val local_inline : JsAst.code -> JsAst.code

(**
   Performs inlining in its usual meaning
*)
type env
val empty_env : env
val env_of_map : Ident.t IdentMap.t -> env
val global_inline_analyse_stm : env -> JsAst.statement -> env
val global_inline_analyse_code : env -> JsAst.code -> env
val global_inline_rewrite_stm : env -> JsAst.statement -> JsAst.statement

(**
   The interface for separate compilation
*)
module type R =
sig
  val load : env -> env
  val save : env:env -> loaded_env:env -> initial_env:env -> unit
end
val make_r : string -> (module R)
