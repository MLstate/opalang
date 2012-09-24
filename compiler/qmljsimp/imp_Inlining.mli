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
val global_inline_analyse_stm :  ?topobj:(string -> bool) -> env -> JsAst.statement -> env
val global_inline_analyse_code :  ?topobj:(string -> bool) -> env -> JsAst.code -> env
val global_inline_rewrite_stm : env -> JsAst.statement -> JsAst.statement option

(** [map_expr_in_env f env] Map all JavaScript expression in [env] with [f]. *)
val map_expr_in_env : (JsAst.expr -> JsAst.expr) -> env -> env

(** [fold_env f env acc] Fold in the inlining [env] with [f]. *)
val fold_env : (JsAst.ident -> JsAst.expr -> 'a -> 'a) -> env -> 'a -> 'a

(**
   The interface for separate compilation
*)
module type R =
sig
  val load : env -> env
  val save : env:env -> loaded_env:env -> initial_env:env -> unit
end
val make_r : string -> (module R)
