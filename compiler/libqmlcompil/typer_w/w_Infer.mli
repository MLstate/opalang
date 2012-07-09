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
(*
   @author Fran�ois Pessaux
*)

(** @raise W_InferErrors.Infer_detailled_unification_conflict
    @raise QmlTyperException.Exception *)
val infer_expr_type:
  bypass_typer: QmlTypes.bypass_typer -> W_TypingEnv.t -> QmlAst.expr ->
  (W_Algebra.simple_type * W_AnnotMap.annotmap)
