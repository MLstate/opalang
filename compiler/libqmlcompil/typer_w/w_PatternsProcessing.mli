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
   @author François Pessaux
*)


val merge_patterns_types :
  W_TypingEnv.t -> W_Algebra.simple_type list -> pat_match_expr: QmlAst.expr ->
  matched_expr_ty: W_Algebra.simple_type ->
  patterns_types: W_Algebra.simple_type list -> W_Algebra.simple_type
