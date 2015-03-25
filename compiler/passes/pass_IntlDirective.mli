(**
 * Copyright © 2015 MLstate
 *
 * This file is part of Opa.
 *
 * Opa is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Affero General Public License, version 3, as published by
 * the Free Software Foundation.
 *
 * Opa is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Opa. If not, see <http://www.gnu.org/licenses/>.
 *)

(** @author Henri Chataing *)

(** Eliminates `intl directives. *)
val process : options:OpaEnv.opa_options -> ((SurfaceAst.nonuid, SurfaceAst.parsing_directive) SurfaceAstPassesTypes.env_both_lcodes as 'env) -> 'env
