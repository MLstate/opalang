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
   This pass implements the simplification of magic functions
   in the case when they are used with a known type

   For instance, [compare:int -> int -> int] will be replaced by [compare_int]
*)

(** Type of an environnment for this passes. This environnement is a
    map wich contains bindings beetween the ident to specialize and
    the list of [(ty, expr)] where [expr] is a specialized expression
    for ident and [ty] its type *)
type info = { strict : bool  ; specialize : (QmlAst.ty * QmlAst.expr) list }
type env = info IdentMap.t

val process_code : ?specialized_env:env -> QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code -> QmlAst.annotmap * QmlAst.code
