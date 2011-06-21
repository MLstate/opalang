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


(**)

type ordered_quantif = 
    (QmlTypeVars.TypeVar.t list, QmlTypeVars.RowVar.t list, QmlTypeVars.ColVar.t list) QmlTypeVars.generic_quantif

(* generic type scheme, parameterized by types and normal form constraints *)
type ('t, 'c) tsc

val import : QmlTypeVars.FreeVars.t -> 't -> 'c -> ('t, 'c) tsc

val export_unsafe : ('t, 'c) tsc -> QmlTypeVars.FreeVars.t * 't * 'c

(* consults and updates a cache reference inside the argument tsc *)
val freevars_with_cache : ('t -> 'c -> QmlTypeVars.FreeVars.t) -> ('t, 'c) tsc -> QmlTypeVars.FreeVars.t
val phantomvars_with_cache : ('t -> 'c -> QmlTypeVars.FreeVars.t) -> ('t, 'c) tsc -> QmlTypeVars.FreeVars.t

val export_vars : ('t, 'c) tsc -> QmlTypeVars.FreeVars.t

val export_ordered_quantif : ('t, 'c) tsc -> ordered_quantif

(**  number of type vars in a type definition *)
val arity : ('t, 'c) tsc -> int

val full_arity : ('t, 'c) tsc -> int * int * int

(** tells if the full arity if (0,0,0) *)
val is_empty : ('t, 'c) tsc -> bool

val map_body_unsafe : ('t -> 'u) -> ('t, 'c) tsc -> ('u, 'c) tsc

    (* TODO: add refresh, etc. here, when they are made generic *)
