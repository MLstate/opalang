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

(* ************************************************************************** *)
(** {b Descr}: This module provides manipulation of annotation maps used
    internally by the typechecker. Such annotation maps contains typing
    information as manipulated by the typechecker and a different from
    annotation map of the QML side. Bridges between the 2 kinds of annotation
    map are provided here to allows communication between the 2 worlds: the
    world of low and the world of high levels typecheckers.                   *)
(* ************************************************************************** *)



type annotmap = W_Algebra.simple_type QmlAnnotMap.gen_annotmap

val empty_annotmap : annotmap
(* val qml_annotmap_to_annotmap: QmlAst.annotmap -> annotmap *)
val annotmap_to_qml_annotmap: annotmap -> QmlAst.annotmap
