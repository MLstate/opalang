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
   Deprecated module for localisation in the source code.
   @deprecated Positions management have been unified in [FilePos]
*)

(**
   The type of informations shared between all nodes of all ASTs.

   TODO: there is here an indirection in the record,
   QmlAst and OpaAst disagree about how to represent position,
   and annotation index in the AST.
   Some benches done by Valentin have shown that inderection for
   positions and annotation index in the AST have a terrible impact
   on performance.

   TODO: In QmlAst, maybe flatten the positions and notes in each
   constructor, like in OpaAst.
*)
type annot = {
  pos : FilePos.pos ; (**The position in the code corresponding to this AST*)
  notes: int ; (**A unique identifier, which may be used to uniquely identifier an AST node. *)
}

(** get the pos field of an annot *)
val pos : annot -> FilePos.pos
val pp_pos_short : Format.formatter -> (FilePos.pos * bool) -> unit
val pos_to_short_string : (FilePos.pos * bool) -> string
(**
   A convenience structure to uniformly label the nodes of an AST
   with [annot] data.

   Convenient maybe, but inneficient. (cf TODO)
*)
type 'a label = 'a * annot
