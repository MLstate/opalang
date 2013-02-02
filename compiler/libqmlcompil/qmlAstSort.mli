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
  Authors:
           2009, Mathieu Barbin           <Mathieu.Barbin@mlstate.com>
*)


(**)

type t
val empty : t
val add_elt : t -> QmlAst.code_elt -> t
val add : t -> QmlAst.code -> t
val get : t -> QmlAst.code

module Get :
sig
  val all : t -> QmlAst.code (** same as get *)
  val database : t -> QmlAst.code
  val new_type : t -> QmlAst.code
  val new_db_value : t -> QmlAst.code
  val new_val : t -> QmlAst.code
end

module RevGet : (** custom tail append need a RevGet *)
sig
  val database : t -> QmlAst.code
  val new_type : t -> QmlAst.code
  val new_db_value : t -> QmlAst.code
  val new_val : t -> QmlAst.code
end
