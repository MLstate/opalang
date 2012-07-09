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
open SurfaceAst

module Pattern :
  sig
    include TraverseInterface.TRAVERSE
      with type 'a t = 'b pat constraint 'a  = 'b * _ * _
       and type 'a container = 'b pat constraint 'a  = 'b * _ * _
    val get_vars : ?acc:'a list -> 'a pat -> 'a list
    val appears_str : string -> string pat -> bool
    val appears : Ident.t -> Ident.t pat -> bool
  end

module Type :
  sig
    include TraverseInterface.TRAVERSE
      with type 'a t = 'b ty constraint 'a  = 'b * _ * _
       and type 'a container = 'b ty constraint 'a  = 'b * _ * _
    val get_typenames : 'a ty -> 'a list
    val get_typenames_with_acc : 'a list -> 'a ty -> 'a list
    val get_typenames_from_arrow_type : 'a arrow_t -> 'a list
  end

module Expr :
  sig
    include TraverseInterface.TRAVERSE
      with type 'a t = ('b,'c) expr constraint 'a  = 'b * 'c * _
      and type 'a container = ('b,'c) expr constraint 'a  = 'b * 'c * _
    val appears_str : string -> (string,_) expr -> bool
    val appears : Ident.t -> (Ident.t,_) expr -> bool
    val used_vars_str : string list -> (string, _) expr -> string list
    val used_vars : Ident.t list -> (Ident.t, _) expr -> Ident.t list
  end

module CodeElt : TraverseInterface.TRAVERSE
  with type 'a t = ('b,'c) expr constraint 'a  = 'b * 'c * _
   and type 'a container = ('b,'c) code_elt constraint 'a  = 'b * 'c * _

module Code :
  sig
    include TraverseInterface.TRAVERSE
      with type 'a t = ('b,'c) expr constraint 'a  = 'b * 'c * _
      and type 'a container = ('b,'c) code constraint 'a  = 'b * 'c * _
    val size : (_,_) code -> int
    val length : (_,_) code -> int
  end

module CodeEltTopPattern : TraverseInterface.TRAVERSE
  with type 'a t = 'b pat constraint 'a  = 'b * _ * _
   and type 'a container = ('b,'c) code_elt constraint 'a  = 'b * 'c * _

module CodeTopPattern : TraverseInterface.TRAVERSE
  with type 'a t = 'b pat constraint 'a  = 'b * _ * _
   and type 'a container = ('b,'c) code constraint 'a  = 'b * 'c * _
