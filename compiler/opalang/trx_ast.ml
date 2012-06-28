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
(*
    @author Adam Koprowski
**)

(* TODO, This module should at some point be generalized in such
   a way that both OPA TRX and Ocaml TRX could use this AST *)

open QmlLoc

type prefix = [ `AND
              | `NOT
              | `NORMAL
              ]

type suffix = [ `QUESTION
              | `STAR
              | `PLUS
              | `NORMAL
              ]

type range = [ `ONE of int
             | `RANGE of int * int
             ]


type 'code expr = 'code expr_node label
 and 'code expr_node = 
   | Expr of 'code seq list

 and 'code seq = 'code seq_node label
 and 'code seq_node = 
   { seq_items : 'code item list
   ; seq_code : 'code option
   }

 and 'code item = 'code item_node label
 and 'code item_node = 
   { item_name : string option
   ; item_prefix : prefix
   ; item_primary : 'code primary
   ; item_suffix : suffix
   }

 and 'code primary = 'code primary_node label
 and 'code primary_node = 
   | Parens of 'code expr
   | Literal of string(*literal*) * bool(*is case-sensitive?*)
   | DynamicLiteral of 'code (* literal string containing embedded OPA expressions {...} *)
   | Code of 'code (* arbitrary expression implementing a parser *)
   | Rule of 'code (* reference to another parsing rule *)
   | Class of range list
   | Any
