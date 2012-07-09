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
    author David Rajchenbach-Teller
*)

(**
   A simple monad for handling operations that may fail.
*)

(**
   The status of an operation.
*)
type ('a, 'b) t = 
  | Ok  of 'a (**The operation succeeded*)
  | Err of 'b (**The operation failed*)

val return : 'a -> ('a, 'b) t
val fail:    'b -> ('a, 'b) t
val bind:    ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
