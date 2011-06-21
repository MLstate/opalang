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
##register chr : int -> option(char)
let chr i = try Some (Char.chr i) with Invalid_argument _ -> None

##register unsafe_chr \ `Char.unsafe_chr` : int -> char
##register code \ `Char.code` : char -> int
##register compare \ `Char.compare` : char, char -> int
##register escaped \ `Char.escaped` : char -> string
##register lowercase \ `Char.lowercase` : char -> char
##register uppercase \ `Char.uppercase` : char -> char

##register leq: char, char -> bool
let leq (a:char) (b:char) = a <= b

##register lt: char, char -> bool
let lt (a:char) (b:char) = a < b

##register eq: char, char -> bool
let eq (a:char) (b:char) = a = b

##register geq: char, char -> bool
let geq (a:char) (b:char) = a >= b

##register gt: char, char -> bool
let gt (a:char) (b:char) = a > b

##register neq: char, char -> bool
let neq (a:char) (b:char) = a <> b

##register ordering: char, char -> opa[Order.ordering]
let ordering (a:char) (b:char) =
  if a < b then BslPervasives.ord_result_lt
  else if a==b then BslPervasives.ord_result_eq
  else BslPervasives.ord_result_gt
