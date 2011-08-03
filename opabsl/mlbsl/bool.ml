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

(* Bool *)
(* import Pervasives *)

(* register-spe-op _and\(&&) : bool -> bool -> bool *)
(* register _and\Pervasives.(&&) : bool -> bool -> bool *)



##register _and : bool, bool -> bool
let _and = ( && )
##register _or : bool, bool -> bool
let _or = ( || )
##register not : bool -> bool
let not = ( not )

##register eq : 'a, 'a -> bool
let eq x y = 0 = ServerLib.compare x y
##register lt : 'a, 'a -> bool
let lt x y = 0 > ServerLib.compare x y
##register gt : 'a, 'a -> bool
let gt x y = 0 < ServerLib.compare x y
##register le : 'a, 'a -> bool
let le x y = 0 >= ServerLib.compare x y
##register ge : 'a, 'a -> bool
let ge x y = 0 <= ServerLib.compare x y


(* register &&\Pervasives.(&&) : bool -> bool -> bool *)
(* (\* let and_bool = ( && ) *\) *)

(* register or_bool : bool -> bool -> bool *)
(* register ||\or_bool : bool -> bool -> bool *)
(* let or_bool = ( || ) *)

(* register not_bool : bool -> bool *)
(* register not_bool : bool -> bool *)
(* let not_bool = ( not ) *)
