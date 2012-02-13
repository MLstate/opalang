(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
