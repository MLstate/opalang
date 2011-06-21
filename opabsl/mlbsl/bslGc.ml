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
    @author Rudy Sicard
   
**)

open Gc

##register minor_collections : -> int
let minor_collections () = (Gc.stat()).minor_collections

##register major_collections : -> int
let major_collections () = (Gc.stat()).major_collections

##register compactions : -> int
let compactions () = (Gc.stat()).compactions

