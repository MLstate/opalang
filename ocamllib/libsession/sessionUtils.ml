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
let (@>) f k = f k

let debug level fmt =
  Logger.debug ("[LIBSESSION][%s] "^^fmt^^"%!") level

let info level fmt =
  Logger.info ("[LIBSESSION][%s] "^^fmt^^"%!") level

let error level fmt =
  Logger.error ("[LIBSESSION][%s] "^^fmt^^"%!") level

let random_int () = Random.int 1073741823 (* 2^30 -1 *)

let generate_without_conflicts exists =
  let rec aux () =
    let id = random_int () in
    if exists id then
      aux ()
    else id
  in aux ()
