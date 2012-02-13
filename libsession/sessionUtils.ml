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
