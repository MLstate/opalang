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

(**
   Specific bsl processing for Opa files.

   @author Mathieu Barbin
*)

(**
   This module implements part of the bslregister process.
*)
(** *)
type filename = string
type contents = string


(**
   Bsl Preprocessing for Opa files.

   Essentially, it is about producing generated declaration.
   {[
   ##include functions bslpervasives
   ]}
   will produce a code like :
   {[
   `+` = %%bslpervasives.add_int%% : int, int -> int
   ...
   etc...
   ]}

   The final bymap should have been built at the end of all registering
   of bypass, or some inclusion may be unsuccessfull.
*)
val preprocess :
  final_bymap:BslLib.BSL.ByPassMap.t -> BslDirectives.opalang_decorated_file -> (filename * FBuffer.t)


type true_means_error = bool

(**
   This performs checks on the bsl opa.
   All errors log are produced using OManager channel.

   It makes no OManager errors, or does not raise any exception,
   just logs errors using citation if possible,
   and returns at the end a bool indicating
   if some errors was encounted during the checking.
   [true] means errors.

   {[
   if checking_fail ~final_bymap opa_codes then
     !! the opa in the bsl is buggy !!
   else
     (* everything is ok *)
   ]}

   The function returns also the typed opa interfaces,
   produces after typing.

   The filenames should be preserved.
*)
val checking_fail :
  final_bymap:BslLib.BSL.ByPassMap.t -> (filename * contents) list ->
  true_means_error * (filename * FBuffer.t) list
