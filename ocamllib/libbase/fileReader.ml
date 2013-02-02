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
(* depends *)
module String = BaseString

(* -- *)

type ('a, 'b) input = string
type ('a, 'b) t = in_channel
let make = open_in
let length = in_channel_length
let get ic pos =
  let _ = seek_in ic pos in
  input_char ic
let sub ic pos len =
  let _ = seek_in ic pos in
  String.init len (fun _ -> input_char ic)
let close = close_in
