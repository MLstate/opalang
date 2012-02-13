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
