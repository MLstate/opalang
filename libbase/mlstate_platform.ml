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
(**
  This module offers utility functions to have run-time dispatch or conditional execution over platform specific implementation.
*)

(** The platform type, essentially the same as the ocaml platform type *)
type mlstate_platform = Unix | Cygwin | Windows

(** the current platform value *)
let mlstate_platform = match Sys.os_type with
                       | "Unix" -> Unix
                       | "Cygwin" -> Cygwin
                       | "Windows"
                       | "Win32" -> Windows
                       | _ -> failwith ("mlstate_platform : unknown platform "^(Sys.os_type))

(** on_PLATFORM f is a conditional execution of the given function if mlstate_platform matches PLATFORM *)

let apply_f f = ignore (f ())
let not_apply_f _ = ()

(** Function argument is executed on both Unix and Cygwin *)
let on_unixes  = if mlstate_platform = Unix || mlstate_platform = Cygwin then apply_f else not_apply_f
(** Function argument is executed on Unix only *)
let on_unix    = if mlstate_platform = Unix    then apply_f else not_apply_f
(** Function argument is executed on Cygwin only *)
let on_cygwin  = if mlstate_platform = Cygwin  then apply_f else not_apply_f
(** Function argument is executed on Windows only *)
let on_windows = if mlstate_platform = Windows then apply_f else not_apply_f

(** Select the good function,
    unix and windows implementation are mandatory.
    cygwin implemention can be ommited, in this case the unix implementation is used as fallback
    all function are taken as input, only the good one is returned
*)
let platform_dependent ~unix ?(cygwin=unix) ~windows () =
   match mlstate_platform with
   | Unix    -> unix
   | Cygwin  -> cygwin
   | Windows -> windows
