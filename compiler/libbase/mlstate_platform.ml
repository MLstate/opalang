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
