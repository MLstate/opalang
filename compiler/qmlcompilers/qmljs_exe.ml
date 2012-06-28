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
    js compiler -- command line tool

    @author Maxime Audouin
    @author Mathieu Barbin
*)

(** loading the warnings of the different compilers
    HACK: it also forces caml to link all the backends even
    if they are not called directly (but only through registers)
*)
let () = List.iter WarningClass.load_set [
  Imp_Compiler.warning_set;
]

(** run *)
exception SigInt
let _ =
  try
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise SigInt)) ;
    let () = OpabslgenPlugin.Self.self_store () in
    let () = ObjectFiles.turn_separated_off () in
    let return = QmlCompilers.Qml2jsSugar.console () in
    exit return
  with
  | SigInt -> OManager.error "building process not accomplished due to an user interruption@\n"
