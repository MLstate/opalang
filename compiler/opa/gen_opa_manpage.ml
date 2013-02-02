(*
    Copyright © 2011, 2012 MLstate

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

(* FIXME: should be moved in tools *)

(** Print the manpage for the Opa compiler. S3 version. *)

(* Load warnings of opa s3 applications *)
(* required by some toplevel sideeffects in the modules of OpaEnv *)
let _ = WarningClass.load_set S3Warnings.warning_set

let _ = OpaEnv.Options.write_manpage stdout

let () = OManager.exit 0
