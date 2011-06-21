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
module Io = FileIo.Make(IoUnix)

let do_backup location db_path =
  Logger.info "Will copy from %s to %s" db_path location;
  let fm = Io.make Io.ReadOnly db_path in
  let copy f =
    try Io.copy_file fm f ~location ""
     with Failure "No cp" ->
       Logger.error "Can not do a backup of the file : %s\n%!" (Io.get_name fm f)
  in
  let lst = [ Io.Node; Io.Uid; Io.Uid_rev; Io.Timestamp; Io.Flags; Io.Trans; Io.Db_state; Io.Config ] in
  List.iter copy lst;
  Io.close fm;
  Logger.info "Finished to copy %s to %s\n%!" db_path location;

  let fm = IoManager.create `append location in
  IoManager.close fm
