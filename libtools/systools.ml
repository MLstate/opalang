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
module S = ServerArg

(* module DropPrivileges : Runtime.COMPONENT = *)
(* struct *)
(*   type options = { *)
(*     stay_root:bool; *)
(*     user:string; *)
(*     group:string; *)
(*   } *)
(*   type t = unit *)

(*   let name = "Drop privileges" *)
(*   let version = "1.0" *)
(*   let default_options = { *)
(*     stay_root = true; *)
(*     user = ""; *)
(*     group = "" *)
(*   } (\* TODO *\) *)

(*   let spec_args = *)
(*     [ *)
(*       ["--stay_root"], *)
(*       S.func S.bool (fun opt b -> {opt with stay_root = b}), *)
(*       "", "TODO" *)
(*     (\* TODO *\) *)
(*     ] *)

(*   let ports = [] *)
(*   let make opt pi = *)
(*     let _ = opt, pi in *)
(*     () *)

(*   let run _ sch =  *)
(*     let _ = sch in *)
(*     (\* TODO base en change_user fun below *\) *)
(*     () *)

(*   let close _ _ = () *)
(* end *)

let change_user () =
  if (Unix.geteuid ()) <> 0 then ()
  else
    begin
      let get_arg ref_str =
        let res =
          Array.fold_left (
            fun accu opt ->
              if fst accu then (false, (Some opt))
              else if ref_str = opt then (true, (snd accu))
              else accu
          ) (false, None) Sys.argv
        in snd res
      in
      let get_id pattern getter =
        match get_arg pattern with
        | Some value ->
            begin try int_of_string value with
            | Failure _ -> (try getter value with | Not_found | Unix.Unix_error _ -> -1)
            end
        | _ -> -1
      in
      let stay_root = Array.fold_left (fun acc s -> acc || s = "--stay-root") false Sys.argv in
      if stay_root then Logger.warning "Warning: Be careful with the --stay-root flag !\n%!"
      else (
        let user =
          let id = get_id "--user" (fun user -> (Unix.getpwnam user).Unix.pw_uid) in
          if id >= 0 then id else 33 (* uid for www-data under linux systems? *)
        in
        let group =
          let id =
            let tmp_grp = get_id "--group" (fun group -> (Unix.getgrnam group).Unix.gr_gid) in
            if user <> -1 && tmp_grp = -1 then
              get_id "--user" (fun user -> (Unix.getpwnam user).Unix.pw_gid)
            else tmp_grp
          in
          if id >= 0 then id else 33 (* guid for www-data under linux systems? *)
        in
        (* let () = File.iter_dir_rec ~showdir:true (fun ~name:_ ~path -> Unix.chown path user group) (Lazy.force File.mlstate_dir) in *)
        let () = try Unix.setgid group; Logger.notice "[+] setting gid to %d%!" group with Unix.Unix_error _ -> () in
        let () = try Unix.setuid user; Logger.notice "[+] setting uid to %d%!" user with Unix.Unix_error _ -> () in
        ()
      )
    end
