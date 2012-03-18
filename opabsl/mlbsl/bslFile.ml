(*
    Copyright © 2011, 2012 MLstate

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
module U = Unix

##register mlstate_dir : void -> string
let mlstate_dir () = Lazy.force File.mlstate_dir

##register exists : string -> bool
let exists n = try ignore (Unix.stat n) ; true with _ -> false
  (*   let exists = File.exists : this one use Sys.file_exists, what do you prefer ?*)

##register is_regular : string -> bool
let is_regular = File.is_regular

  (**
     Return true if given path is a file is a directory, false otherwise.
     If the file/directory doesn't exist, return false too.
  *)
##register is_directory : string -> bool
let is_directory x =
  try
    File.is_directory x
  with Unix.Unix_error (Unix.ENOENT, _, _) ->  false

##register make_dir : string -> bool
let make_dir n =
  try Unix.mkdir n 0o700; true with _ -> false

##register basename \ `Filename.basename` : string -> string

##register dirname \ `Filename.dirname` : string -> string

##register dir_sep : string
let dir_sep = Filename.dir_sep

##register copy: string, string, bool -> void
let copy a b force = ignore (File.copy ~force a b)

##register move: string, string, bool -> void
let move a b force = ignore (File.mv ~force a b)

##register remove_rec: string -> void
let remove_rec file = ignore (File.remove_rec file)


(**
   {1 Obsolete API}

   The following functions are blocking. They must be reimplemented in a non-blocking way
*)




  ##register fold_dir_rec : ('a, string, string -> 'a), 'a, string -> 'a
  let fold_dir_rec f = File.fold_dir_rec (fun acc ~name ~path -> f acc name path)

  ##register fold_dir_rec_opt : ('a, string, string -> 'a), 'a, string -> option('a)
  let fold_dir_rec_opt f acc path  =
    try
        Some (File.fold_dir_rec (fun acc ~name ~path -> f acc name path) acc path)
   with Unix.Unix_error (Unix.ENOENT, _, _) ->  None

  ##register path_sep : string
  let path_sep = File.path_sep


  ##register mimetype_opt : string -> option(string)
  let mimetype_opt x =
    try
        Some (File.mimetype x)
    with Failure _ -> None

  ##register explicit_path : string, option(string) -> string
  let explicit_path = File.explicit_path

  ##register clean_beginning_path : string -> string
  let clean_beginning_path = File.clean_beginning_path

  ##register last_modification : string -> time_t
  let last_modification f = Time.in_milliseconds (File.last_modification f)

  (**
     Dump a value to a file

     @param n The name of the file
     @param content The content to put in the file

     In case of error, explode.
  *)
  ##register of_string : string, string -> void
  let of_string n content =
    let och =
      let path = Filename.dirname n in
      ignore (File.check_create_path path);
	open_out n
    in output_string och content ; close_out och

##register create_full_path: string -> void
let create_full_path path = ignore (File.check_create_path path)

##register content_opt: string -> option(string)
let content_opt = File.content_opt

(**
   {1 Must reimplement}

   This works on Macintosh, but not Linux, due to limitations of epoll!
*)

##register [cps-bypass] content_cps: string, continuation(opa[option(string)]) -> void
let content_cps filename k =
  let fd = U.openfile filename [U.O_RDONLY; U.O_NONBLOCK] 0o600 in
  let size = (U.fstat fd).U.st_size in
  let sched = BslNet.default_scheduler in
  let addr = NetAddr.mk_file ~fd in
  let conn = Scheduler.make_connection sched addr in
  let finalize result =
    U.close fd;
    QmlCpsServerLib.return k result
  in
  let on_failure _ = finalize ServerLib.none
  and on_success (_, buffer) = finalize (ServerLib.some (ServerLib.wrap_string (FBuffer.contents buffer))) in
  Scheduler.read_all ~read_max:(Some size) sched conn ~err_cont:on_failure on_success

(**
   {1 Deprecated}
*)
(*Deprecated: use [content_cps]*)
##register content : string -> string
let content = File.content
