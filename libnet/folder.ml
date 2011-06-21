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
(** folder.ml *)

(**
   Notes:
   1) Now extensively rewritten to use Base, Filename and Unix.
   2) Removed old FileSys functor.
*)
module List = Base.List
module String = Base.String

type folder =
    { current: string ref;
      valid: string list ref
    }

exception Folder_error of string

(* Equivalent of try with _ -> for this file
 * We ban all exceptions named here and log any we don't know about.
 *)
let check_exn ?raise_exn funname defval exn =
  let action () = match raise_exn with | Some exn -> raise exn | None -> defval in
  match exn with
  | Folder_error _ -> action ()
  | Failure _ -> action ()
  | Unix.Unix_error _ -> action ()
  | exn -> (Logger.warning "Folder.%s: Unknown exception %s" funname (Printexc.to_string exn); action ())

(* Some support functions *)

(* Split a path into its directory and filename.
 * We need to actually stat the file to determine if it is really
 * a file or a directory.  This is needed so that we can determine
 * if a file spec is in a valid directory or not.  We need to take
 * into account whether the file or dir exists or not.  This routine
 * is now used to stat files which we want to create.
 *)
let dir_file path =
  let (dir,file) =
    try
      (match (Unix.stat path).Unix.st_kind with
	 Unix.S_DIR -> (path,"")
       | Unix.S_REG -> (Filename.dirname path,Filename.basename path)
       | _ -> raise (Failure "File not regular file or directory"))
    with Unix.Unix_error (Unix.ENOENT, "stat", _) ->
      (* It doesn't exist so we don't know if it's a dir or file *)
      (Filename.dirname path,Filename.basename path)
  in
  (PathTransform.string_to_mysys (File.explicit_path "" (Some (PathTransform.string_to_unix dir))),file)

(* Just get the current folder and extract up to last '/'.
   May raise Not_found *)
let chup file =
  let len = String.length file in
  let pos = Str.search_backward (Str.regexp_string File.path_sep) file (len-1) in
  String.sub file 0 (pos+1)

(* canonical cwd path: 
 * Return the canonical name of a file relative to "from".
 * Now just a call to explicit_path in the File module.
 *)
let canonical from file = File.explicit_path file (Some from)

(* Rename Unix functions *)
let openfileread file = Unix.openfile file [Unix.O_RDONLY] 0o640
let openfilewrite file = Unix.openfile file [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640
let openfileappend file = Unix.openfile file [Unix.O_WRONLY;Unix.O_APPEND;Unix.O_CREAT] 0o640
let filesize fd = (Unix.fstat fd).Unix.st_size
let lseek fd int = Unix.lseek fd int Unix.SEEK_SET

(* Creator methods *)

let empty dir = { current = ref dir; valid = ref [ dir ] }

(* Accessor methods *)

let current_folder f = !(f.current)
let get_valid_folders f = !(f.valid)

(* We store the canonical name to make later searches quicker. *)
let add_valid_folder f dir =
  let cdir = canonical (!(f.current)) dir in
  if not (List.mem cdir (!(f.valid)))
  then f.valid := cdir :: (!(f.valid))
  else raise (Folder_error "not_valid")

(* validate_folder f dir:
 *   Check if a folder is under one of the valid folders.
 *   Return the canonical name if it is, raise Folder_error "not_valid" if
 *   it's not.
 *)
let validate_folder f dir =
  let cdir = canonical (!(f.current)) dir in
  if List.fold_left (fun ok d -> ok || String.is_prefix d cdir) false (!(f.valid))
  then (*Logger.debug "validate_folder: %s is valid\n" dir;*) cdir
  else (*Logger.debug "validate_folder: %s is NOT valid\n" dir;*) raise (Folder_error "not_valid")

(* Access permissons for folder *)
let exists_folder f dir perms =
  try Unix.access (canonical (!(f.current)) dir) perms; true
  with exn -> check_exn "exists_folder" false exn

let writable_folder f dir =
  let res = exists_folder f dir [Unix.W_OK] in
  Logger.debug "writable_folder: %s %b" dir res;
  res

(* Same as above but turn into bool *)
let valid_folder f dir =
  try ignore (validate_folder f dir); true
  with exn -> check_exn "valid_folder" false exn

(* We chdir to the validated folder so it must exist.
 * We also convert any error exceptions into access_denied.
 *)
let chfolder f dir =
  let cwd = Unix.getcwd () in
  try
    let cdir = validate_folder f dir in
    Unix.chdir cdir;
    f.current := cdir;
    Unix.chdir cwd
  with exn ->
    (*Logger.debug "chfolder: exn=%s\n" (Printexc.to_string exn);*)
    Unix.chdir cwd;
    check_exn ~raise_exn:(Folder_error "access_denied") "chfolder" () exn

(* Move up one folder. Need to check we're still valid. *)
let chfolderup f =
  try chfolder f (chup (current_folder f))
  with Not_found -> raise (Folder_error "not_valid")

(* May leave the current folder invalid !!! *)
let remove_valid_folder f dir =
  f.valid := List.filter (fun d -> d <> dir) (!(f.valid));
  let cfolder = current_folder f in
  try ignore (validate_folder f cfolder)
  with (Folder_error "not_valid") ->
    if List.length (!(f.valid)) = 0
    then ()
    else chfolder f (List.nth (!(f.valid)) 0)

(* Rename a folder, assume already valid *)
let rename_folder f from_name to_name =
  try
    let cfn = canonical (!(f.current)) from_name in
    let ctn = canonical (!(f.current)) to_name in
    Unix.rename cfn ctn;
    true
  with exn ->
    Logger.debug "rename_folder: exn=%s" (Printexc.to_string exn);
    false

(* Delete a folder, assume already valid *)
let file_action f name action action_name =
  try action (canonical (!(f.current)) name); true
  with exn -> Logger.debug "%s: exn=%s" action_name (Printexc.to_string exn); false

(* Delete a folder, assume already valid *)
let delete_folder f name = file_action f name Unix.unlink "delete_folder"

(* Create a directory, assume already valid *)
let create_directory f name = file_action f name (fun n -> Unix.mkdir n 0o751) "create_directory"

(* Delete a directory, assume already valid *)
let delete_directory f name = file_action f name Unix.rmdir "delete_directory"

(* Return a string array with a directory listing for the given dir (relative to f.current) *)
let list f dir =
  try
    let cwd = current_folder f in
    let _ = chfolder f dir in
    let dir = (!(f.current)) in
    let _ = f.current := cwd in
    Some (Sys.readdir dir)
  with exn -> check_exn "list" None exn

(* Support code for the ls-style directory listing *)

(* File permissions *)
let str_of_rwx rwx =
  (if (rwx land 0x04) <> 0 then "r" else "-")^
  (if (rwx land 0x02) <> 0 then "w" else "-")^
  (if (rwx land 0x01) <> 0 then "x" else "-")

let str_of_perm k p =
  (match k with
     | Unix.S_REG -> "-" 	(*	Regular file	*)
     | Unix.S_DIR -> "d" 	(*	Directory	*)
     | Unix.S_CHR -> "c" 	(*	Character device	*)
     | Unix.S_BLK -> "b" 	(*	Block device	*)
     | Unix.S_LNK -> "l" 	(*	Symbolic link	*)
     | Unix.S_FIFO -> "p" 	(*	Named pipe	*)
     | Unix.S_SOCK -> "s")^ 	(*	Socket	*)
   (str_of_rwx ((p asr 6) land 0x07))^
   (str_of_rwx ((p asr 3) land 0x07))^
   (str_of_rwx (p land 0x07))

(* Human-readable file sizes.
 * Todo: use LargeFile for file sizes!!!
 * Currently not used because ftp clients didn't understand it.
 *)
(*let human i =
  let f = float_of_int i in
  if f < 1024.0 then Printf.sprintf "%d" i
  else if f < (1024.0 *. 1024.0) then Printf.sprintf "%.1fk" (f /. 1024.0)
  else if f < (1024.0 *. 1024.0 *. 1024.0) then Printf.sprintf "%.1fM"  (f /. (1024.0*.1024.0))
  else Printf.sprintf "%.1fG"  (f /. (1024.0*.1024.0*.1024.0))*)

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

(* Convert Unix times into date string *)
let mtime t =
  let gmt = Unix.gmtime t in
    (* This is ls -l format *)
    (*Printf.sprintf "%d-%02d-%02d %02d:%02d"
            (1900+gmt.Unix.tm_year) (gmt.Unix.tm_mon+1) gmt.Unix.tm_mday gmt.Unix.tm_hour gmt.Unix.tm_min*)
    (* This is what ftp clients seem to need *)
    Printf.sprintf "%s %02d %04d" (months.(gmt.Unix.tm_mon)) gmt.Unix.tm_mday (1900+gmt.Unix.tm_year)

(* Group and user names *)
let uid_name uid = try (Unix.getpwuid uid).Unix.pw_name with Not_found -> Printf.sprintf "%d" uid
let gid_name gid = try (Unix.getgrgid gid).Unix.gr_name with Not_found -> Printf.sprintf "%d" gid

(* For tabulation, compute maximum size per column *)
let get_max_slens a b =
  let alen,blen = Array.length a,Array.length b in
  let geta i = if i < alen then Array.unsafe_get a i else 0 in
  let getb i = if i < blen then Array.unsafe_get b i else "" in
    Array.init (max alen blen) (fun i -> max (geta i) (String.length (getb i)))

(* Given an array of arrays of strings, pad each column to have the same length.
 * The pat parameter is an array of "l" or "r" strings indicating whether
 * left or right padding for each column.
 * Any other character results in no padding.
 * The pat characters default to "l" if the pat array is too small.
 * Note that we must not have spaces after the filename because the client
 * will think it's part of the filename.
 *)
let tabulate pat aa =
  let patlen = Array.length pat in
  let len = Array.fold_left (fun mx a -> max mx (Array.length a)) 0 aa in
  let maxs = Array.fold_left get_max_slens (Array.make len 0) aa in
    Array.map (fun a ->
		 let alen = Array.length a in
		   Array.init len (fun i ->
				     let getpat i = if i < patlen then pat.(i) else "l" in
                                     let complete_none _ _ s = s in
				     let complete =
                                       match getpat i with
                                       | "l" -> String.complete_left
                                       | "r" -> String.complete_right
                                       | _ -> complete_none in
				       complete maxs.(i) ' ' (if i < alen then a.(i) else ""))) aa

(* Return a file stat, checking validity, None if not valid *)
let stat_file folder _file =
  let cwd = Unix.getcwd () in
  try
    Unix.chdir (!(folder.current));
    let (dir,file) = dir_file _file in
    let cdir = validate_folder folder dir in
    Unix.chdir cdir;
    let s = Unix.stat (if file = "" then cdir else file) in
    Unix.chdir cwd;
    Some s
  with exn ->
    Unix.chdir cwd;
    check_exn "stat_file" None exn

(* Return a formatted ls-style file listing *)
let ls_files folder filenames =
  let ssc ss s = match (ss,s) with (ss,"") -> ss | ("",s) -> s | (ss,s) -> ss^" "^s in
  Array.map (fun a -> Array.fold_right ssc a "")
    (tabulate [|"l";"l";"r";"r";"l";"r";"n"|]
       (Array.map (fun filename ->
		     match stat_file folder filename with
			 Some stat ->
			   [| Printf.sprintf "%s" (str_of_perm stat.Unix.st_kind stat.Unix.st_perm);
			      Printf.sprintf "%d" stat.Unix.st_nlink;
			      Printf.sprintf "%s" (uid_name stat.Unix.st_uid);
			      Printf.sprintf "%s" (gid_name stat.Unix.st_gid);
			      Printf.sprintf "%d" ((*human*) stat.Unix.st_size);
			      Printf.sprintf "%s" (mtime stat.Unix.st_mtime);
			      Printf.sprintf "%s" filename |]
		       | None -> [|"";"";"";"";"";"";""|]) filenames))

(* Open a file for reading, check it's valid *)
let openfile open_file f _file =
  let cwd = Unix.getcwd () in
  try
    Unix.chdir (!(f.current));
    let (dir,file) = dir_file _file in
    let cdir = validate_folder f dir in
    Unix.chdir cdir;
    let fd = open_file file in
    Unix.chdir cwd;
    fd
  with Unix.Unix_error _ ->
    Unix.chdir cwd;
    raise (Folder_error "file_inaccessible")

(** Open file for read.
    We need to stat it because Unix.open_file allows you to open a directory.
*)
let openfileread folder file =
  match stat_file folder file with
    Some stat ->
      (match stat.Unix.st_kind with
         Unix.S_REG -> Some (openfile openfileread folder file)
       | _ -> None)
  | None -> None
let openfilewrite folder file = openfile openfilewrite folder file
let openfileappend folder file = openfile openfileappend folder file

(* Read in [amnt] bytes to a string.
 * If amnt=0 then read in whole file.
 *)
let read fd amnt =
  let size = match amnt with 0 -> filesize fd | size -> size in
  let buff = String.create size in
  let cnt = Unix.read fd buff 0 size in
  (*Logger.debug "read: size=%d cnt=%d buff=%s" size cnt (String.sub buff 0 cnt);*)
  if cnt < size
  then String.sub buff 0 cnt
  else buff

(* Write size bytes from buff, starting from index 0 to fd *)
let write fd buff size =
  (*Logger.debug "Folder.write: writing %d bytes '%s'" size buff;*)
  Unix.write fd buff 0 size

(* Write-through to file close *)
let closefile = Unix.close

(* End of file folder.ml *)
