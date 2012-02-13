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
(** folder.mli *)
(**
   Simple file access library which provides a minimal
   set of file and directory access functions but
   maintains a list of authorized directories.  Only
   directories which are prefixed by one of the authorized
   dirs are valid.
   @author Norman Scaife
*)

exception Folder_error of string

(** Type of the folders. *)
type folder

(** empty dir: Return a new folder with the current dir
    set to dir.  We also add the initial directory to
    the list of valid directories.
*)
val empty : string -> folder

(** current_folder folder: Return the current folder *)
val current_folder : folder -> string

(** valid_folders: return the current list of valid folders. *)
val get_valid_folders : folder -> string list

(** add_valid_folder folder dirname:
    Add dirname to the current list of valid folders.
    If the folder does not exist then raise Folder "not_found".
*)
val add_valid_folder : folder -> string -> unit

(** remove_valid_folder folder:
    Remove the given folder from the valid folder list.
    The name of the folder must match exactly, ie. "tmp/"
    will not match "tmp".
    If the current directory is a subdir of the one being
    removed then the current dir is set to the first folder
    in the valid folder list.
    Note that this might not result in removing access to
    the given directory if one of its parents is still in
    the list.
    WARNING: If the last valid folder is removed then the
    current dir is technically in an unsafe state since
    there is currently no way of flagging the non-existence
    of the current dir.
*)
val remove_valid_folder : folder -> string -> unit

(** valid_folder folder filename
    Test whether filename is accessible under folder.
*)
val valid_folder : folder -> string -> bool

(** writable_folder folder filename
    Test if file is writable.
*)
val writable_folder : folder -> string -> bool

(** chfolder folder dirname:
    Change current directory to dirname.
    The dirname must be a subdirectory of one of the valid_folders.
*)
val chfolder : folder -> string -> unit

(** chfolderup folder:
    Move up one folder checking that the parent folder is still valid.
*)
val chfolderup : folder -> unit

(** rename_folder:
    We just do the rename under folder conventions, assume already valid
*)
val rename_folder : folder -> string -> string -> bool

(** delete_folder: Again, assume already valid *)
val delete_folder : folder -> string -> bool

(** create_directory: Again, assume already valid *)
val create_directory : folder -> string -> bool

(** delete_directory: Again, assume already valid *)
val delete_directory : folder -> string -> bool

(** list folder filename:
    Return a directory listing of filename relative to folder
    as an array of strings.
    The . and .. directories are included.
    The list is unsorted, it's in OS order.
    We return None if the requested directory is inaccessible.
*)
val list : folder -> string -> string array option

(** ls_files folder filenames
    Return a formatted list of "ls"-style file listings for a list of filenames.
*)
val ls_files : folder -> string array -> string array

(** openfileread/write/append folder filename:
    Open a file for reading/writing/appending checking for validity, return file descriptor.
*)
val openfileread : folder -> string -> Unix.file_descr option
val openfilewrite : folder -> string -> Unix.file_descr
val openfileappend : folder -> string -> Unix.file_descr

(** lseek fd pos: seek to position pos from start of file *)
val lseek : Unix.file_descr -> int -> int

(** read fd amnt: read [amnt] bytes into a string *)
val read : Unix.file_descr -> int -> string

val write : Unix.file_descr -> string -> int -> int

(** closefile fd: close a file *)
val closefile : Unix.file_descr -> unit


(* End of file: folder.mli *)
