(*
    Copyright © 2011 MLstate

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
(** Rights for files. *)
val default_rights : int

val path_sep : string
  (**
     The path separator, ["/"] on Unix, ["\\"] on Windows
  *)

val exists : string -> bool
  (**
     Checks if the file exists
  *)

val copy : ?force:bool -> string -> string -> int
  (**
     [copy ?force source target] copy the content of file named [source]
     into the file [target]
     It will create any directory that is needed
     If [force] is set, then [copy] occurs when though [target] exists
     @return The exit code of the underlying command
  *)

val copy_rec : ?force:bool -> string -> string -> int
  (**
     [copy ?force source target] copy the content of file or directory
     named [source] into destination [target]
     It will create any directories that are needed
     If [force] is set, then [copy] occurs even when [target] exists
     @return The exit code of the underlying command
  *)

val mv : ?force:bool -> string -> string -> int
  (**
     [mv ?force source target] mv the content of file named [source]
     into the file [target]
     It will create any directory that is needed
     If [force] is set, then [mv] occurs when though [target] exists
     @return The exit code of the underlying command
  *)

val extension : string -> string
  (**
     [extension filename] computes the extension of the file, ie the substring of
     [filename] between the last dot and the end of the file
     Returns the empty string is [filename] contains no dot.
  *)

val chop_extension : string -> string
  (**
     [chop_extension] removes the extension of the filename (as defined above)
     or does nothing when there is no extension
  *)

val module_name : string -> string
  (**
     [module_name filename] returns the name of the caml module that is defined by that name
     ie ["Pervasives"] for ["pervasives.ml"]
  *)

val is_relative_include_path : string -> bool
  (**
     [is_relative_include_path filename] tells you if the filename (in a format accepted by
     the -I option of ocamlc is relative to the current directory
     ie: [is_relative_include_path "dir"] is [true]
     [is_relative_include_path "/path/to/dir"] is [false]
     [is_relative_include_path "+dir"] is [false]
     (it is relative to the stdlib, not to the current directory)
  *)

val subst : (string * string) list -> string -> string
  (**
     [subst ext_assoc filename] replacing the extension of [filename]
     with its image through [ext_assoc]
     If the image doesn't exist, then the original string is returned
     If the image is [""], then the extension is stripped off ([subst \[("ml","")\] "a.ml"] is ["a"], not ["a."])
  *)

val from_pattern : string -> string -> string
  (**
     [from_pattern pattern path]
     @return [pattern] where the following sequences are replaced:
     - ["%b"] by the basename of [path]
     - ["%_b"] by the basename of [path] where dots are replaced by underscores
     - ["%d"] by the dirname of [path]
     - ["%e"] by the extension of [path]
     - ["%"] by [path] without its extension
     - ["%_"] by [path] without its extension and where dots are replaced by underscores
     Note that you cannot escape ['%'].
  *)

val content : string -> string
  (**
     @param f the filename
     @return The content of the file as one string
     @raise Unix.Unix_error ? if the file doesn't exists
     @raise Failure if the file is a directory
  *)

val content_opt : string -> string option
  (**
     @param f the filename to read
     @return [None] if an error occurred, [Some] of the content instead
  *)

val mimetype : ?mime_db:string -> string -> string
  (**
     @return the mimetype of the given filename
     @param mime_db is a filename as accepted by the -m option of the command file
  *)

val is_relative: string -> bool
  (**
     Determine if a path is relative
  *)

val normalize_relative_path: string -> string option
  (**
     Normalize a relative path.

     - "/foo" -> None
     - "./foo" -> Some "foo"
     - "foo"   -> Some "foo"
     - "a/b/c/../../d" -> Some "a/d"
  *)

val explicit_path : string -> string option -> string
  (**
     According given path and prefix, clean and explicit it :
     Exemple :
     explicit_path "/profA/profB/profC/../profC1/../../profB1/.///./profC2/./profD/profE/.././" None;;
     - string = "/profA/profB1/profC2/profD"
     @param path : the path can be absolute or relative.
     @param prefix : optionnal prefix

     If the path is absolute, it is jsut cleaned
     If it is relative, and there is no prefix given, current absolute path is retrieved [Sys.getcwd] and concatenated to path, and then produced path is cleaned
     If it is relative, and a preifx is given, they are jsute concatenated and cleaned
     if path and prefix are relatives, they are concatenated, then current absolute path is concateneted to produced path, and finaly, all is cleaned
  *)

val clean_beginning_path : string -> string
  (**
     According given path, clean it's beginning and verify that rest of the path has not relatif parts
     Exemple :
     clean_beginning_path "../.././toto/titi";;
     - : string = "/toto/titi"
     clean_beginning_path  "../.././toto/./titi";;
     Exception: Failure "\"../.././toto/./titi\" : End of path is not clean".
  *)

val last_modification : string -> Time.t
  (**
     [last_modification filename]
     @return the time as described by [Unix.time]
     @raise Unix_error _ if [filename] doesn't exists
  *)

val virtual_content : int -> string -> string

val lines_fold : ('a -> string -> 'a) -> 'a -> string -> 'a
  (**
     [lines_fold f acc filename] opens the file [filename] and gives
     every line to [f], along with [acc].
  *)

val lines_foldi : ('a -> string -> int -> 'a) -> 'a -> string -> 'a
  (**
     same as [lines_fold], but [f] is also given the line number
     (starting from 1)
  *)

val lines_foldi_offset : ('a -> string -> int -> int -> 'a) -> 'a -> string -> 'a
  (**
     same as [lines_foldi], but [f] is also given the global offset
     of the beginning of the line (starting from 0)
  *)

val lines_mapi : (string -> int -> 'a) -> string -> 'a list
  (**
     [lines_mapi f filename]
     maps through the lines of [filename]
  *)

val lines_rev_mapi : (string -> int -> 'a) -> string -> 'a list
  (**
     [lines_mapi f filename]
     maps through the lines of [filename] from top to bottom and then
     reverse the output list.
     [f] is NOT given the last lines of the file first
  *)

val lines_map_and_fold_i :
  ('a -> string -> int -> 'a * 'b) -> 'a -> string -> 'a * 'b list
  (**
     The usual fold_map with a counter on the lines of the given file
  *)

(**
   cover more compatibilty than Filename.concat
*)
val concat : string -> string -> string

(**
   [output filename content]
   returns [true] if everything ok,
   returns [false] in case of a [Sys_error]

   @deprecated This function is unefficient because
   it allocate a big string just to print it in a file.
   Use [pp_output] or [oc_output].
*)
val output : string -> string -> bool

(**
   Like [output] but with a format interface.
   <!> Unlike [output], the error message is returned

   [pp_output filename printer data]
   returns [None] if everything ok,
   returns [Some error] in case of a [Sys_error]
*)
val pp_output : string -> 'a BaseFormat.pprinter -> 'a -> string option

(**
   Like [pp_output] but with a out_channel interface.

   [oc_output filename printer data]
   returns [None] if everything ok,
   returns [Some error] in case of a [Sys_error]
*)
val oc_output : string -> (out_channel -> 'a -> unit) -> 'a -> string option

val mkdtemp : string -> string
val check_create_path : ?rights:Unix.file_perm -> ?nobackslash:bool -> ?isdirectory:bool -> string -> bool
val mlstate_dir : string Lazy.t
val mlstatelibs : string Lazy.t
val is_regular : string -> bool
val is_directory : string -> bool

(** {6 Directory Iterators} *)

(**
   The function are executed in the current working directory.
   [name] is the basename of the file
   [path] is the relative path wrt to the given path, complete,
   containing also the given path as prefix, and the filename.

   Example:
   let's say you have this tree in your current wd:
   $ tree
   toto/
   foo.t
   bar.g
   foobar.gogo

   you'll iter on:
   [
   name:"foo.t" path:"toto/foo.t" ;
   name:"bar.g" path:"toto/bar.g" ;
   name:foobar.gogo path:"foobar.gogo" ;
   ]
   Order of iter is unspecified
*)

(**
   Iter also on directories if [showdir] is set to [true].
*)
val iter_dir_rec :
  ?showdir:bool ->
  (name:string -> path:string -> unit) -> string -> unit

(**
   <!> Beware, the implementation contains a FIXME
*)
val fold_dir_rec :
  ('a -> name:string -> path:string -> 'a) -> 'a -> string -> 'a

(**
   Ignoring directories, iter only on regular files.
*)
val iter_dir :
  (name:string -> path:string -> unit) -> string -> unit

(**
   Ignoring directories, fold only on regular files.
   <!> Beware, the implementation contains a FIXME
*)
val fold_dir :
  ('a -> name:string -> path:string -> 'a) -> 'a -> string -> 'a

(**
   [remove_rec file] behaves like [rm -rf file]
   @raise Invalid_argument if [remove_rec] encounters a character or a
   block device
*)
val remove_rec : string -> unit

(** {6 Sadly undocumented API} *)

val completion : string -> string list
val backup_path : string -> string option
val append_or_create : string -> out_channel
val channel_contents : in_channel -> string
exception Process_error of string
val process_output : string -> string
val simplify_path : string -> string
  (* return completed paths for filename fname for each occurrence in dirs *)

val get_locations_regexp : ?dir:bool -> string list (*dirs*) -> string (*filename:caml regexp*) -> string list (* fnames *)
val get_locations : ?dir:bool -> string list (*dirs*) -> string (*fname*) -> string list (* fnames *)
  (* if dir, looks for a directory, else for a regular file. Defaults to [false] *)
  (* same as get_locations but return only one path,
     user can specilaised behavior in case of no or many possibl paths *)
val get_one_location :
  ?dir:bool -> (* same meaning as for [get_locations] *)
  ?missing_file : (string list (*dirs*) -> string (*fname*) -> string) ->
  ?many_files   : (string list (*dirs*) -> string (*fname*) -> string list (* paths *) -> string) ->
  string list (*dirs*) -> string (*fname*) -> string
