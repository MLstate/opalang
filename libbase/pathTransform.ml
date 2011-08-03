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
(*
    @authors Rudy Sicard
*)

(**
 
 This module is about file system path transfomation between windows space and unix space.
 It should not depend on anything else then Str or other standard Ocaml library NOT INCLUDING THE UNIX MODULE
 
 *)


(** Basic module for inspecting Unix path (string) *)
module Unix = struct
  let is_absolute s =  (String.length s> 0) && s.[0] == '/'
end

let is_sep c = (c = '/') || (c = '\\')
let string_drop s n = String.sub s n (String.length s - n)

(** Basic module for inspecting windows path (string) *)
module Win = struct
  let get_disk s = s.[0]
  let is_drive c = match c with | 'a'..'z' | 'A'..'Z' ->true | _ -> false
  (** Absolute or relative that is the question *)
  let is_absolute s = 
    (String.length s > 1) && (s.[1]=':') && (is_drive (get_disk s))
  let decomp s = 
     let first = if is_sep s.[2] then 3 else 2 in
     String.make 1 (get_disk s),(string_drop s first)

end

(** windows disk, can only be a char but let's be ready for the following 20000 years *)
type disk = string

(** 
    strings stinks so we have a proper representation for path
    there are absolute and of two kinds of path :
      Unix path are used as the standard normalized path representation, they are just the path that you know
      Windows path are composed of a disk and a sub path

    All windows path have a unix representation, there are 'mounted' in /cygdrive/
    Some Unix path have no Windows representation  like /cygdrive/bad_drive_name/toto
    
    After normalisation  "." and ".." are eliminated, superfluous ".." are just ignored
*)
type path = Windows of disk * string list | Unix of string list

(** 
    where is linux in windows : (disk:char,path:"string") 
    it is the mount point of your linux partition in the windows tree
*)
let where_is_cygwin_disk = "C"
let where_is_cygwin_path = ["cygwin"] (* not truly generic, limited to length one path *)
let where_is_cygwin =  Windows (where_is_cygwin_disk,where_is_cygwin_path)  (* Remark ~ Unix ["/"] *)  

(**
  where is windows in unix ? 
  it is the mount point of your windows partition in the linux tree
*)
let where_is_windows_path = [ "cygdrive" ]


(** translation of a path to a particular flavour *)
let to_unix_path    p= match p with 
      |Windows(drive,path) -> 
            if ( try List.hd path = List.hd where_is_cygwin_path with Failure "hd" -> false ) then Unix (List.tl path)
             else Unix (where_is_windows_path @ (drive :: path)) 
      | Unix _ -> p

let to_windows_path p= match p with 
        | Windows _ -> p 
        | Unix path -> 
            if (try List.hd path = List.hd where_is_windows_path with Failure "hd" -> false) then Windows (List.nth path 1, (List.tl (List.tl path)))
            else Windows (where_is_cygwin_disk,where_is_cygwin_path@path)


(** *)
(*let remove_links p = 
  let is_link = Unix.state
match p with
| Windows _ -> p
| Unix path *)



(** path normalisation, go to unix path and resolve "." ".." elements *)
let normalize ?(_remove_link = fun p -> p) p = 
   let get_unix_path = function Unix p -> p | _ -> assert false in
   let p = to_unix_path p in
   let p = List.fold_left 
     (fun rev s-> 
	  if s = "."  then rev else
	  if s = ".." then try List.tl rev with Failure "tl" -> rev else
	  s::rev) [] (get_unix_path p)
   in Unix(List.rev p)

let windows_mode =  match Sys.os_type with
  | "Win32" | "Windows" -> true | _ -> false

(** since the path can be used in a subshell we need to escape backslash, the more the better *)
let escaping_level = if windows_mode then 1 else 2

(** translation of a path to a string *)
let to_string p =  
  match p with
  | Unix p -> "/"^(String.concat "/" p)
  | Windows(disk,p) -> 
    let win_sep = String.make escaping_level '\\' in
    disk ^ ":" ^ win_sep ^(String.concat win_sep p)

let regexp_allslash = Str.regexp "/\\|\\\\"

let split_any     s = Str.split regexp_allslash s

(** properties of a string path, which are lost after translation to type path *)
let is_absolute s = (Unix.is_absolute s) || (Win.is_absolute s)
let is_relative s = not(is_absolute s)

(** string imports : function to gain *)
let inspect_string relative_position s = 
    let s = if String.length s > 0 && s.[0] = '"' && s.[String.length s - 1] = '"' then  String.sub s 1 (String.length s - 2) else s in
    let relative_position = 
          let rec aux r = match r with Unix p -> p | Windows _ -> aux (to_unix_path r)
          in aux relative_position
    in 
    if is_relative s then false,Unix( relative_position @ (split_any s) )
    else if Unix.is_absolute s then true,Unix( split_any s )
    else if Win.is_absolute  s then 
       let (disk,path)= Win.decomp s in true,normalize( Windows(disk,split_any path) )
    else assert false

let relative_position = snd (inspect_string (Unix []) (Sys.getcwd ()))
let inspect_string ?(relative_position=relative_position) s = inspect_string relative_position s    

(** convert a string path to a normalized path type *)
let of_string ?(relative_position=relative_position) s = normalize (snd (inspect_string ~relative_position s))

(** check if a path is absolute, whatever the os *)
let string_is_absolute s = fst (inspect_string ~relative_position s)


(** convert a string path to a unix or windows string path or the current runtime os path *)
let string_to_unix    ?(relative_position=relative_position) s = (to_string (to_unix_path (of_string ~relative_position s)))
let string_to_windows ?(relative_position=relative_position) s = (to_string (to_windows_path (of_string ~relative_position s)))
let string_to_mysys   ?(relative_position=relative_position) s = if windows_mode then string_to_windows ~relative_position s else string_to_unix ~relative_position s

(* REBUS VERSIONNE


(** Recognise and use a cygdrive path i.e.of form  cydrive/c/path *)
let is_cygdrive s = 
 let len = String.length  prefix_cygdrive in
  (String.sub s 0 len =  prefix_cygdrive)
   &&  s.[len]='cygdrive_disk_location-1' 
   && is_win_drive s.[cygdrive_disk_location] 
   && s.[cygdrive_disk_location+1]='/'

let get_cygdrive_drive s= 
  assert(is_cygdrive s);
  s.[cygdrive_disk_location] 

let get_cygdrive_path s=
  assert(is_cygdrive s);
  String.sub s cygdrive_path_location (String.length s - cygdrive_disk_location)

let uncygdrive s =
  assert( String.sub s 0 (String.length  prefix_cygdrive) =  prefix_cygdrive);
  (get_cygdrive_drive s),(get_cygdrive_path s)

(*let prefix_cygdrive = "/cygdrive"
(* 
let anti_slash s = string_str_map (function '/' -> escaped_anti_slash_char 2  | c -> String.make 1 c) s path
  let unix_root_cygdrive_path = sprintf "/cygdrive/%c/%s/" (fst where_is_cygwin) (snd where_is_cygwin)
  let windows_root_cygdrive_path = sprintf "%c:\\%s\\" (fst where_is_cygwin) (anti_slash (snd where_is_cygwin)) *)

let cygdrive disk path = Unix( [ "cygdrive" ; String.make 1 disk ] @ path )

let cygdrive_disk_location = String.length prefix_cygdrive + 1 (*/*)
let cygdrive_path_location = String.length prefix_cygdrive + 1 (*/*) + 1 (* c *) + 1 (* / *)
*)


(* let regexp_slash     = Str.regexp "/"
let regexp_antislash = Str.regexp "\\\\" *)

(* let split_unix    s = Str.split regexp_slash s
let split_windows s = Str.split regexp_antislash s *)

*)
