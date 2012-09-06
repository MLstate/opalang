(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
module U = Unix

(** TODO - plugins dependencies *)
##property[mli]
##extern-type time_t = int
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##extern-type binary = Buf.t
##extern-type llarray('a) = Obj.t array
##extern-type tuple_2('a, 'b) =  ('a, 'b) BslUtils.opa_tuple_2
##property[endmli]

(** *****************************)

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
  ##register of_string : string, binary -> void
  let of_string n content =
    let och =
      let path = Filename.dirname n in
      ignore (File.check_create_path path);
	open_out n
    in output_string och (Buf.contents content) ; close_out och

##register create_full_path: string -> void
let create_full_path path = ignore (File.check_create_path path)

let buffer_of_string s =
  let b = Buf.create (String.length s) in
  Buf.add_string b s;
  b

##register content_opt: string -> option(binary)
let content_opt x =
  Option.map buffer_of_string (File.content_opt x)



(**
   {1 Deprecated}
*)
(*Deprecated: use [content_cps]*)
##register content : string -> binary
let content x = buffer_of_string (File.content x)

(**
   {1 Todo}
*)
##extern-type File.watcher

##register onchange : string, bool, (string, string -> void) -> File.watcher
let onchange _ _ _ = assert false

##register filewatcher_stop : File.watcher -> void
let filewatcher_stop _ = assert false

##register [cps-bypass] readdir : string, continuation(tuple_2(string,llarray(string))) -> void
let readdir dir cont =
  let cont t = QmlCpsServerLib.return cont (BslUtils.opa_tuple_2 t) in
  try
    let dir_handle = Unix.opendir dir in
    let rec next l () =
      try
        let l = (Unix.readdir dir_handle)::l in
        Scheduler.push Scheduler.default (next l)
      with
      | ex -> (
        Unix.closedir dir_handle;
        cont ((if ex=End_of_file then "" else "error while reading directory"),(Array.of_list (List.rev l)))
      )
    in Scheduler.push Scheduler.default (next [])
  with ex -> cont ("can not open dir "^dir,[||])
