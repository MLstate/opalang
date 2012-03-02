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
(* depends *)
module Format = BaseFormat
module List = BaseList
module String = BaseString

(* -- *)

let default_rights = 0o600

let path_sep =
  if Base.is_windows then "\\"
  else "/"

let exists = Sys.file_exists

let extension name =
  let l = String.length name in
  let rec search_dot i =
    if i < 0 then ""
    else if String.unsafe_get name i = '.' then String.sub name (i + 1) (l - i - 1)
    else search_dot (pred i) in
  search_dot (pred l)

let chop_extension t = try Filename.chop_extension t with Invalid_argument _ -> t

let module_name t = String.capitalize (chop_extension (Filename.basename t))

(** OCaml-specific is_relative for -I **)
let is_relative_include_path dir =
  dir = "" || (dir.[0] <> '+' && Filename.is_relative dir)

let subst sl =
  fun s ->
    let ext = extension s in
    let chop = chop_extension s in
    match List.assoc_opt ext sl with
    | Some "" -> chop
    | Some e2 -> chop^"."^e2
    | None -> s

let from_pattern pat =
  fun s ->
    let e = extension s in
    let f = chop_extension s in
    let _f = String.replace f "." "_" in
    let b = Filename.basename f in
    let _b = String.replace b "." "_" in
    let d = Filename.dirname s in
    let fold acc (pat, rep) = String.replace acc pat rep in
    List.fold_left fold pat [("%b", b); ("%_b", _b); ("%d", d); ("%e", e); ("%_", _f); ("%", f)]


let content f =
  let stat = Unix.stat f in
  match stat.Unix.st_kind with
  | Unix.S_DIR -> failwith (Printf.sprintf "Base.File.content: %S is a directory" f)
  | Unix.S_LNK -> assert false (* stat goes through symbolic links *)
  | Unix.S_CHR  (* Character device *)
  | Unix.S_BLK  (* Block device *)
  | Unix.S_FIFO  (* Named pipe *)
  | Unix.S_SOCK  (* Socket *) ->
      (* for these kind of files, the size information is meaningless *)
      let ic = open_in_bin f in
      let len = 10000 in
      let str = String.create len in
      let buf = Buffer.create 10000 in
      let rec aux () =
        let read = input ic str 0 len in
        if read <> 0 then (
          Buffer.add_substring buf str 0 read;
          aux ()
        ) in
      aux ();
      close_in ic;
      Buffer.contents buf
  | Unix.S_REG  (* Regular file *) ->
      let size = stat.Unix.st_size in
      assert (size <= Sys.max_string_length) ;
      let ic = open_in_bin f
      and buf = String.create size in
      really_input ic buf 0 size ;
      close_in ic ;
      buf

let is_relative = Filename.is_relative

let normalize_relative_path path =
  (*Get rid of absolute paths*)
  if not (is_relative path) then None
  else
    let split = Str.split (Str.regexp path_sep) path in
    let rec aux ~acc = function
      | []      -> Some acc
      | "."::t  -> aux ~acc t
      | ".."::t ->
          begin
            match acc with
            | []    -> None (*Can't climb any higher*)
            | _::t' -> aux ~acc:t' t
          end
      | ""::t-> aux ~acc t
      | h::t -> aux ~acc:(h::acc) t
    in
    match aux ~acc:[] split with
    | None        -> None
    | Some result -> Some (String.concat "/" (List.rev result))

let explicit_path fpath pre =
  let path =
    if not(Filename.is_relative fpath) then
      fpath
    else
      match pre with
      | None -> fpath
      | Some "./" -> Filename.concat  (Sys.getcwd ()) fpath
      | Some pr ->
          if (Filename.is_relative pr) then
            Filename.concat (Sys.getcwd ()) (Filename.concat pr fpath)
          else
            Filename.concat pr fpath

  in
  let split = (Str.split (Str.regexp "/") path) in
  let revsplit = List.rev split in
  let rec parcours torm acc lst =
    match lst with
    | [] -> acc
    | x::y ->
        if x = "." || x = "" then
          parcours torm acc y
        else
          if x = ".." then
            parcours (torm + 1) acc y
          else
            if torm > 0 then
              parcours (torm -1) acc y
            else
              parcours torm (x::acc) y
  in
  let res = (parcours 0 [] revsplit) in
  String.concat "/" (""::res )

let clean_beginning_path path =
  let split =  (Str.split (Str.regexp "/") path) in
  let rec parcours lock acc lst =
    match lock,lst with
    | _,[] -> acc
    | false,(x::y) ->
        if x = "." || x = "" || x = ".." then
          parcours false acc y
        else
          parcours true (x::acc) y
    | true,(x::y) ->
        if x = "." || x = ".." then
          failwith ("\""^path^"\" : End of path is not clean")
        else
          if x = "" then
            parcours true acc y
          else
            parcours true (x::acc) y
  in
  String.concat "/" (""::(List.rev (parcours false [] split)))


let last_modification f =
  let t = (Unix.stat f).Unix.st_mtime in
  Time.of_unix_time t

(* files like /proc/cpuinfo have a 0kb size *)
(* this function is used for logs, /proc/*, etc.,
   where we really want to limit reading to max_size *)
(* TODO : progressive log allocation *)
(* FIXME: if the input stream is shorter than max_size,
   there can be garbage at the end of the buffer *)
let virtual_content max_size f =
  assert (max_size <= Sys.max_string_length) ;
  let ic = open_in f
  and buf = String.create max_size in
  (try
     really_input ic buf 0 max_size ;
   with End_of_file -> ()) ;
  close_in ic ;
  buf

(**FIXME : Catch another exception? *)
let content_opt f =
  try Some (content f)
  with Unix.Unix_error(_, ("opendir"|"stat"), _) -> None

(* FIXME: the file won't be closed if f raised an exception *)
let lines_fold f acc name =
  let ic = open_in name in
  let rec aux acc = match try Some (Pervasives.input_line ic) with End_of_file -> None with
    | Some line -> aux (f acc line)
    | None -> acc
  in
  let acc = aux acc in
  let _ = close_in ic in
  acc

(* FIXME: the file won't be closed if f raised an exception *)
let lines_foldi f acc name =
  let ic = open_in name in
  let rec aux nr acc = match try Some (Pervasives.input_line ic) with End_of_file -> None with
    | Some line -> aux (nr + 1) (f acc line nr)
    | None -> acc
  in
  let acc = aux 1 acc in
  let _ = close_in ic in
  acc

let lines_foldi_offset f acc name =
  let acc, _ =
    lines_foldi (fun (acc,offset) line i ->
                   let acc = f acc line i offset in
                   (acc, offset + String.length line)) (acc,0) name in
  acc

let lines_rev_mapi f name =
  let aux acc line i = (f line i)::acc in
  lines_foldi aux [] name

let lines_mapi f name =
  List.rev (lines_rev_mapi f name)

let lines_map_and_fold_i f acc name =
  let aux (acc_fold, acc_map) line i =
    let acc_fold, elt_map = f acc_fold line i in
    acc_fold, elt_map::acc_map
  in
  let acc_fold, acc_map = lines_foldi aux (acc, []) name in
  acc_fold, (List.rev acc_map)

(* FIXME: workaround for Filename.concat *)
let concat dir f =
  Filename.concat dir (
    match Sys.os_type with
    | "Unix"| "Cygwin"  ->
        let lf = String.length f in
        if lf > 0 && f.[0] = '/' then String.sub f 1 (pred lf) else f

    | "Win32" | "Windows" ->
        let lf = String.length f in
        if lf > 1 && f.[1] = ':' then String.sub f 2 (pred lf) else f

    | _ -> failwith ("Base.File.concat : unknown system "^Sys.os_type)
  )

let output f s =
  try
    let oc = open_out f in
    output_string oc s ;
    close_out oc ;
    true
  with Sys_error _ -> false

let oc_output filename printer data =
  try
    let oc = open_out filename in
    printer oc data ;
    close_out oc ;
    None
  with
  | Sys_error msg -> Some msg

let pp_output filename printer data =
  let printer oc data =
    let fmt = Format.formatter_of_out_channel oc in
    printer fmt data ;
    Format.pp_print_flush fmt ()
  in
  oc_output filename printer data

(* Create a temporary directory with a random unique name *)
(* ex: mkdtemp "/tmp" *)
let rec mkdtemp path =
  try
    (* N.B: on Mac, the path_sep may be duplicated,
       especially if path = Filename.temp_dir_name *)
    let dirname = path ^ path_sep ^ String.random 10 in
    Unix.mkdir dirname 0o700;
    dirname
  with
    Unix.Unix_error (Unix.EEXIST, _, _) -> mkdtemp path

(* doesn't remove symlinks everywhere in the path, it just guarantees that
 * the file pointed to by the returned path is not a symlink *)
let rec remove_symlinks path =
  (* BEWARE: if you make loops with symlinks, then this function will loop *)
  let path' =
    try
      let link = Unix.readlink path in
      if is_relative link then Filename.concat (Filename.dirname path) link
      else link
    with Unix.Unix_error _ | Invalid_argument _ -> path in
  if path' = path
  then path'
  else remove_symlinks path'

(* remove symlinks everywhere in the path *)
let rec remove_all_symlinks path =
  if path = "." || path = "/" then path else
    let dirname = Filename.dirname path in
    let dirname = remove_all_symlinks dirname in
    let path = Filename.concat dirname (Filename.basename path) in
    remove_symlinks path

(** crée tous les répertoires nécessaires pour accéder à path *)
let check_create_path ?(rights=0o755) path =
  let path =
    if Filename.basename path = "." then path
    else path ^ "/" in
  let path = Mlstate_platform.platform_dependent ~unix:remove_all_symlinks ~windows:(fun x->x) () path in
  let rec aux1 = function
      [] -> aux1 [Filename.dirname path]
    | (hd :: tl) as l ->
        let d = Filename.dirname hd in
        if d <> hd then aux1 (d :: l)
        else tl (* car hd = C:\ ou / *) in
  let mkdir d = try Unix.mkdir d rights; true with Unix.Unix_error _ -> false in
  List.for_all (fun x -> Sys.file_exists x || mkdir x) (aux1 [])

(** hopefully, this is the ultimate and portable version of cp *)
let copy ?(force=false) src tgt =
  if not (Sys.file_exists src) then 1
  else
    if src = tgt then 0
      (* FIXME if you have symbolic links, this check is not enough
       * and if [force] is set, then you will end up deleting your file *)
    else
      begin
        if force && (Sys.file_exists tgt) then Sys.remove tgt;
        if Sys.file_exists tgt then 1
        else
          let dir = Filename.dirname tgt in
          if check_create_path dir then
            let command = (if Base.is_windows then Format.sprintf  "copy \"%s\" \"%s\"" else Format.sprintf "cp \"%s\" \"%s\"") src tgt in
            Sys.command command
          else 1
      end

(** hopefully, this is the ultimate and portable version of mv *)
let mv ?(force=false) src tgt =
  if not (Sys.file_exists src) then 1
  else
    if src = tgt then 0
      (* FIXME if you have symbolic links, this check is not enough
       * and if [force] is set, then you will end up deleting your file *)
    else
      begin
        if force && (Sys.file_exists tgt) then Sys.remove tgt;
        if Sys.file_exists tgt then 1
        else
          let dir = Filename.dirname tgt in
          if check_create_path dir then
            let command = (if Base.is_windows then Format.sprintf  "rename \"%s\" \"%s\"" else Format.sprintf "mv \"%s\" \"%s\"") src tgt in
            Sys.command command
          else 1
      end

exception NoMLstateDir

(* In case of failure, mlstate_dir is in the same place as the program.
   Make this lazy? *)
let mlstate_dir = lazy (
  try
    let path = match Sys.os_type with
      | "Unix" ->
	  (* begin match Config.os with *)
	  (* | Config.Mac -> Filename.concat (Sys.getenv "HOME") "Library/Application Support/Opa/" *)
	  (* | _ ->  *) Filename.concat (Sys.getenv "HOME") ".opa/"
	  (* end *)
      | "Cygwin" -> Filename.concat (Sys.getenv "HOME") ".opa/"
      | "Win32" -> Filename.concat (Sys.getenv "USERPROFILE") "AppData\\Local\\MLstate\\"
      | s -> failwith (Printf.sprintf "Base.ml_state_dir : this platform (%s) is yet unsupported" s) in
    (* assert (check_create_path ~rights:0o700 path) ; *)
    if check_create_path ~rights:0o700 path then path
    else ".opa/" (* raise NoMLstateDir *)
  with Not_found -> ".opa/"
)

let mlstatelibs =
  lazy (
    try
      let path = Sys.getenv "MLSTATELIBS" in
      PathTransform.string_to_mysys path
    with Not_found -> failwith "Environnement variable MLSTATELIBS not defined." )


let is_regular file =
  (* Jounral.Interface.jlog (Printf.sprintf  "file=%s" file) ; *)
  try
    let st = Unix.stat file in
    st.Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let is_directory path =
  (* Jounral.Interface.jlog (Printf.sprintf  "file=%s" file) ; *)
  try
    let st = Unix.stat path in
    st.Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false

let mimetype ?mime_db file =
  let database =
    match mime_db with
    | Some s -> File_mimetype.get_mimetype_database s
    | None -> File_mimetype.mimetype_database mlstatelibs in
  try
    File_mimetype.get_mimetype file database
  with File_mimetype.MimeType_NotFound -> (
    (* Journal.Interface.warning (Printf.sprintf "\"%s\" : error, don't find the mimetype !! replace by default, i.e 'text/plain'\n" file); *)
    "text/plain"
  )


let rec iter_dir_rec ?(showdir=false) f d =
  let dh = Unix.opendir d in
  try
    while true do
      let x = Unix.readdir dh in
      let path = Filename.concat d x in
      let st = Unix.stat path in
      match st.Unix.st_kind with
      | Unix.S_REG -> f ~name:x ~path
      | Unix.S_DIR ->
          if showdir && x="." then f ~name:x ~path:d
          else if x="." or x=".." then ()
          else iter_dir_rec f path ~showdir
      | _ -> ()
    done
  with
    End_of_file -> Unix.closedir dh

(* FIXME: tester les iter/fold rec ou non *)
let fold_dir_rec f i d =
  let rec aux d dh r =
    try
      let x = Unix.readdir dh in
      let path = Filename.concat d x in
      let st = Unix.stat path in
      match st.Unix.st_kind with
      | Unix.S_REG -> aux d dh (f r ~name:x ~path)
      | Unix.S_DIR ->
          if x="." or x=".." then aux d dh r
          else aux d dh (aux path (Unix.opendir path) r)
      | _ -> aux d dh r
    with
      End_of_file -> Unix.closedir dh ; r
    | Unix.Unix_error _ -> aux d dh r
  in
  aux d (Unix.opendir d) i

let rec remove_rec file =
  let sto = try Some (Unix.stat file) with Unix.Unix_error _ -> None in
  match sto with
  | None -> ()
  | Some st ->
      match st.Unix.st_kind with
      | Unix.S_REG
      | Unix.S_LNK
      | Unix.S_FIFO
      | Unix.S_SOCK ->
          Unix.unlink file
      | Unix.S_CHR
      | Unix.S_BLK ->
          Base.invalid_argf "File.remove_rec: %s" file
      | Unix.S_DIR ->
          let dir = file in
          let handle = Unix.opendir dir in
          try
            while true do
              match Unix.readdir handle with
              | "." | ".." -> ()
              | file ->
                  let path = Filename.concat dir file in
                  remove_rec path
            done;
          with End_of_file ->
            Unix.closedir handle;
            Unix.rmdir dir

(** itère une fonction sur un répertoire / non récursif, ignore les répertoires ! *)
let iter_dir f d =
  let dh = Unix.opendir d in
  try
    while true do
      let x = Unix.readdir dh in
      let y = concat d x in
      if is_regular y then f ~name:x ~path:y
    done
  with
    End_of_file -> Unix.closedir dh

(** itère une fonction sur un répertoire / non récursif, ignore les répertoires ! *)
(* FIXME: remplacer ici . par getcwd ? *)
let fold_dir f i d =
  let dh = Unix.opendir d in
  let rec aux r =
    try
      let x = Unix.readdir dh in
      let y = concat d x in
      aux (if is_regular y then f r ~name:x ~path:y else r)
    with
      End_of_file -> Unix.closedir dh ; r
  in
  aux i

(* FIXME: optimiser si le dir n'a pas changé (lors d'une saisie lettre par lettre)... *)
(* FIXME: Glib.Convert.filename_to_utf8 ! *)
let completion path =
  let dir = Filename.dirname path (* FIXME: tenir compte de ~ *)
  and base = Filename.basename path in
  let f_comp acc ~name ~path =
    if String.is_substring base name 0 then (if dir="." then name else path)::acc
    else acc in
  fold_dir f_comp [] (if dir = "." then Unix.getcwd () else dir)

let backup_path name =
  let path = concat (Lazy.force mlstate_dir) (name ^ path_sep) in
  (* Journal.Interface.jlog (Printf.sprintf "backup_path = %s" path) ; *)
  if check_create_path path then Some path
  else (
    (* Journal.Interface.warning "backup path could not be created. NO automatic backups!" ; *)
    None
  )


let append_or_create name =
  let f = if Sys.file_exists name then
    open_out_gen [Open_wronly; Open_binary; Open_append] default_rights
  else
    open_out_gen [Open_wronly; Open_binary; Open_creat; Open_trunc] default_rights
  in f name

let channel_contents chan =
  let rec aux b = (* FIXME: recursion might stack overflow *)
    try aux (FBuffer.add b (input_line chan ^ "\n"))
    with End_of_file -> b
  in
  FBuffer.contents (aux (FBuffer.create ~name:"channel_contents" 256))

(** highly non portable (especially the cmd itself)
    please do not abuse or else you will be responsible of the Windows port :p *)
type filter = { process: string -> string;
                close : unit -> unit }

let new_filter cmd =
  let (inchan,outchan) as chans = Unix.open_process cmd in
  let process s =
    Pervasives.output_string outchan s;
    Pervasives.output_char outchan '\n';
    Pervasives.flush outchan;
    Pervasives.input_line inchan
  and close () =
    ignore (Unix.close_process chans)
  in
  { process = process;
    close = close }
    (** examples:
        let sed = new_filter "sed -u 's/foo/bar/'";;
        List.map sed.process ["bar";"foo";"foobar";"foofoo"] returns ["bar";"bar";"barbar";"barfoo"]
        sed.close() when the filter is not needed anymore *)

exception Process_error of string
let process_output cmd =
  let chan = Unix.open_process_in cmd in
  let lines = channel_contents chan in
  match Unix.close_process_in chan with
  | Unix.WEXITED 0 -> lines
  | Unix.WEXITED i -> raise (Process_error (Printf.sprintf "command failed with code %d" i))
  | Unix.WSIGNALED i -> raise (Process_error (Printf.sprintf "command killed by signal %d" i))
  | Unix.WSTOPPED i -> raise (Process_error (Printf.sprintf "command stopped by signal %d" i))

let simplify_path =
  let rec aux void = function
    | [] -> []
    | ".."::l -> ".."::(aux false l)
    | "."::l
    | _::".."::l ->
        begin match l with
        | _::_ -> aux void l
        | _ when not void -> aux void l
        | [] -> ["."]
        end
    | d::l ->
        let l = aux false l in
        begin match l with
        | ".."::_ -> aux void (d::l)
        | _ -> d::l
        end
  in
  fun path ->
    let len = String.length path in
    if len <= 1 then
      path
    else
      let b = path.[len-1] = path_sep.[0] in
      let l = String.slice path_sep.[0] path in
      let b2 = path.[0] = path_sep.[0] in
      let l = aux (not b2) l in
      let path = String.concat path_sep l in
      let path = if b2 then path_sep ^ path else path in
      let path = if b && l <> [] then path ^ path_sep else path in
      path


let get_locations_regexp ?(dir=false) directories regexp =
  let regexp = Str.regexp (regexp^"$") in
  List.concat_map (
    fun p ->
      let files = Array.to_list (Sys.readdir p) in
      let matching_files = List.filter (fun s -> Str.string_match regexp s 0) files in
      let fullnames = List.map (Filename.concat p) matching_files in
      List.filter (if dir then is_directory else is_regular) fullnames
  ) directories

let get_locations ?(dir=false) directories filename =
  List.filter_map (
    fun p ->
      let fullname = Filename.concat p filename in
      if (if dir then is_directory else is_regular) fullname
      then Some fullname
      else None
  ) directories

let get_one_location ?dir
    ?(missing_file=fun _dirs fname -> failwith (Printf.sprintf "get_one_location : missing file %s" fname))
    ?(many_files=fun _dirs fname l ->
        let fullname =  List.hd l in
        Printf.printf "get_one_location : \"%s\" found in several places. I will use \"%s\"" fname fullname; fullname
     )
    directories filename =
  let found_files = get_locations ?dir directories filename in
  match found_files with
  | [fullname] -> fullname
  | [] -> missing_file  directories filename
  | _ -> many_files directories filename found_files
