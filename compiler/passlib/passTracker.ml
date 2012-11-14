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

(* depends *)
module String = BaseString
module Marshal = BaseMarshal

let error fmt = OManager.error ("OpaTracker."^^fmt^^"\n")

type index = int
type info = string
type passname = string
type printer_id = string
type tracker_id = string
type filename = string
type 'a printer = Format.formatter -> 'a -> unit
type 'a outputer = Pervasives.out_channel -> 'a -> unit
type tag = int
type t =
    {
      index : index ;
      info : info ;
      mutable tags : (tag, Obj.t) Hashtbl.t option (* majority of case: None, better than empty hashtbl *)
    }
type iter_tracker = {
  track : 'tracked. (filename -> 'tracked printer -> 'tracked -> unit) ;
}
type 'env tracker = iter_tracker -> 'env -> unit

let info t = t.info
let next =
  let i = ref (-1) in
  (fun info -> incr(i); { index = !i ; info = info ; tags = None})

let next_tag =
  let i = ref (-1) in
  (fun () -> incr(i); !i)

let embed () =
  let tag = next_tag () in
  let set t alpha =
    let obj = Obj.repr alpha in
    match t.tags with
    | None ->
        let ht = Hashtbl.create 5 in
        Hashtbl.add ht tag obj;
        t.tags <- Some ht
    | Some ht -> Hashtbl.replace ht tag obj (* GC *)
  and get t =
    match t.tags with
    | None -> None
    | Some ht -> (
        try Some (Obj.obj (Hashtbl.find ht tag))
        with Not_found -> None
      )
  in set, get

let started = ref false
let directory_name = ref "_tracks"
let directory = ref (!directory_name)

let handle_open_out ?(append=false) file =
  try
    if append
    then
      Pervasives.open_out_gen
        [Open_wronly; Open_creat; Open_append; Open_text]
        0o666
        file
    else
      Pervasives.open_out file
  with
  | Sys_error s ->
      error "Output.start: cannot open_out %s : %s" file s

let handle_open_in file =
  try open_in file
  with
  | Sys_error s ->
      error "Input.start: cann open_int %s: %s" file s

let handle_close_out oc =
  try close_out oc
  with
  | Sys_error s ->
      error "Output.start: cannot close_out in %s : %s" !directory s

let handle_close_in ic =
  try close_in ic
  with
  | Sys_error s ->
      error "Input.start: cannot close_int in %s : %s" !directory s

let handle_mkdir path =
  if not (File.check_create_path ~isdirectory:true path) then
    error "Output.start: cannot create directory %s" path

(* Conventions over name of files and directories. *)
(* <!> Keep it synchronized with 'opatrack.sh' and documentation *)
let name_passes_list = "passes.list"
let name_printers_list = "printers.list"
let name_trackers_list = "trackers.list"
let name_printers_dir = "printers"
let name_trackers_dir = "trackers"
let name_internal_dir = "internal"
let name_files_dir = "files"
let name_check_dir = "check"
let name_time_dir = "time"
let name_all_time = "all.time"
let marshal_filename = "marshal"

let passes_list = ref stdout
let printers_list = ref stdout
let trackers_list = ref stdout

(* caching everything for a image of the system file *)
let passes = ( ( Hashtbl.create 10 ) : (passname, unit) Hashtbl.t )
(* association between passes and printers *)
let pass_printer = ( ( ListHashtbl.create 10 ) : (passname, printer_id) ListHashtbl.t )
let printer_pass = ( ( ListHashtbl.create 10 ) : (printer_id, passname) ListHashtbl.t )
(* association between passes and trackers *)
let pass_tracker = ( ( ListHashtbl.create 10 ) : (passname, tracker_id) ListHashtbl.t )
let tracker_pass = ( ( ListHashtbl.create 10 ) : (tracker_id, passname) ListHashtbl.t )
(* association between tracker_id and filenames *)
let tracker_oc = ( (Hashtbl.create 10 ) : (tracker_id, out_channel) Hashtbl.t )
let tracker_filename = ( ( ListHashtbl.create 10 ) : (tracker_id, filename) ListHashtbl.t )

(* names generation *)
let build_passname_filename pass = Printf.sprintf "pass_%s" pass
let build_passtime_filename pass = Printf.sprintf "pass_%s.time" pass
let build_printer_id_filename printer_id = printer_id
let build_tracker_id_filename tracker_id = tracker_id
let build_internal_filename file index = Printf.sprintf "%s.%d" file index
let build_check_filename file index = Printf.sprintf "%s.%d" file index
let build_tracker_id_list tracker_id = Printf.sprintf "%s.list" (build_tracker_id_filename tracker_id)

(* for the `tracker directive *)
let build_track_name t = Printf.sprintf "index.%d" t.index

(*
  Clear hashtbl for restart
*)
let clear_tables () =
  Hashtbl.clear passes ;
  ListHashtbl.clear pass_printer ;
  ListHashtbl.clear printer_pass ;
  ListHashtbl.clear pass_tracker ;
  ListHashtbl.clear tracker_pass ;
  Hashtbl.clear tracker_oc ;
  ListHashtbl.clear tracker_filename ;
  ()

(* keep list files coherent online *)
let output_pass passname =
  if not ( Hashtbl.mem passes passname )
  then (
    Printf.fprintf !passes_list "%s\n%!" (build_passname_filename passname);
    Hashtbl.add passes passname ()
  )

let output_printer passname printer_id =
  output_pass passname;
  if not ( ListHashtbl.mem printer_pass printer_id )
  then (
    Printf.fprintf !printers_list "%s\n%!" (build_printer_id_filename printer_id);
    ListHashtbl.add printer_pass printer_id passname;
    ListHashtbl.add pass_printer passname printer_id
  )

let output_tracker passname tracker_id =
  output_pass passname;
  if not ( Hashtbl.mem tracker_oc tracker_id )
  then (
    let path = Filename.concat !directory name_trackers_dir in
    let file = Filename.concat path (build_tracker_id_list tracker_id) in
    let oc = handle_open_out file in
    Hashtbl.add tracker_oc tracker_id oc
  );
  if not ( ListHashtbl.mem tracker_pass tracker_id )
  then (
    Printf.fprintf !trackers_list "%s\n%!" (build_tracker_id_filename tracker_id);
    ListHashtbl.add tracker_pass tracker_id passname;
    ListHashtbl.add pass_tracker passname tracker_id;
  )

let output_tracked tracker_id filename =
  let oc =
    try
      Hashtbl.find tracker_oc tracker_id
    with
    | Not_found -> error "Output.output_tracked: no channel available for tracker: %s" tracker_id
  in
  if not ( ListHashtbl.mem_cp tracker_filename (tracker_id, filename) )
  then (
    Printf.fprintf oc "%s\n%!" filename;
    ListHashtbl.add tracker_filename tracker_id filename
  )

let set_directory dirname =
  if !started then error "Output.set_directory: already started"
  else directory_name := dirname

let get_directory () = !directory

let finalize () =
  if not !started then () else (
    handle_close_out !passes_list;
    handle_close_out !printers_list;
    handle_close_out !trackers_list;
    Hashtbl.iter (fun _ oc -> handle_close_out oc) tracker_oc
  )

let all_time = ref 0.0

let start () =
  let new_directory =
    let directory mode =
      match mode with
      | `prelude -> Filename.concat !directory_name "prelude"
      | `init -> Filename.concat !directory_name "init"
      | `linking -> !directory_name
      | `compilation ->
          let compilation_directory = Option.default "" (ObjectFiles.get_compilation_directory ()) in
          Filename.concat compilation_directory !directory_name
    in
    let mode = ObjectFiles.compilation_mode () in
    if ObjectFiles.Arg.is_separated () then directory mode
    else if mode = `prelude then directory `linking else directory mode
  in
  let new_directory =
    if Filename.is_relative new_directory then
      Filename.concat (Sys.getcwd ()) new_directory
    else new_directory
  in
  let same_directory = new_directory = !directory in
  directory := new_directory ;
  if !started && same_directory then () else (
    if !started then (
      clear_tables () ;
      finalize () ;
    ) ;
    all_time := 0.0 ;
    handle_mkdir !directory ;
    let handle_open_out file = handle_open_out (Filename.concat !directory file) in
    started := true ;
    passes_list := handle_open_out name_passes_list ;
    printers_list := handle_open_out name_printers_list ;
    trackers_list := handle_open_out name_trackers_list ;
    let directory_trackers = Filename.concat !directory name_trackers_dir in
    handle_mkdir directory_trackers;
    let directory_time = Filename.concat !directory name_time_dir in
    handle_mkdir directory_time
  )

(*
  + should not be used, because printer_id and tracker_id should rather be abstract
  types, with a constructor storing id in a table for PassHandler.Arg to be smart.
  + is used for filename generation of trackers
*)
let digest s = String.sub (Digest.to_hex (Digest.string s)) 0 8
let protect_filename s =
  if s <> "" && Base.String.is_word s then s else digest s

let print ~passname ~printer_id printer env =
  start () ;
  output_printer passname printer_id;
  let oc =
    let pass = build_passname_filename passname in
    let path = Filename.concat !directory pass in
    let path = Filename.concat path name_printers_dir in
    handle_mkdir path ;
    let file = Filename.concat path (build_printer_id_filename printer_id) in
    handle_open_out file
  in
  let fmt = Format.formatter_of_out_channel oc in
  printer fmt env ;
  Format.pp_print_flush fmt () ;
  handle_close_out oc

let track ~passname ~tracker_id tracker env =
  start () ;
  output_tracker passname tracker_id;
  let pass = build_passname_filename passname in
  let path = Filename.concat !directory pass in
  let path = Filename.concat path name_trackers_dir in
  let path = Filename.concat path (build_tracker_id_filename tracker_id) in
  handle_mkdir path ;
  let track filename printer tracked =
    let filename = protect_filename filename in
    (* add the filename to the list of tracked for this tracker_id *)
    output_tracked tracker_id filename;
    let file = Filename.concat path filename in
    let oc = handle_open_out file in
    let fmt = Format.formatter_of_out_channel oc in
    printer fmt tracked ;
    Format.pp_print_flush fmt () ;
    handle_close_out oc
  in
  tracker { track = track } env

let time ~passname time =
  start () ;
  output_pass passname;
  let oc =
    let path = Filename.concat !directory name_time_dir in
    handle_mkdir path ;
    let file = Filename.concat path (build_passtime_filename passname) in
    handle_open_out file
  in
  Printf.fprintf oc "%f\n%!" time;
  handle_close_out oc ;
  all_time := !all_time +. time ;
  let oc =
    let path = Filename.concat !directory name_time_dir in
    let file = Filename.concat path name_all_time in
    handle_open_out file
  in
  Printf.fprintf oc "%f\n%!" !all_time;
  handle_close_out oc

let global_env_stack : ((Obj.t -> unit) * (unit -> Obj.t)) list ref = ref []
let register_global_env (cap : ('a -> unit) * (unit -> 'a)) = global_env_stack := Obj.magic cap :: !global_env_stack
let register_global_ref r = register_global_env ((fun v -> r := v), (fun () -> !r))
let get_global_values () = List.map (fun (_,reader) -> Obj.obj (reader ())) !global_env_stack
let set_global_values values = List.iter2 (fun (writer,_) value -> writer (Obj.repr value)) !global_env_stack values

let marshal ~passname env =
  let (/) = Filename.concat in
  start ();
  output_pass passname;
  let pass = build_passname_filename passname in
  let oc =
    let path = !directory/pass in
    handle_mkdir path;
    let file = path/marshal_filename in
    handle_open_out file in
  Marshal.marshal_no_fun oc (env :: get_global_values ());
  handle_close_out oc

let unmarshal ~passname =
  let (/) = Filename.concat in
  start ();
  output_pass passname;
  let pass = build_passname_filename passname in
  let ic =
    let file = !directory/pass/marshal_filename in
    handle_open_in file in
  match Marshal.unmarshal_no_fun ic with
  | [] -> assert false
  | v :: values -> set_global_values values;
      handle_close_in ic;
      v

let current_passname = ref "default"
let set_current_passname passname = current_passname := passname
let make_fresh () =
  let t = Hashtbl.create 10 in
  (fun passname filename ->
     let key = passname, filename in
     let index = try Hashtbl.find t key with Not_found -> 0 in
     Hashtbl.add t key (succ index);
     index)
let internal_fresh = make_fresh ()
let internal ~filename fmt env =
  start () ;
  output_pass !current_passname ;
  let oc =
    let pass = build_passname_filename !current_passname in
    let path = Filename.concat !directory pass in
    let path = Filename.concat path name_internal_dir in
    handle_mkdir path ;
    let index = internal_fresh !current_passname filename in
    let filename = Filename.concat path (build_internal_filename filename index) in
    handle_open_out filename
  in
  let ff = Format.formatter_of_out_channel oc in
  fmt ff env ;
  Format.pp_print_flush ff () ;
  handle_close_out oc

let append_file =
  let table = Hashtbl.create 16 in
  (fun filename ->
     if Hashtbl.mem table filename
     then true
     else (
       Hashtbl.add table filename ();
       false
     ))

let file ~filename fmt env =
  start () ;
  output_pass !current_passname ;
  let filename =
    let pass = build_passname_filename !current_passname in
    let path = Filename.concat !directory pass in
    let path = Filename.concat path name_files_dir in
    handle_mkdir path ;
    let filename = Filename.concat path filename in
    filename
  in
  let oc =
    let append = append_file filename in
    handle_open_out ~append filename
  in
  fmt oc env ;
  Pervasives.flush oc ;
  handle_close_out oc ;
  filename


let check_fresh = make_fresh ()
let check_fail ~filename fmt env =
  start () ;
  output_pass !current_passname ;
  let oc =
    let pass = build_passname_filename !current_passname in
    let path = Filename.concat !directory pass in
    let path = Filename.concat path name_check_dir in
    handle_mkdir path ;
    let index = check_fresh !current_passname filename in
    let filename = Filename.concat path (build_check_filename filename index) in
    handle_open_out filename
  in
  let ff = Format.formatter_of_out_channel oc in
  fmt ff env ;
  Format.pp_print_flush ff () ;
  handle_close_out oc

(* exported in mli *)
let filename = build_track_name

let _ =
  Pervasives.at_exit finalize
