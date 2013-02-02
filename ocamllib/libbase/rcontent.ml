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
(* Rcontent: module for handling multiple http content types. *)
let eprintf fmt = Format.kfprintf (fun _ -> Format.pp_print_flush Format.err_formatter ()) Format.err_formatter fmt
module List = Base.List
module String = Base.String

type content =
  | ContentString of string
  | ContentBuffer of Buffer.t
  | ContentFBuffer of FBuffer.t
  | ContentFile of string * in_channel option * out_channel option * Unix.stats option * bool
  | ContentNone

type content_type = CT_STRING | CT_BUFFER | CT_FBUFFER | CT_FILE | CT_NONE

let string_of_content_type = function
  | CT_STRING -> "CT_STRING"
  | CT_BUFFER -> "CT_BUFFER"
  | CT_FBUFFER -> "CT_FBUFFER"
  | CT_FILE -> "CT_FILE"
  | CT_NONE -> "CT_NONE"

let content_temporary_files = ((ref []):string list ref)

let remove_temporary_file file =
  content_temporary_files := List.filter (fun f -> f <> file) (!content_temporary_files)

let content_unlink_temporary_files () =
  List.iter (fun f -> if File.exists f then ((*eprintf "Unlinking: %s\n" f;*) Unix.unlink f)) (!content_temporary_files);
  content_temporary_files := []

let create_tmp_file () =
  let (tmpfile,file) = Filename.open_temp_file ~mode:[Open_binary] "content_" "_tnetnoc" in
  content_temporary_files := tmpfile::(!content_temporary_files);
  (tmpfile,file)

let to_tmp_file f a close =
  match create_tmp_file () with
  | (tmpfile,file) ->
      f file a;
      if close
      then (close_out file; ContentFile (tmpfile, None, None, Some (Unix.stat tmpfile),true))
      else ContentFile (tmpfile, None, Some file, Some (Unix.stat tmpfile),true)

let get_content_type = function
  | ContentString _ -> CT_STRING
  | ContentBuffer _ -> CT_BUFFER
  | ContentFBuffer _ -> CT_FBUFFER
  | ContentFile _ -> CT_FILE
  | ContentNone -> CT_NONE

let get_content = function
  | ContentString s -> s
  | ContentBuffer b -> Buffer.contents b
  | ContentFBuffer b -> FBuffer.contents b
  | ContentFile (f,_,None,_,_) -> File.content f
  | ContentFile (f,_,Some oc,_,_) -> (Pervasives.flush oc; File.content f)
  | ContentNone -> ""

let content_make ?(truncate=false) ?(hint=1024) = function
  | CT_STRING -> ContentString ""
  | CT_BUFFER -> ContentBuffer (Buffer.create hint)
  | CT_FBUFFER -> ContentFBuffer (FBuffer.create hint)
  | CT_FILE ->
      let (tmpfile, file) = create_tmp_file () in
      if truncate then Unix.truncate tmpfile hint; (* <-- Be careful with this *)
      ContentFile (tmpfile,None,Some file,Some (Unix.stat tmpfile),true)
  | CT_NONE -> ContentNone

let content_unallocate = function
  | ContentString _ -> ()
  | ContentBuffer b -> Buffer.clear b
  | ContentFBuffer _ -> ()
  | ContentFile (f,ic_opt,oc_opt,_,unlinkable) ->
      (try (match ic_opt with Some ic -> close_in ic; | None -> ()) with Sys_error _ -> ());
      (try (match oc_opt with Some oc -> close_out oc; | None -> ()) with Sys_error _ -> ());
      content_temporary_files := List.filter (fun _f -> _f <> f) (!content_temporary_files);
      if unlinkable && File.exists f then ((*eprintf "Unlinking: %s\n" f;*) Unix.unlink f);
  | ContentNone -> ()

let content_add ?(evolve=true) ?(max_buffer_size=(10*1024*1024)) str = function
  | ContentString s ->
      if evolve
      then
        let sstr = s^str in
        let len = String.length sstr in
        let b = Buffer.create (len * 10) in
        Buffer.add_string b sstr; ContentBuffer b
      else
        ContentString (s^str)
  | ContentBuffer b ->
      (Buffer.add_string b str;
       if evolve && Buffer.length b >= max_buffer_size
       then to_tmp_file Buffer.output_buffer b false
       else ContentBuffer b)
  | ContentFBuffer b -> ContentFBuffer (FBuffer.add b str)
  | ContentFile (f,ic_opt,oc_opt,stat_opt,unlinkable) ->
      let oc_opt =
        match oc_opt with
        | Some oc -> Some oc
        | None -> Some (open_out_gen [Open_binary;Open_append] 0o777 f) in
      Pervasives.output_string (Option.get oc_opt) str;
      let stat_opt =
        match stat_opt with
          Some stat -> Some { stat with Unix.st_size = stat.Unix.st_size + String.length str }
        | None -> None in
      ContentFile (f,ic_opt,oc_opt,stat_opt,unlinkable)
  | ContentNone -> ContentString str

let content_add_content ?(max_buffer_size=(10*1024*1024)) from_content to_content =
  match from_content, to_content with
  | ContentNone, content -> content
  | content, ContentNone -> content
  | _, ContentString ts -> content_add ~max_buffer_size ts from_content
  | ContentBuffer fb, ContentBuffer tb ->
      (Buffer.add_buffer fb tb;
       if Buffer.length fb < max_buffer_size
       then ContentBuffer fb
       else to_tmp_file Buffer.output_buffer fb false)
  | ContentFBuffer fb, ContentFBuffer tb ->
      ContentFBuffer (FBuffer.union fb tb)
  | ContentFile (ff,fic_opt,foc_opt,fstat_opt,funlinkable), ContentFile (tf,tic_opt,toc_opt,_,_) ->
      ((match toc_opt with Some toc -> flush toc | None -> ());
       let tic_opt = match tic_opt with | Some tic -> (seek_in tic 0; Some tic) | None -> Some (open_in tf) in
       let foc_opt = match foc_opt with | Some foc -> Some foc | None -> Some (open_out_gen [Open_binary;Open_append] 0o777 ff) in
       let buf = String.create 4096 in
       let tic = Option.get tic_opt in
       let foc = Option.get foc_opt in
       let n = ref 1 in
       let cnt = ref 0 in
       while !n > 0 do
         n := Pervasives.input tic buf 0 4096;
         cnt := !cnt + !n;
         Pervasives.output foc buf 0 !n
       done;
       let fstat_opt =
         match fstat_opt with
           Some fstat -> Some { fstat with Unix.st_size = fstat.Unix.st_size + !cnt }
         | None -> None in
       ContentFile (ff,fic_opt,foc_opt,fstat_opt,funlinkable))
  | _, _ ->
      content_add ~max_buffer_size (get_content to_content) from_content

(*
let tstcac () =
  let rec op = function | [] -> [] | h::t as l -> (List.map (fun e -> h, e) l)@(op t) in
  let cts = [CT_STRING; CT_BUFFER; CT_FBUFFER; CT_FILE] in
  let lst = List.map (fun ct1ct2 -> ct1ct2, "abcdef") (op cts) in
  let mkc ct str = let c = content_make ct in content_add ~evolve:false str c in
  let mkp (ct1,str1,ct2,str2) = (mkc ct1 str1,mkc ct2 str2) in
  let tst (ct1,ct2) = let c1, c2 = mkp (ct1,"abc",ct2,"def") in get_content (content_add_content c1 c2) in
  let res = verifyfn tst (Sl.st2s string_of_content_type) (fun s -> s) lst in
  content_unlink_temporary_files ();
  res
*)

let bodystr ?(max_body=50) ?(escaped=false) ?(hex=false) content =
  let body,dots =
    match content with
    | ContentString s ->
      let len = String.length s in
      let body = String.sub s 0 (min len max_body) in
      let dots = len > max_body in
      (body,dots)
    | ContentBuffer b ->
      let len = Buffer.length b in
      let body = Buffer.sub b 0 (min len max_body) in
      let dots = len > max_body in
      (body,dots)
    | ContentFBuffer b ->
      let len = FBuffer.length b in
      let body = FBuffer.sub b 0 (min len max_body) in
      let dots = len > max_body in
      (body,dots)
    | ContentFile (filename,None,_,_,_) ->
      let buf = String.create (max_body+1) in
      let ic = open_in filename in
      let len = input ic buf 0 (max_body+1) in
      let () = close_in ic in
      let body = String.sub buf 0 (min len max_body) in
      let dots = len > max_body in
      (body,dots)
    | ContentFile (_,Some ic,_,_,_) ->
      let buf = String.create (max_body+1) in
      let () = seek_in ic 0 in
      let len = input ic buf 0 (max_body+1) in
      let body = String.sub buf 0 (min len max_body) in
      let dots = len > max_body in
      (body,dots)
    | ContentNone ->
      ("",false)
  in
  let hex = if hex then String.to_hex else fun x -> x in
  let esc = if escaped then String.escaped else fun x -> x in
    (esc (hex body))^(if dots then "..." else "")

let content_length = function
  | ContentString s -> String.length s
  | ContentBuffer b -> Buffer.length b
  | ContentFBuffer b -> FBuffer.length b
  | ContentFile (_,_,_,Some stat,_) -> stat.Unix.st_size
  | ContentFile (f,_,_,None,_) -> (Unix.stat f).Unix.st_size
  | ContentNone -> 0

let content_is_string = function
  | ContentString _ -> true
  | ContentBuffer _ -> true
  | ContentFBuffer _ -> true
  | ContentFile _ -> false
  | ContentNone -> true

let content_is_buffer = function ContentBuffer _ -> true | _ -> false

let content_is_fbuffer = function ContentFBuffer _ -> true | _ -> false

let content_is_file = function
  | ContentString _ -> false
  | ContentBuffer _ -> false
  | ContentFBuffer _ -> false
  | ContentFile _ -> true
  | ContentNone -> false

let content_force_string ?(unallocate=false) = function
  | ContentString str -> ContentString str
  | ContentBuffer buf -> ContentString (Buffer.contents buf)
  | ContentFBuffer buf -> ContentString (FBuffer.contents buf)
  | (ContentFile (f,_,oc_opt,_,_)) as c ->
      (match oc_opt with Some oc -> Pervasives.flush oc | None -> ());
      let str = File.content f in
      if unallocate then content_unallocate c;
      ContentString str
  | ContentNone -> ContentString ""

let content_from_file ?(unlinkable=false) ?(stat=true) filename =
  ContentFile (filename,None,None,(if stat then Some (Unix.stat filename) else None),unlinkable)

let content_force_file ?(close=false) = function
  | ContentString str -> to_tmp_file output_string str close
  | ContentBuffer buf -> to_tmp_file Buffer.output_buffer buf close
  | ContentFBuffer buf -> to_tmp_file FBuffer.output buf close
  | ContentFile (f,i,None,s,u) -> ContentFile (f,i,None,s,u)
  | ContentFile (f,i,Some oc,s,u) -> if close then (close_out oc; ContentFile (f,i,None,s,u)) else ContentFile (f,i,Some oc,s,u)
  | ContentNone -> to_tmp_file (fun _ _ -> ()) "" close

let content_rename_file ?(force=false) content target =
  let get_oc () =
    let mode = [Open_wronly;Open_creat;Open_binary]@(if force then [Open_trunc] else [Open_excl]) in
    Pervasives.open_out_gen mode 0o640 target
  in
  match content with
  | ContentString str -> let oc = get_oc () in output_string oc str; close_out oc; 1
  | ContentBuffer buf -> let oc = get_oc () in Buffer.output_buffer oc buf; close_out oc; 1
  | ContentFBuffer buf -> let oc = get_oc () in FBuffer.output oc buf; close_out oc; 1
  | ContentFile (file,ic_opt,oc_opt,_,_) ->
      (try (match ic_opt with Some ic -> close_in ic; | None -> ()) with Sys_error _ -> ());
      (try (match oc_opt with Some oc -> close_out oc; | None -> ()) with Sys_error _ -> ());
      remove_temporary_file file;
      File.mv ~force file target
  | ContentNone -> let oc = get_oc () in close_out oc; 1

let content_md5 = function
  | ContentString str -> Digest.to_hex (Digest.string str)
  | ContentBuffer buf -> Digest.to_hex (Digest.string (Buffer.contents buf))
  | ContentFBuffer buf -> Digest.to_hex (Digest.string (FBuffer.contents buf))
  | ContentFile (file,None,_,_,_) -> Digest.to_hex (Digest.file file)
  | ContentFile (_,Some ic,_,_,_) -> (seek_in ic 0; Digest.to_hex (Digest.input ic))
  | ContentNone -> Digest.to_hex (Digest.string "")

(*
let tst _type =
  let cnt = 3 in
  let c = content_make _type 100 in
  let rec aux n c1 = if n <= 0 then c1 else aux (n-1) (content_add "abc" c1) in
  let c1 = aux cnt c in
  Printf.printf "%sc1: '%s'\n" _type (escaped (get_content c1));
  Printf.printf "%sc1 length: %d\n" _type (content_length c1);
  Printf.printf "%sc1 md5: %s\n" _type (content_md5 c1);
  Printf.printf "%sc1 bodystr: %s\n" _type (bodystr ~max_body:25 ~hex:true c1);
  let sc = content_force_string (aux cnt c) in
  Printf.printf "%sc1: sc='%s'\n" _type (escaped (get_content sc));
  let fc = content_force_file (aux cnt c) in
  Printf.printf "%sc1: fc='%s'\n" _type (escaped (get_content fc));
  let c2 = aux (cnt * 100) c in
  let compressed, cc = content_compress true true 6 true c2 (content_length c2) in
  Printf.printf "%sc1: compressed=%b length=%d cc='%s'\n" _type compressed (content_length cc) (escaped (get_content cc));
  content_unallocate c;
  content_unallocate c1;
  content_unallocate fc;
  content_unallocate cc;
  if content_is_file c1 then content_unlink_temporary_files ()
;;

let _ = tst "s";;
let _ = tst "b";;
let _ = tst "fb";;
let _ = tst "f";;
let _ = tst "n";;
*)
