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
(* CF mli *)
module String = BaseString

(* type alias *)
type filename = string
type contents = string
type nonuid = SurfaceAst.nonuid

(* meta-information *)
let hash = OpaParserVersion.hash

(* low level parsing *)
let ll_factory parser_rule ?filename contents =
  let _filename = filename in
  let _start = None in
  let _pos, result = parser_rule ?_filename ?_start contents in
  result

let ll_expr = ll_factory Opa_parser.parse_opa_parser_expr_eoi
let ll_ty = ll_factory Opa_parser.parse_opa_parser_ty_eoi
let ll_code = ll_factory Opa_parser.parse_opa_parser_main_eoi

(* ====================================================================================== *)
(* TODO: clean-up *)
exception Specific_parse_error = Parser_utils.Specific_parse_error
let parse_file_content ~filename content =
  let _pos, code = Opa_parser.parse_opa_parser_main_eoi ~_filename:filename content in
  code

exception No_such_file of string
let parse_file filename =
  match File.content_opt filename with
  | None -> raise (No_such_file filename)
  | Some content -> parse_file_content ~filename content

let parse_content_expr ~filename content =
  let _pos, code = Opa_parser.parse_opa_parser_expr_eoi ~_filename:filename content in
  code
(* ====================================================================================== *)

module CacheParse =
struct
  open Digest
  open Unix

  let caching_directory = Lazy.lazy_from_val (
    Filename.concat (Lazy.force File.mlstate_dir) "opa/cache/parser"
  )
  let check_directory () =
    if Sys.file_exists (Lazy.force caching_directory) then ()
    else (
      #<If:TESTING>
        ()
        #<Else>
        OManager.verbose "opa-parser: creating @{<bright>%s@} to store opa compiler caches" (Lazy.force caching_directory)
        #<End>;
      if not (File.check_create_path (Lazy.force caching_directory))
      then
        OManager.error (
          "cannot create cache directory @{<bright>%s@}^@\n"^^
          "@{<bright>Hint@}:@\n"^^
          "You can try to create manually the directory, with permissions 755"
        )
          (Lazy.force caching_directory)
    )

  (* cache file naming option *)
  (* setting true everywhere minimize disk consumption *)
  (* cannot set the 3 to true !!!*)
  (* see filename *)
  let k_last_version = true (* only last compiler version *)  (* for faster debugging *)
  let k_last_hash    = true (* only last file content     *) (* not proof to undo redo *)
  let k_last_name    = false (* only last file name *) (* not proof to name changing,multiple file with same name *)

  let _ = assert(  (not k_last_name) || (not k_last_hash))

  let verify_last_version = true

  let keep_only last value =
    if (*keep_only*)last(*value*) then "last" else value

  (* name is form of [hash| "last"] concatenation for name hash version
     that is why having "last" everywhere is bad
  *)
  let filename name hash =
    let name = String.replace name "/" ":" in
    Printf.sprintf "%s/%s-%s-opaparse.%s.bin"
      (Lazy.force caching_directory)
      (keep_only k_last_name name)
      (keep_only k_last_hash hash)
      (Unix.gethostname ())

  let set name content result =
    let internal_version = OpaParserVersion.hash in
    let _ = check_directory () in
    let hash = to_hex (Digest.string content) in
    let filename = filename name hash in
    match (try Some (open_out_bin filename)
           with Sys_error _msg ->
             #<If:PARSER_CACHE_DEBUG>OManager.printf "Failed to write in %s: %s@." filename _msg#<End>;
             None
          ) with
    | None -> ()
    | Some c ->
        Marshal.to_channel c (name,hash,internal_version,(result: (string,'b) SurfaceAst.code)) [];
        close_out c

  let get name content =
    let internal_version = OpaParserVersion.hash in
    let _ = check_directory () in
    let hash = to_hex (Digest.string content) in
    let cache_file = filename name hash in
    if Sys.file_exists cache_file then (
      match (try Some (open_in_bin cache_file) with Sys_error _ -> None) with
      | None -> None
      | Some c ->
          let (_oldname,oldhash,oldversion,res) =
            try (Marshal.from_channel c : string * string * string * (string,'b) SurfaceAst.code)
            with End_of_file -> OManager.printf "Bad cache@\n" ;  "","","",[]
          in
          close_in c;
          if oldhash=hash && ((not verify_last_version) || oldversion=internal_version) then (Some res)
          else None
    ) else None
end

let rec get_index_N_lines_before s i nl =
  if nl=0 then i else
    let i' = try String.rindex_from s i '\n' with Not_found -> 0 in
    if i' = 0 then 0 else get_index_N_lines_before s (i'-1) (nl-1)

let get_index_N_lines_after s i nl =
  let len = String.length s - 1 in
  let rec get i nl =
    if nl=0 then i else
      let i' = try String.index_from s i '\n' with Not_found -> len in
      if i' = len then len else get (i'+1) (nl-1)
  in get i nl

let parse_error_flag =
  let search_for = Str.regexp "\\b[Uu][Tt][Ff]-?8\\b" in
  try
    let lang = try Sys.getenv "LC_CTYPE" with Not_found -> Sys.getenv "LANG" in
    let _ = Str.search_forward search_for lang 0 in "⚐"
  with Not_found -> "-->"

(* FIXME, use FilePos for obtaining citations etc. *)
let show_parse_error file_name content error_summary error_details pos =
  let n = max 0 (min pos (String.length content-1)) in
  let begin_citation  =   get_index_N_lines_before  content n 5  in
  let length_citation =   n -  begin_citation                    in
  let begin_error_zone =  get_index_N_lines_before  content n 0  in
  let length_error_zone = min( (get_index_N_lines_after  content n 5)-begin_error_zone +1) (String.length content -begin_error_zone) in
  let red = Ansi.print  `red
  and green = Ansi.print  `green in
  let line, col, gchar =
    if pos = -1 then
      "??", "??", "??"
    else
      let line_int, col_int = FilePos.get_pos file_name pos in
      string_of_int line_int, string_of_int col_int, string_of_int pos
  in
  (* FIXME: use really format *)
  OManager.printf "%s" (
    (Printf.sprintf "In %s [%s:%s-%s:%s | global chars=%s-%s]\n%s at line %s, column %s\n"
       (red file_name) line col line col gchar gchar
       (red error_summary) (red line) (red col)
    )
    ^ (Printf.sprintf "The error may be in the following citation, usually in the %s part (starting at %s) or just before:" (red"red") (parse_error_flag))
    ^ (Printf.sprintf "\n<<%s%s>>\n"
         (green (String.sub content begin_citation    length_citation  ))
         (red   (parse_error_flag^(String.sub content begin_error_zone  length_error_zone))))
    ^ (Printf.sprintf "Hint: %s\n" error_details)
  ) ;
  OManager.error "Syntax error"
(* ====================================================================================== *)

(* high level parsing *)
let hl_factory parser_rule name ?filename contents =
  try
    ll_factory parser_rule ?filename contents
  with
  | Trx_runtime.SyntaxError (loc, err) ->
      let filename = Option.default name filename in
      show_parse_error filename contents "Syntax error" err loc

let expr = hl_factory Opa_parser.parse_opa_parser_expr_eoi "Expression"
let ty = hl_factory Opa_parser.parse_opa_parser_ty_eoi "Type"

let code ?(cache=false) ?(filename="") content =
  (*print_string content;*)
  FilePos.add_file filename content;
  match if cache then CacheParse.get filename content else None with
  | None ->
      #<If:PARSER_CACHE_DEBUG>OManager.printf "Cache @{<red>miss@} for %s@." filename#<End>;
      let res =
        try
          Chrono.measure (fun () -> parse_file_content ~filename content)
            (fun f ->
               #<If:TESTING>
                 ()
               #<Else>
                 if f > 1.0 && (not BuildInfos.is_release) then
                   Printf.printf
                     "Parsing of %s is too long : %1.3f seconds \n%!"
                     filename f
               #<End>)
        with
        | Trx_runtime.SyntaxError (pos, err) ->
            show_parse_error filename content "Syntax error" err pos
        | Specific_parse_error (pos, err) ->
            show_parse_error filename content "Error" err (FilePos.get_first_char pos) (* FIXME, whole range should be used *)
        | err ->
            (* All parser error in OPA should use the [Specific_parse_error] exception.
               Otherwise we have no location to report... *)
            show_parse_error filename content "Error" (Printexc.to_string err) (-1)
      in
      OManager.flush_errors (); (* make sure that if someone threw errors, then we stop before saving the cache *)
      if cache then CacheParse.set filename content res;
      res
  | Some l ->
      #<If:PARSER_CACHE_DEBUG>OManager.printf "Cache @{<green>hit@} for %s@." filename#<End>;
      l
