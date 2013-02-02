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
(* CF MLI *)

module Format = BaseFormat
module List = BaseList

(* alias type *)

type filename = string
type content = string
type absolute_char_offset = int
type line_number = int
type column_number = int


(* cache mechanisme *)

(* memoize filename,content,offsetmap (first line char -> line)
   globally to retrieve easily information from file on errors *)

let parsed_files : (string, (string * int IntMap.t)) Hashtbl.t = Hashtbl.create 16

(* an other cache for having the reverse map *)
let reverse_parsed_files : (string, int array) Hashtbl.t = Hashtbl.create 16

let debug () =
  let iter file (_content, map) =
    Printf.printf "File %S\n" file;
    IntMap.iter (fun offset line -> Printf.printf "line %d -- global offset %d\n%!" line offset) map
  in
  Hashtbl.iter iter parsed_files

let get_file_content file =
  fst (Hashtbl.find parsed_files file)

let uncache file =
  Hashtbl.remove parsed_files file;
  Hashtbl.remove reverse_parsed_files file

let clear () =
  Hashtbl.clear parsed_files;
  Hashtbl.clear reverse_parsed_files

(* compute maps of previous structure *)
let rec compute_lines content pos line map =
  let len =  String.length content in
  if pos < len then
    if
      content.[pos] = '\n' ||
      content.[pos] = '\r' && ( ( (pos<len-1) && content.[pos+1]<>'\n' ) ||
                                  ( (pos>1    ) && content.[pos-1]<>'\n' ) )
    then
      compute_lines content (pos+1) (line+1) (IntMap.add (pos+1) (line+1) map)
    else compute_lines content (pos+1) line map
  else map

(* add a file to the memoization *)
let add_file file content =
  let pos_line_map = compute_lines content 0 1 (IntMap.add 0 1 IntMap.empty) in
  Hashtbl.replace parsed_files file (content, pos_line_map)

(* get the line and column number from a char position for a given file *)
let get_line_char file i =
  let i = max 0 i in
  let get_line_char map i =
    let i_line, line_number = try IntMap.find_inf i map with  Not_found -> (1,1) in
    let char_in_line = i - i_line in
    (line_number, char_in_line)
  in
  let _, map = Hashtbl.find parsed_files file in
  get_line_char map i

let get_pos filename offset =
  let getLine = get_line_char filename in
  getLine offset

let get_pos_string (line, column) =
  Printf.sprintf "line %d, column %d" line column

let get_line filename offset =
  let offset = max 0 offset in
  let line, col = get_pos filename offset in
  (* Printf.fprintf stdout "get_line %d -> %d (line %d)\n%!" offset (offset - col) line; *)
  offset-col, line

let get_next_line file i =
  let default = (1, 1) in
  try (
    let _, map = Hashtbl.find parsed_files file in
    try
      IntMap.find_sup i map
    with Not_found -> (
      try IntMap.find_inf i map
      with Not_found -> default
    )
  ) with Not_found -> default

let cache_reverse_map file array =
  Hashtbl.add reverse_parsed_files file array

let build_reverse_map map =
  let size = IntMap.size map in
  let t = Array.make size 0 in
  IntMap.iter (fun i j -> t.(pred j) <- i) map;
  t

let line_position file line =
  let lines =
    try
      Hashtbl.find reverse_parsed_files file
    with
    | Not_found ->
        (* this can also raise Not_found *)
        let _, map = Hashtbl.find parsed_files file in
        let array = build_reverse_map map in
        cache_reverse_map file array;
        array
  in
  let len = Array.length lines in
  if (1 <= line) && (line <= len)
  then
    let start = lines.(line-1) in
    if line < len
    then start, lines.(line)
    else start, start
  else raise Not_found

(* position tracking *)

type range = {
  start : absolute_char_offset ;
  stop : absolute_char_offset ;
}

type filerange = {
  filename : filename ;
  ranges : range HdList.t
}

type private_cache = {
  mutable one_loc : (filename * line_number) option ;
}

type pos =
  | Builtin of string
  | Files of filerange HdList.t * private_cache

let make_cache () = {
  one_loc = None ;
}

let nopos pass = Builtin pass
let get_file = function
  | Builtin pass -> Printf.sprintf "builtin_%s" pass
  | Files (hd, _) -> (HdList.hd hd).filename

let get_one_loc = function
  | Builtin pass -> Printf.sprintf "builtin_%s" pass, 0
  | Files (hd, cache) -> (
      match cache.one_loc with
      | Some loc -> loc
      | None ->
          let hd = HdList.hd hd in
          let filename = hd.filename in
          let start = (HdList.hd hd.ranges).start in
          let _, line = get_line filename start in
          let loc = filename, line in
          cache.one_loc <- Some loc ;
          loc
    )

let get_first_char = function
  | Builtin _ -> 0
  | Files ((f, _), _) -> (fst f.ranges).start

let make_pos filename start stop =
  if stop < start then invalid_arg "FilePos.make_pos" else
    let range = { start = start ; stop = stop } in
    Files (
      (HdList.singleton { filename = filename ; ranges = HdList.singleton range } ),
      make_cache ()
    )
let cmp (a, _) (b, _) = compare a b
let sort_pos = List.sort cmp

let make_pos_from_line file line =
  try
    let start, stop = line_position file line in
    let pos = make_pos file start stop in
    let () =
      match pos with
      | Files (_, cache) -> cache.one_loc <- Some (file, line)
      | _ -> ()
    in
    pos
  with
  | Not_found ->
      nopos (Printf.sprintf "File %S, line %d:" file line)

let merge_range {start = x1; stop = y1} {start = x2; stop = y2} =
  {start = min x1 x2; stop = max y1 y2}

let merge_pos_for_parser p1 p2 =
  match p1, p2 with
  | Files (({filename=filename1;ranges=(range1,[])},[]), _),
    Files (({filename=filename2;ranges=(range2,[])},[]), _) ->
      assert (filename1 = filename2);
      Files (
        ({filename=filename1; ranges = (merge_range range1 range2, [])}, []),
        make_cache()
      )
  | _ -> assert false

(* very bad complexity, but in practice the list are very little (less than 20 files) *)
module LH = ListHashtbl
let merge_pos p1 p2 =
  match p1, p2 with
  | Builtin _, a | a, Builtin _ -> a
  | Files (r, _), Files (r', _) -> (
      let lh = LH.create 10 in
      (* collect by filenames *)
      let iter f =
        let filename = f.filename in
        HdList.iter (fun r -> LH.add lh filename (r.start, r.stop)) f.ranges in
      HdList.iter iter r ;
      HdList.iter iter r';
      (* insertion of a segment in a segment list sorted by start *)
      let rec merge acc ((start, stop) as seg) =
        match acc with
        | [] -> [ seg ]
        | ((start', stop') as hd)::tl ->
            if start > stop' + 1 then hd::(merge tl seg)
            else
              if stop < start' - 1 then seg::acc
              else
                (min start start', max stop stop')::tl
      in
      let collect filename segs acc =
        let segs = List.fold_left merge [] segs in
        let ranges = List.fold_left (fun acc (start, stop) -> { start = start ; stop = stop } :: acc ) [] segs in
        { filename = filename ; ranges = HdList.wrap (List.rev ranges) } :: acc
      in
      let ranges = List.rev (LH.fold_list collect lh []) in
      Files ((HdList.wrap ranges), make_cache())
    )

let merge_pos_list = function
  | [] -> invalid_arg "FilePos.merge_pos_list"
  | h :: t -> List.fold_left merge_pos h t

let is_empty = function
  | Builtin _ -> true
  | _ -> false

let to_string_range filename r =
  let start = r.start in
  let stop = r.stop in
  try
    let line1, col1 = get_pos filename start in
    let line2, col2 = get_pos filename stop in
    (* Do not change the layout there, or update in the opa-mode the variable compilation-error-regexp-alist *)
    Printf.sprintf "File %S, line %d, characters %d-%d, (%d:%d-%d:%d | %d-%d)" filename line1 col1 (col1 + stop - start) line1 col1 line2 col2
      (#<If:TESTING> 0 #<Else> start #<End>)
      (#<If:TESTING> 0 #<Else> stop #<End>)
  with
  | Not_found ->
      Printf.sprintf "File %S (%d-%d)" filename start stop

let pp_filerange filename fmt r =
  Format.pp_print_string fmt (to_string_range filename r)

let pp_filerange fmt {filename=filename; ranges=ranges} =
  let ranges = HdList.unwrap ranges in
  Format.pp_list "@\n" (pp_filerange filename) fmt ranges

let pp_pos fmt = function
  | Builtin pass -> Format.fprintf fmt "<no position available (%s)>" pass
  | Files (files, _) ->
      let files = HdList.unwrap files in
      Format.pp_list "@\n" pp_filerange fmt files

let pp_files fmt = function
  | Builtin pass -> Format.fprintf fmt "<no file available (%s)>" pass
  | Files (files, _) ->
      let files = HdList.unwrap files in
      Format.pp_list ", " (fun fmt v -> Format.pp_print_string fmt v.filename) fmt files

let to_string_filerange f = Format.to_string pp_filerange f
let to_string p = Format.to_string pp_pos p

(* deprecated API *)
let to_old_pos_many = function
  | Builtin _ -> StringMap.empty
  | Files (fileranges, _) ->
      let translate {start = d; stop = f} = (d, f) in
      let fold map f =
        let file = f.filename in
        let r, ranges = f.ranges in
        let wrong = List.fold_left merge_range r ranges in
        StringMap.add file (translate wrong) map
      in
      HdList.fold_left fold StringMap.empty fileranges

let to_old_pos nopos = function
  | Builtin _ -> nopos ()
  | Files ((f, _), _) ->
      let r = fst f.ranges in
      f.filename, r.start, r.stop

(* citations *)

(*
  The effeciency is not a goal, because we are just printing once a citation in case
  of error. So, do not hack the code for unused optimisation, keep it rather simple.
*)

type options = {
  truncate_lines : int option ;
  lines_before : int ;
  lines_after : int ;
  lines_between : int ;
  color : Ansi.color option ;
  max_length_citation : int option ;
}

let default_options = {
  truncate_lines = Some 80 ;
  lines_before = 5 ;
  lines_after = 5 ;
  lines_between = 5 ;
  color = Some ( `red : Ansi.color ) ;
  max_length_citation = Some 200 ;
}

(* Shame : no Format.pp_print_string_sub ? *)
let pp_print_string_sub fmt content offset len = Format.pp_print_string fmt (String.sub content offset len)

let no_citation fmt pass =
  Format.fprintf fmt "%s (no-citation-available)@\n" pass

(* ALL THE FOLLOWING FUNCTIONS MAY RAISE NOT_FOUND, BUT THEY ARE ALL USED ONLY
   INTERNALLY BY THE FUNCTION CITATION, WHICH CATCH THE EXCEPTION.
*)

(* this function return the global offset of the first char of the previous line of the line which contains the char offset *)
(*

called with the offset of m :

p
       m

it returns the offset of p
*)
let predline_offset filename offset =
  let offset, _ = get_line filename offset in
  let offset, _ = get_line filename (pred offset) in
  offset

(* symetric *)
let succline_offset filename offset =
  let offset, _ = get_next_line filename (succ offset) in
  offset

(* compose predline or succ line, return the sorted offset of successive lines *)
let compose n line_offset filename offset =
  let rec aux acc offset i = if i >= n then acc else
    let offset = line_offset filename offset in
    aux (offset::acc) offset (succ i)
  in
  let row = aux [] offset 0 in
  List.uniq row

let predlines n = compose n predline_offset
let succlines n filename offset = List.rev (compose n succline_offset filename offset)

(* extract a line with a maximal allowed length, and print it into the formatter *)
(* The offsets list is a list of first char of line *)
let extract_lines options fmt filename offsets =
  let content = get_file_content filename in
  let iter_trunc trunc offset =
    let succline = succline_offset filename offset in
    let all = succline - offset in
    let line =
      match trunc with
      | Some trunc ->
          if all > trunc then (String.sub content offset trunc)^"[...]\n"
          else String.sub content offset all
      | None ->
          String.sub content offset all
    in
    Format.pp_print_string fmt line
  in
  try List.iter (iter_trunc options.truncate_lines) offsets
  with
  | Invalid_argument _ -> ()

(* print the right part of the string, including the index *)
let extract_right _options fmt filename offset =
  let noff = fst (get_next_line filename offset) in
  let noff = if noff = offset then fst (get_next_line filename (succ offset)) else noff in
  let content = get_file_content filename in
  Format.pp_print_string fmt (String.sub content offset (noff - offset))

(* print the left part of the line, excluding the index *)
let extract_left _options fmt filename offset =
  let _, col = get_pos filename offset in
  let content = get_file_content filename in
  Format.pp_print_string fmt (String.sub content (offset-col) col)

(* usefull to print between two points (the stop of a range, and the start of the next) *)
let extract_between options fmt filename pointA pointB =
  let lineA, _ = get_pos filename pointA in
  let lineB, _ = get_pos filename pointB in
  let content = get_file_content filename in
  if lineB - lineA > options.lines_between
  then (
    let n = options.lines_between / 2 in
    let succA = succlines n filename pointA in
    let predB = predlines n filename pointB in
    extract_right options fmt filename pointA ;
    extract_lines options fmt filename succA ;
    Format.pp_print_string fmt "[...]\n";
    extract_lines options fmt filename predB ;
    extract_left options fmt filename pointB
  )
  else
    pp_print_string_sub fmt content pointA (pointB-pointA)

(* First version : Print the citation with some color *)
let unsafe_citation_files options fmt filerange =
  let open_color, close_color =
    match options.color with
    | Some color -> Ansi.open_color_code color, Ansi.close_color_code
    | None -> "<<<", ">>>"
  in
  let filename = filerange.filename in

  Format.pp_print_string fmt (to_string_filerange filerange);
  Format.pp_print_char fmt '\n';

  let hdranges = filerange.ranges in
  let content = get_file_content filename in
  let first = HdList.hd hdranges in
  let last = HdList.last hdranges in

  let predlines = predlines options.lines_before filename first.start in
  let succlines = succlines options.lines_after filename last.stop in

  let offset_first, _ = get_line filename first.start in
  let predlines = List.filter (fun l -> l <> offset_first) predlines in
  let offset_last, _ = get_line filename last.stop in
  let succlines = List.filter (fun l -> l <> offset_last) succlines in

  Format.pp_print_string fmt "------citation-------\n" ;
  Format.pp_print_string fmt "---------------------\n" ;

  (* lines before *)
  extract_lines options fmt filename predlines;

  (* first part *)
  extract_left options fmt filename first.start ;

  (* Midle part *)
  (* common part *)
  let common r =
    let start = r.start in
    let stop = r.stop in
    Format.pp_print_string fmt open_color;
    pp_print_string_sub fmt content start (stop-start);
    Format.pp_print_string fmt close_color;
  in

  let rec iter ranges =
    match ranges with
    | hd::((hd2::_) as tl) ->
        common hd ;
        extract_between options fmt filename hd.stop hd2.start ;
        iter tl
    | [last] ->
        common last ;
        (* in the last case case, we should extract the right part *)
        extract_right options fmt filename last.stop
    | [] -> assert false
  in
  iter (HdList.unwrap hdranges) ;

  (* lines after *)
  extract_lines options fmt filename succlines ;
  Format.pp_print_char fmt '\n' ;

  Format.pp_print_string fmt "---------------------\n" ;
  Format.pp_print_string fmt "---end-of-citation---\n" ;

  ()

let citation_files options fmt filerange =
  try unsafe_citation_files options fmt filerange
  with Not_found -> no_citation fmt "<source file not available anymore>"

let citation ?(options=default_options) fmt pos =
  match pos with
  | Builtin pass -> no_citation fmt pass
  | Files (files, _) ->
      HdList.iter (citation_files options fmt) files

(* FIXME, consider re-implementing this function; it's
    supposed to serve a single request so we don't have to
    check line breaks after [pos]. *)
let get_pos_no_cache content pos =
  let id = ref 0 in
  let tmp_filename = Printf.sprintf "tmp-@%d@" !id in
  incr id;
  add_file tmp_filename content;
  let res = get_pos tmp_filename pos in
  uncache tmp_filename;
  res

let pp_citation fmt pos = citation fmt pos
let pp = pp_pos
