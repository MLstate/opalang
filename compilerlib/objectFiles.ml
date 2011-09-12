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
(**
   A package foo is stored into the directory foo.opx
   It may have several object files, each in a separate file

   The files themselves just contain one line with the git hash of
   libqml and opa, you won't try to marshal old datastructures
   (and segfault doing so)
   The rest of the file is just the programmer information, marshalled
*)

#<Debugvar:OBJECT_DEBUG>

(* depends *)
module List = Base.List
module String = Base.String
module Format = Base.Format
module Hashtbl = Base.Hashtbl

(* -- *)

type package_name = string (* foo *)
type filename = string (* path/foo.opx *)
type basename = string (* foo.opx *)
type hash = string
type content = string
type regexp = string

type 'code_elt parsed_code = (filename * content * 'code_elt list) list

(*
  Convention for unit storing.
  All package object files are saved in one directory for each package.
  In this directory, each pass can store a object file nammed like the pass.
  We store also in a special directory all the compiled object files from
  the (pass final compilation), with a object file index which contains the ordered
  list of files contained in this directory.
*)
module Name =
struct
  let plugin_ext = "opp"
  let object_ext = "opx" (* pronounced opix *)
  let bin_directory = "bin"
  let index_bin = "index" (* stored in bin/index *)
end

module Package =
struct
  type t = package_name * FilePos.pos (* FIXME should be a list, to have the positions of successives imports *)
  let name = fst
  let pos = snd
  let compare ((n1,_):t) ((n2,_):t) = compare n1 n2
  let compare_opt = Option.make_compare compare
  let equal ((n1,_):t) ((n2,_):t) = n1 = n2
  let equal_opt o1 o2 =
    match o1, o2 with
    | None, None -> true
    | Some p1, Some p2 -> equal p1 p2
    | _ -> false
  let hash ((n,_):t) = Hashtbl.hash n
  let pp f ((s,_):t) = Format.pp_print_string f s
  let pp_full f ((s,pos) : t) = Format.fprintf f "%s at %a" s FilePos.pp_pos pos
  let pp_option f = function
    | None -> Format.fprintf f "None"
    | Some p -> Format.fprintf f "Some %a" pp p
  let pp_field sep ppa fmt f t =
    pp fmt f;
    Format.fprintf fmt sep;
    ppa fmt t
  let pp_2 ppa ppb f (a,b) =
    Format.fprintf f "(%a, %a)" ppa a ppb b
end
module PackageTbl = Hashtbl.Make(Package)
module PackageAssoc = List.MakeAssoc(Package)
module ListPackageMap = ListMap.Make(Package)

type package = Package.t

(*
  Configuration files. They are used in case of refactoring big projects,
  for expliciting the package organization for files which does not precise
  a package. Loading conf files leads to a side effect on this table.
*)
let conftable : (filename, package) Hashtbl.t = Hashtbl.create 10

(*
  Extra import specified in conf files
*)
let confimport : (package_name, package) ListHashtbl.t = ListHashtbl.create 10

(*
  In a conf file, a line can be a package definition :
  {[
    mypackage:
  ]}
  or a infos
  {[
      import package
      toto/titi.opa
      /tmp/myfile.opa
      $ENVAR/myfile.opa
  ]}
*)
type conf_line =
  | Package of package_name
  | Infos of (filename list * string list) (* filenames, and imported packages *)

let conf_split s = String.slice_chars " \t" s
let process_line ~position line =
  let line =
    match String.findi '#' line with
    | None -> line
    | Some i -> String.sub line 0 i
  in
  let line = String.trim line in
  let length = String.length line in
  if length = 0
  then None
  else
    if line.[pred length] = ':'
    then
      let package_name = String.sub line 0 (pred length) in
      let package_name = String.trim package_name in
      Some (Package package_name)
    else
      let words = conf_split line in
      let filenames, imported =
        let rec aux ((filenames, imported) as acc) = function
          | [] -> acc
          | "import" :: tl -> (
              match tl with
              | [] ->
                  OManager.error (
                    "%a@\n" ^^
                    "@{<bright>import@} should be followed by a package name on the same line."
                  )
                    FilePos.pp_pos position
              | package :: tl ->
                  aux (filenames, package::imported) tl
            )
          | hd :: tl ->
              aux (hd::filenames, imported) tl
        in
        aux ([], []) words
      in
      Some (Infos (filenames, imported))

(*
  Solving the env:
  + resolve path
  + resolve env vars
*)
let solve_env conffile prefix filename =
  let find s =
    try Sys.getenv s with Not_found -> s in
  let filename =
    try
      let b = Buffer.create 124 in
      let _ = Buffer.add_substitute b find filename in
      Buffer.contents b
    with
    | Not_found -> filename in
  let filename =
    if Filename.is_relative filename
    then
      File.simplify_path (Filename.concat prefix filename)
    else
      filename in
  if not (File.is_regular filename) then
    OManager.serror "%s: file %s is not a regular file."
      conffile filename;
  filename

let load_conf conffile =
  let () =
    if not (File.is_regular conffile) then
      OManager.error "I/O error: @{<bright>%S@} -> No such file" conffile
  in
  let () =
    let content =
      match File.content_opt conffile with
      | Some content -> content
      | None ->
          OManager.error "I/O error: cannot read conf file @{<bright>%S@}" conffile
    in
    FilePos.add_file conffile content
  in
  (* the path in a conf file is relative to the location of the conf file *)
  let prefix = Filename.dirname conffile in
  let process_line package line _i offset =
    let position = FilePos.make_pos conffile offset (offset + String.length line) in
    match process_line ~position line with
    | None -> package
    | Some (Package package_name) ->
        if package_name = "" || not ( List.for_all String.is_word (String.slice '.' package_name ) )
        then
          OManager.error (
            "%a@\n" ^^
            "The package name @{<bright>%S@} is invalid"
          )
            FilePos.pp_pos position
            package_name
        else
          Some package_name
    | Some (Infos (filenames, imported)) -> (
        match package with
        | Some package_name ->
            List.iter (
              fun filename ->
                (* solving the env *)
                let filename = solve_env conffile prefix filename in
                let _ =
                  #<If$minlevel 10>
                    Printf.printf "add conf : file:%S --> pack:%S\n%!" filename package_name
                  #<End>
                in
                Hashtbl.add conftable filename (package_name, position) ;
            ) filenames ;
            List.iter (
              fun import ->
                ListHashtbl.add confimport package_name (import, position) ;
            ) imported ;
            package
        | None ->
            let context =
              let filename = String.concat " " filenames in
              let import = String.concat " " imported in
              let files =
                match filenames with
                | [] -> ""
                | _ -> Printf.sprintf "file(s) %s" filename
              in
              let imported =
                match imported with
                | [] -> ""
                | _ -> Printf.sprintf "imported package(s) %s" import
              in
              match files with
              | "" -> imported
              | _ -> Printf.sprintf "%s and %s" files imported
            in
            OManager.error (
              "%a@\n" ^^
              "You should supply a @{<bright>package@} for @{<bright>%s@}@\n"^^
              "@[<2>@{<bright>Hint@}:@\nAdd a package declaration in a line before, using this syntax:@\n" ^^
              "@[<2>mypackage:@\n" ^^
              "%s@]@]@\n"
            )
              FilePos.pp_pos position
              context
              line
      )
  in
  let _ = File.lines_foldi_offset process_line None conffile in
  ()
(*------------------------------*)
(*---- command line options ----*)
(*------------------------------*)

(* a *)

let autobuild : bool option ref = ref (Some true) (* the bool says is we need to link *)

(* c *)

type compilation_mode = [
  | `prelude
  | `init
  | `linking
  | `compilation
]
let compilation_mode_state = ref `prelude
let compilation_mode () = !compilation_mode_state

(* m *)

let more_import_package_names = MutableList.create ()
let more_link_package_names = MutableList.create ()

(* o *)

let opadep = ref false
let opadep_all = ref false

(* r *)
let rebuild = ref false

(* s *)

let default_separated = `full
let separated = ref default_separated
let turn_separated_on () =
  match !separated with
  | `partial | `full -> ()
  | `no -> separated := default_separated

let turn_separated_off () =
  separated := `no

(* v *)
let verbose_build = ref false

(* utility function on command line arguments *)
let is_separated () = !separated <> `no

(*-------------------------*)
(*----- a few utils -------*)
(*-------------------------*)
(* for simplicity, every time the compiler changes, the object files are invalid *)
let this_file_version = BuildInfos.opalang_git_sha
let opxdir = ref "."
let unprefixed_dirname (package:package_name) : filename = package ^ "." ^ Name.object_ext
let dirname (package:package_name) : filename = Filename.concat !opxdir (unprefixed_dirname package)
let unprefixed_dirname_plugin (package:package_name) : filename = package ^ "." ^ Name.plugin_ext
let dirname_plugin (package:package_name) : filename = Filename.concat !opxdir (unprefixed_dirname_plugin package)
let dirname_from_package ((package_name,_):package) = dirname package_name
let undirname filename : package_name = Filename.chop_suffix (Filename.basename filename) ("."^Name.object_ext)
let undirname_plugin filename : package_name = Filename.chop_suffix (Filename.basename filename) ("."^Name.plugin_ext)
let filename_from_dir dir pass = Filename.concat dir pass
let filename_from_package package pass = filename_from_dir (dirname_from_package package) pass

let fst3 (a,_,_) = a
let fst_snd3 (a,b,_) = (a,b)
module MakeMemo(S:Hashtbl.HashedType) : sig
  val memo : (S.t -> 'a) -> (S.t -> 'a) * (S.t -> [ `value of 'a | `exn of exn ] -> unit) * (S.t -> unit)
  val memo_but_exn : (S.t -> 'a) -> (S.t -> 'a) * (S.t -> 'a -> unit) * (S.t -> unit)
end =
struct
  module H = Hashtbl.Make(S)
  let memo f =
    let h = H.create 10 in
    let get =
      fun x ->
        try
          match H.find h x with
          | `exn e -> raise e
          | `value v -> v
        with Not_found ->
          try
            let v = f x in
            H.add h x (`value v);
            v
          with e ->
            H.add h x (`exn e);
            raise e in
    let set k v = H.add h k v in
    let unset k = H.remove h k in
    get, set, unset
  let memo_but_exn f = (* doesn't memoize the result when an exception is thrown *)
    let h = H.create 10 in
    let get =
      fun x ->
        try H.find h x
        with Not_found ->
          let v = f x in
          H.add h x v;
          v in
    let set k v = H.replace h k v in
    let unset k = H.remove h k in
    get, set, unset
end

(* a few utils so that you can choose whether functions exit
 * or throw exception
 * this should somehow be merged with omanager, not sure how, though *)
exception No_exit of string
let raise_exn = ref false
let with_exn f =
  let b = !raise_exn in
  raise_exn := true;
  try let r = f () in
      raise_exn := b;
      r
  with e ->
    raise_exn := b;
    raise e
let error fmt =
  if !raise_exn then Format.ksprintf (fun s -> raise (No_exit s)) fmt
  else OManager.error fmt
let serror fmt =
  if !raise_exn then Format.ksprintf (fun s -> raise (No_exit s)) fmt
  else OManager.serror fmt

let verbose fmt =
  let fmt = "[objects] " ^^ fmt in
  if !verbose_build then
    OManager.printf fmt
  else
    OManager.ifprintf fmt

(*----------------------------------*)
(*- info about the current package -*)
(*----------------------------------*)
let linking_package : package = ("",FilePos.nopos "ObjectFiles.dummy")
let current_package = ref linking_package (* foo, not foo.opx *)
let package_names  = ref ([]:package list) (* foo, bar *)
let package_deep_names = ref ([]:package list) (* for transitive deps *)
let package_deep_names_and_more_deeps_names = ref ([] : package list) (* for transitive deps of the package + of the --packages *)
let package_names_and_more_names = ref ([] : package list) (* for direct deps + --packages *)
let after_end_of_separate_compilation = ref false
let no_init = ref false

let global_compilation () = not (is_separated ()) || !after_end_of_separate_compilation
let get_current_package () = !current_package
let get_current_package_name () = fst !current_package
let end_of_separate_compilation () = after_end_of_separate_compilation := true
let last_pass = "pass_end_of_separate_compilation"

(* characters valid in identifiers in js and ml
 * !! do not put the special_char of string_to_valid_ident in here
 *)
let accepted_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

(* returns a valid ml ident, or maybe "" (when s was "") *)
let get_package_name_as_a_valid_ident ?(will_be_prefixed=false) s =
  let special_char = '_' in
  let s = String.escape ~valid_chars:accepted_char ~escape_char:special_char s in
  if will_be_prefixed then
    s
  else
    if s = "" then "l" else "c" ^ s

let get_package_as_a_valid_ident ?will_be_prefixed (package_name,_) =
  get_package_name_as_a_valid_ident ?will_be_prefixed package_name
let get_current_package_name_as_a_valid_ident ?will_be_prefixed () = get_package_name_as_a_valid_ident ?will_be_prefixed (get_current_package_name ())

let prefix_by_mlstatelibs s =
  let mlstatelibs = Lazy.force InstallDir.getenv in
  Filename.concat mlstatelibs s

let defaultpaths =
  (*
    need to delay the compuation until after options, so
    that InstallDir is not computed if unsed (--help, --version, etc.)
  *)
  lazy (
    !opxdir :: prefix_by_mlstatelibs InstallDir.lib_opa :: []
  )

let defaultpaths_for_stdlib =
  (* need to delay the computation until after options have been parsed
   * or else the --quiet option wouldn't have been set yet *)
  lazy (let dirname = prefix_by_mlstatelibs InstallDir.opa_packages in
        let formula =
          Filename.concat (Printf.sprintf "$%s" InstallDir.name) InstallDir.opa_packages
        in
        try
          if Sys.is_directory dirname then
            [dirname]
          else (
            if is_separated () then
              OManager.unquiet "%s (=%s) is not a directory. It won't be used as a default included directory."
                formula dirname;
            []
          )
        with Sys_error _ ->
          if is_separated () then
            OManager.unquiet "%s (=%s) doesn't exist. It won't be used as a default included directory."
              formula dirname;
          [])
let extrapaths = ref []
let bsl_plugins = ref []

(*----------------------------------*)
(*----- the initial loading --------*)
(*----------------------------------*)
let wclass_load =
  WarningClass.create
    ~public:true
    ~name:"load-opx"
    ~doc:"Warn if you use a package that has several corresponding object files in the -I paths"
    ~err:false
    ~enable:true
    ()
let wclass_import = (* warn for instance on the code [import schtroumpf.*] *)
  WarningClass.create
    ~public:true
    ~name:"load-import"
    ~doc:"Warn if you make in import declaration that doesn't import anything"
    ~err:true
    ~enable:true
    ()
let warning_set =
  WarningClass.Set.create_from_list [
    wclass_load;
    wclass_import;
  ]

(* find the dir where the package 'foo' is located
 * the search paths are the ones given with -I to the compiler,
 * plus the default ones *)
let find_dir_no_memo ((package_name, pos) : package) : filename =
  let r =
    File.get_one_location
      ~dir:true
      ~missing_file:(fun _dirs _fname ->
                       error (
                         "%a@\n  Cannot find any object file for package @{<bright>%s@}.@\n" ^^
                           "@[<2>@{<bright>Hint@}:@\nPerhaps some included directories are missing (option -I)@]"
                       )
                         FilePos.pp_pos pos package_name
                    )
      ~many_files:(fun _dirs _fname l ->
                     let choice = List.hd l in
                     OManager.warning ~wclass:wclass_load
                       "@[<v2>Found several object files for package %s:@ %a@]@\nDefaulting to @{<bright>%s@}"
                       package_name (Format.pp_list "@ " Format.pp_print_string) l choice;
                     choice)
      !extrapaths
      (unprefixed_dirname package_name) in
  #<If$minlevel 5>
    Printf.printf "find_dir: %s -> %s\n%!" package_name r
  #<End>;
  r

let find_dir, set_find_dir, unset_dir =
  let module M = MakeMemo(Package) in
  M.memo_but_exn find_dir_no_memo

let find_js_file_no_memo (js_file:basename) =
  File.get_one_location
    ~missing_file:(fun _dirs _fname ->
                     error (
                       "Cannot find the file @{<bright>%s@}.@\n" ^^
                         "@[<2>@{<bright>Hint@}:@\nPerhaps some included directories are missing (option -I)@]"
                     )
                       js_file
                  )
    ~many_files:(fun _dirs _fname l ->
                   let choice = List.hd l in
                   OManager.warning ~wclass:wclass_load
                     "@[<v2>Found several files with name %s:@ %a@]@\nDefaulting to @{<bright>%s@}"
                     js_file (Format.pp_list "@ " Format.pp_print_string) l choice;
                   choice)
    !extrapaths
    js_file
let find_js_file, _set_find_js_file, _unset_find_js_file =
  let module M = MakeMemo(String) in
  M.memo find_js_file_no_memo

let find_js_file_content_digest, _, _unset_find_js_file_content_digest =
  let module M = MakeMemo(String) in
  M.memo (fun js_file ->
            let path = find_js_file js_file in
            let content = File.content path in
            let digest = Digest.string content in
            path, content, digest
         )

let exists_dir package =
  try
    ignore (with_exn (fun () -> find_dir package));
    true
  with No_exit _s ->
    false

(*
 * syntax: * is a glob shell -> .* for Str
 *         {alt1,alt2,alt3} for alternatives -> \(alt1\|alt2\|alt3\) for Str
 *         anything except '*', '{', '}', ',' is literal and you have no escapes sequences
 * even if the input is malformed, the output is well formed
 * (to avoid receiving exceptions from Str.regexp)
 *)
let matching pos beg end_ sep s index : (int * string list) =
  let len = String.length s in
  let rec aux depth last acc i =
    if i = len then (
      OManager.warning ~wclass:wclass_import "%a@\n  An opening %C has no matching %C."
        FilePos.pp_pos pos
        beg end_;
      let s = String.sub s last (i - last) in
      let acc = s :: acc in
      (i-1,List.rev acc)
    ) else if s.[i] = beg then (
      aux (depth+1) last acc (i+1)
    ) else if s.[i] = end_ then (
      if depth = 0 then
        let s = String.sub s last (i - last) in
        let acc = s :: acc in
        (i,List.rev acc)
      else
        aux (depth-1) last acc (i+1)
    ) else if s.[i] = sep then (
      if depth = 0 then
        let s = String.sub s last (i - last) in
        let acc = s :: acc in
        aux depth (i+1) acc (i+1)
      else
        aux depth last acc (i+1)
    ) else
      aux depth last acc (i+1) in
  aux 0 index [] index

let rec brace_expansion_aux pos s : char list list =
  let len = String.length s in
  let rec aux accs i =
    if i = len then
      accs
    else (
      match s.[i] with
      | '{' ->
          let beg = i + 1 in
          let (end_,choices) = matching pos '{' '}' ',' s beg in
          let regexps : char list list = brace_expansions pos choices in
          let accs = List.rectangle_map (fun acc regexp -> regexp @ acc) accs regexps in
          aux accs (end_+1)
      | '}' | ',' as c ->
          OManager.warning ~wclass:wclass_import "%a@\n  Illegal %C in import declaration."
            FilePos.pp_pos pos c;
          aux accs (i+1)
      | c -> aux (List.map (fun l -> c :: l) accs) (i+1)
    ) in
  aux [[]] 0

and brace_expansions pos (sl : string list) : char list list =
  List.concat_map (brace_expansion_aux pos) sl

(* This function performs brace expansion
 * for instance "abc{1,2,3}def" -> ["abc1def","abc2def","abc3def"]
 *)
let brace_expansion (s,pos) =
  let charss = brace_expansion_aux pos s in
  List.map (fun x -> String.of_chars (List.rev x)) charss

let regexp_of_glob s =
  let len = String.length s in
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to len - 1 do
    match s.[i] with
    | '*' -> Buffer.add_string b ".*"
    | '\\' | '[' | ']' | (*'*' |*) '.' | '?' | '+' | '^' | '$' as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c;
    | c ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let expand_glob ?(mode = (`package : [`package|`plugin])) names (package_name,pos) =
  let package_keyword, import_keyword, unprefixed_dirname =
    match mode with
    | `package ->
        "package", "import", unprefixed_dirname
    | `plugin ->
        "plugin", "import-plugin", unprefixed_dirname_plugin
  in
  let globs = brace_expansion (unprefixed_dirname package_name,pos) in
  let results =
    List.concat_map
      (fun glob -> (* glob has the form [.*\.opx] *)
         let string_regexp = regexp_of_glob glob in
         let l = File.get_locations_regexp ~dir:true !extrapaths string_regexp in
         (* we can possibly have duplicates as path1/a.opx and path2/a.opx
          * since we manipulate only package names, we will transform it to ["a";"a"]
          * later when we call find_dir on "a", we will have a proper warning
          * because it will find the two occurrences of */a.opx *)
         let l = List.map (fun x -> undirname x, pos) l in
         let regexp = Str.regexp (string_regexp^"$") in
         let l = l @ List.filter (fun (s,_pos) -> Str.string_match regexp (unprefixed_dirname s) 0) names in
         if l = [] then (
           let glob_without_ext = undirname glob in
           OManager.warning ~wclass:wclass_import (
             "%a@\n@[<2>  Cannot find any %s that matches the %s %s%s.@]@\n"^^
             "@[<2>Hint:@\nPerhaps you forgot to include some directories (-I option).@]"
           )
             FilePos.pp_pos pos
             package_keyword
             import_keyword
             glob_without_ext
             (if "{" ^ glob_without_ext ^ "}" = package_name then "" else Printf.sprintf " (from %s)" package_name)
         );
         l
      ) globs in
  let results = List.uniq_unsorted ~cmp:Package.compare results in
  if String.contains package_name '*' then
    verbose "Glob expansion of %s: %a@."
      package_name
      (Format.pp_list ";" Package.pp) results;
  results

let get_compilation_directory () =
  if is_separated ()
    && compilation_mode () = `compilation (* no saving in linking mode *)
    && not !after_end_of_separate_compilation
  then Some (dirname_from_package !current_package)
  else None

let get_compilation_directory_or_current_dir () =
  match get_compilation_directory () with
  | None -> Filename.current_dir_name
  | Some s -> s

let compilation_has_started = ref false
let successfull_compilation = ref false
let reset_successfull_compilation () = successfull_compilation := false
let compilation_is_successfull () = successfull_compilation := true

let clean_up_object =
  let pwd = Sys.getcwd () in
  fun () ->
    Sys.chdir pwd;
    match get_compilation_directory () with
    | Some d when not !successfull_compilation && !compilation_has_started ->
        (* before compilation has started, we haven't created any package
         * and it is possible that the package name is not yet defined
         * so we can't suppress files *)
        (* deletes partial packages *)
        if BuildInfos.is_release then
          File.remove_rec d
        else (
          let d_broken = d ^ ".broken" in
          File.remove_rec d_broken;
          Unix.rename d d_broken
        )
    | _ ->
        ()
let () = at_exit clean_up_object
let () = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 2))

let get_deps ?(packages=false) ?(deep=false) () =
  match compilation_mode () with
  | `init ->
      if packages && deep then
        !package_deep_names_and_more_deeps_names
      else
        []
  | `compilation | `linking | `prelude ->
      if !after_end_of_separate_compilation then
        []
      else
        (match packages, deep with
         | true, true -> !package_deep_names_and_more_deeps_names
         | true, false -> !package_names_and_more_names
         | false, true -> !package_deep_names
         | false, false -> !package_names)

let fold_dir_name ?packages ?deep f acc =
  List.fold_left (fun acc package -> f acc (find_dir package) package) acc (get_deps ?packages ?deep ())
let iter_dir_name ?packages ?deep f =
  List.iter (fun package -> f (find_dir package) package) (get_deps ?packages ?deep ())
let fold_dir ?packages ?deep f acc =
  fold_dir_name ?packages ?deep (fun acc dir _package -> f acc dir) acc
let iter_dir ?packages ?deep f =
  iter_dir_name ?packages ?deep (fun dir _package -> f dir)
let fold_name ?packages ?deep f acc =
  fold_dir_name ?packages ?deep (fun acc _dir package -> f acc package) acc
let iter_name ?packages ?deep f =
  iter_dir_name ?packages ?deep (fun _dir package -> f package)



(*--------------------------------------*)
(*--- interacting with object files ----*)
(*--------------------------------------*)
let show_list = ref ([] : (package -> unit) list)
let show_package package =
  List.iter (fun f -> f package) !show_list

module type S =
sig
  type t
  val pass : string
  val pp : Format.formatter -> t -> unit
end

module type R =
sig
  type 'a wrapper
  type t

  val iter_with_name : (?packages:bool -> ?deep:bool -> (package -> t -> unit) -> unit) wrapper
  val fold_with_name : (?packages:bool -> ?deep:bool -> (package -> 'a -> t -> 'a) -> 'a -> 'a) wrapper
  val iter_with_dir : (?packages:bool -> ?deep:bool -> (filename -> t -> unit) -> unit) wrapper
  val fold_with_dir : (?packages:bool -> ?deep:bool -> (filename -> 'a -> t -> 'a) -> 'a -> 'a) wrapper
  val iter : (?packages:bool -> ?deep:bool -> (t -> unit) -> unit) wrapper
  val fold : (?packages:bool -> ?deep:bool -> ('a -> t -> 'a) -> 'a -> 'a) wrapper
  val save : (t -> unit) wrapper
end

module Make(S:S) :
  sig
    include R with type t = S.t and type 'a wrapper = 'a

    (* these fields are meant for internal use of this module *)
    val load1 : package -> t
    val load1_exn : package -> t
    val unset_load1 : package -> unit
  end  =
struct
  type 'a wrapper = 'a
  type t = S.t

  (* return the content of one package *)
  let load1_no_memo ((package_name,pos) as package) =
    let name = filename_from_dir (find_dir package) S.pass in
    try
      let channel = open_in_bin name in
      let l1 = input_line channel in
      #<If$contains "noerror"> () #<Else>
        if l1 <> this_file_version then
          error "The package %s was compiled with a different version of the compiler." package_name
      #<End>;
      let v = (Marshal.from_channel channel : t) in
      close_in channel;
      v
    with
    | e ->
        error "%a@\n@[<2>  An error occurred while trying to read package %s: %s.@]"
          FilePos.pp_pos pos
          package_name
          (Printexc.to_string e)
  module M = MakeMemo(Package)
  let (load1 : package -> t), _set_load1, unset_load1 = M.memo_but_exn load1_no_memo
  let load1_exn x = with_exn (fun () -> load1 x)

  let ref_for_partial_sep = ref None
  let load_values ?packages ?deep () =
    let l = List.map (fun (package_pos) -> (package_pos, load1 package_pos)) (get_deps ?packages ?deep ()) in
    match compilation_mode () with
    | `init ->
        (match !ref_for_partial_sep with
         | None -> l
         | Some v -> l @ [(linking_package,v)])
    | `linking | `compilation | `prelude -> l

  let iter_with_name ?packages ?deep f =
    List.iter (fun (package_name, value) -> f package_name value) (load_values ?packages ?deep ())
  let iter_with_dir ?packages ?deep f =
    assert (compilation_mode () <> `init);
    iter_with_name ?packages ?deep (fun package value -> f (find_dir package) value)
  let iter ?packages ?deep f =
    List.iter (fun (_, value) -> f value) (load_values ?packages ?deep ())
  let fold_with_name ?packages ?deep f acc =
    List.fold_left (fun acc (package_name,value) -> f package_name acc value) acc (load_values ?packages ?deep ())
  let fold_with_dir ?packages ?deep f acc =
    assert (compilation_mode () <> `init);
    fold_with_name ?packages ?deep (fun package acc value -> f (find_dir package) acc value) acc
  let fold ?packages ?deep f acc =
    List.fold_left (fun acc (_,value) -> f acc value) acc (load_values ?packages ?deep ())

  (* save the given content in the current package, or raise SaveError if it failed
   * (because you have no 'write' rights for instance) *)
  let save t =
    match compilation_mode () with
    | `linking | `prelude ->
        assert (!ref_for_partial_sep = None);
        ref_for_partial_sep := Some t
    | `init -> ()
    | `compilation ->
      #<If$minlevel 3>Printf.printf "Saving pass:%s\n%!" S.pass#<End>;
      assert(fst !current_package <> ""); (* asserting that the current package has been initialized *)
      let file = dirname_from_package !current_package in
      if not (File.check_create_path file) then
        OManager.error "An error occurred while trying to create the object file %s." file;
      let filename = filename_from_package !current_package S.pass in
      assert (not (File.exists filename)); (* this would break only if the compiler doesn't clean an existing
                                            * object file before compiling it or if someone tries to
                                            * save several times for the same pass *)
      let channel = open_out_bin filename in
      try
        Printf.fprintf channel "%s\n" this_file_version;
        Marshal.to_channel channel t [] ;
        flush channel ;
        close_out channel ;
        ()
      with e ->
        OManager.error "An error occurred while outputting to the object file %s: %s" file (Printexc.to_string e)

  let show package =
    try
      let v = load1_exn package in
      Format.printf "@[<2>%s:@\n%a@]@\n" S.pass S.pp v
    with No_exit _s ->
      Format.printf "@[<2>%s:@\nAn error occurred@]@\n" S.pass

  let () = show_list := show :: !show_list
end

module MakeClientServer(S:S) : R with type t = S.t and type 'a wrapper = side:[`client | `server] -> 'a
= struct
  type 'a wrapper = side:[`client|`server] -> 'a
  type t = S.t

  module SClient = struct
    type t = S.t
    let pass = S.pass ^ "_client"
    let pp = S.pp
  end

  module SServer = struct
    type t = S.t
    let pass = S.pass ^ "_server"
    let pp = S.pp
  end

  (* specialized interfaces *)
  module Client = Make(SClient)
  module Server = Make(SServer)

  let select = function
    | `client -> (fun client _server -> client)
    | `server -> (fun _client server -> server)

  (* generic interface*)
  let iter_with_name ~side = select side Client.iter_with_name Server.iter_with_name
  let fold_with_name ~side = select side Client.fold_with_name Server.fold_with_name
  let iter_with_dir ~side = select side Client.iter_with_dir Server.iter_with_dir
  let fold_with_dir ~side = select side Client.fold_with_dir Server.fold_with_dir
  let iter ~side = select side Client.iter Server.iter
  let fold ~side = select side Client.fold Server.fold
  let save ~side = select side Client.save Server.save

end





(*----------------------------------------------------*)
(*----- checking the consistency of object files -----*)
(*----------------------------------------------------*)

type consistencycheck_t = {
  sources : hash;
  separated_mode : [ `partial | `full ];
  js_libs : (basename * hash) list;
  dependencies : (package * hash) list;
  bsl_plugins : (basename * hash) list;
  conf_import : package list;
  static_include_deps : float option StringMap.t;
}
module ConsistencyCheckS =
struct
  type t = consistencycheck_t
  let pass = "consistency_check"
  let pp_static_include_deps f map =
    StringMap.iter
      (fun k v ->
         Format.fprintf f "@\n%s: %a"
           k
           (fun f o -> match o with
            | None -> Format.pp_print_string f "-"
            | Some float -> Format.pp_print_float f float) v
      ) map
  let pp f {sources; separated_mode; js_libs; dependencies; bsl_plugins; conf_import; static_include_deps} =
    Format.fprintf f "sources: %d@\nseparated_mode: %s@\n@[<2>js_libs:%a@]@\n@[<2>dependencies:%a@]@\n@[<2>bsl_plugins:%a@]@\n@[<2>conf_import:%a@]@\n@[<2>static_include_deps:%a@]@\n"
      (Hashtbl.hash sources)
      (match separated_mode with `partial -> "partial" | `full -> "full")
      (Format.pp_list "" (fun f (basename,hash) -> Format.fprintf f "@\n%s %d" basename (Hashtbl.hash hash))) js_libs
      (Format.pp_list "" (fun f (package,hash) -> Format.fprintf f "@\n%a %d" Package.pp package (Hashtbl.hash hash))) dependencies
      (Format.pp_list "" (fun f (basename,hash) -> Format.fprintf f "@\n%s %d" basename (Hashtbl.hash hash))) bsl_plugins
      (Format.pp_list "" (fun f package -> Format.fprintf f "@\n%a" Package.pp package)) conf_import
      pp_static_include_deps static_include_deps
end

module ConsistencyCheckR =
struct
  include Make(ConsistencyCheckS)

  let get_hash {sources; _} = sources
  let get_mode {separated_mode; _} = separated_mode
  let get_assoc {dependencies; _} = dependencies
  let get_js_libs {js_libs; _} = js_libs
  let get_deps_hash t = List.map snd (get_assoc t)
  let get_deps t = List.map fst (get_assoc t)
  let get_bsl_plugins {bsl_plugins; _} = bsl_plugins
  let get_conf_import {conf_import; _} = conf_import
  let get_static_include_deps {static_include_deps; _} = static_include_deps

  let load_hash package = get_hash (load1 package)
  let load_mode package = get_mode (load1 package)
  let load_hash_exn package = get_hash (load1_exn package)
  let load_deps_hash package = get_deps_hash (load1 package)
  let load_deps package = get_deps (load1 package)
  let load_assoc package = get_assoc (load1 package)
  let load_js_libs package = get_js_libs (load1 package)
  let load_bsl_plugins package = get_bsl_plugins (load1 package)
  let load_conf_import package = get_conf_import (load1 package)
  let load_static_include_deps package = get_static_include_deps (load1 package)
end

(* checks that a package agrees with other packages
 * it should fail using [error] and [serror] only so that
 * the compiler can recover in autobuild
 *)
let merge_consistency_info ~exn (acc_js_libs, acc_deps, acc_bsl_plugins) package =
  let merge_assoc assoc1 assoc2 =
    (* the sort could be done only on assoc2 in check_deps *)
    let assoc1 = PackageAssoc.sort assoc1 in
    let assoc2 = PackageAssoc.sort assoc2 in
    PackageAssoc.unique_sorted_merge
      ~merge:(fun ((p1,pos1),c1) ((_p2,pos2),c2) ->
                if (c1:string) <> c2 then
                 serror "@[<2>Some files have been compiled with different versions of %s.@\n\
                              Files @\n %a @\nuse a version different of %s than the one used by files@\n %a.@]"
                    p1
                    FilePos.pp_files pos1
                    p1
                    FilePos.pp_files pos2;
                ((p1, FilePos.merge_pos pos1 pos2), c1)
             )
      assoc1
      assoc2 in

  let check_deps () =
    let assoc = ConsistencyCheckR.load_assoc package in
    merge_assoc acc_deps assoc in

  let check_sep_mode () =
    let mode = ConsistencyCheckR.load_mode package in
    if (mode :> [ `full | `partial | `no ]) <> !separated then
      serror "@[<2>The package %a was compiled for %s separation.@]"
        Package.pp package
        (match mode with `partial -> "partial" | `full -> "full") in

  let check_js_libs () =
    (* here we check that when a package declares that it uses a js lib
     * then:
     * - either we can find that file in the -I, and then it must be the
     *   same file
     *   This way, we know if we need to recompile that package or not
     *   in autobuild
     * - or we cannot find that file in the -I, and then if we find another
     *   package that sees an other version of this file, we stop (even in
     *   autobuild) because we can't know which version in the right one
     *   and even if we did, we don't know where the file is, and even if we
     *   did, we would perhaps need to recompile a package that we already
     *   compiled (in this execution of the compiler) !
    *)
    let js_libs = ConsistencyCheckR.load_js_libs package in
    List.fold_left
      (fun acc (js_lib,digest) ->
         (* package knows the version [digest] of [js_lib] *)
         try
           let digest' = List.StringAssoc.find js_lib acc in
           (* [digest'] is the version we currently know of [js_lib] *)
           if digest = digest' then
             (* same version -> ok *)
             acc
           else (
             let _, _, digest'' =
               try
                 (* not the same version *)
                 (* see on disk which version is there: [digest''] *)
                 with_exn (fun () -> find_js_file_content_digest js_lib)
               with
                 No_exit s ->
                   (* the file is not the disk, we cannot recompile anything so
                    * this error cannot be recovered (OManager.error instead of error) *)
                   OManager.error "The package %a disagrees with an other package on %s. (%s)"
                     Package.pp package js_lib s in
             (* the version on disk must be the one we know of js_lib
              * (see the long comment above) *)
             assert (digest' = digest'');
             error "The package %a uses an old version of %s."
               Package.pp package js_lib
           )
         with Not_found ->
           (* we have never seen [js_lib] for the moment *)
           let o =
             try
               Some (find_js_file_content_digest js_lib)
             with No_exit _ ->
               None in
           match o with
           | None ->
               (* there is no version on the disk, so save the current information and keep going *)
               (js_lib, digest) :: acc_js_libs
           | Some (_filename,_content,digest') ->
               (* [digest'] is the version on the disk *)
               if digest = digest' then
                 (* [package] agrees with the version on disk, save the version an go on *)
                 (js_lib, digest) :: acc_js_libs
               else
                 (* [package] disagrees with the version on disk *)
                 error "The package %a uses an old version of %s."
                   Package.pp package js_lib
      ) acc_js_libs js_libs in

  let check_bsl_plugins () =
    (* checking *)
    let fold acc (name, uniq_id) =
      match List.assoc_opt name acc with
      | Some conflict_id ->
          if String.equal conflict_id uniq_id then acc
          else
            #<If$contains "noerror">
              acc
            #<Else>
              error (
                "@[<2>External plugin: conflicting versions for %S@\n"^^
                "package @{<bright>%a@} uses @{<bright>%s@}@\n"^^
                "previous packages used @{<bright>%s@}@]@\n"
              )
                name
                Package.pp package uniq_id
                conflict_id
            #<End>
      | None ->
          (name, uniq_id) :: acc
    in
    List.fold_left fold acc_bsl_plugins (ConsistencyCheckR.load_bsl_plugins package)
  in

  let check_everything () =
    check_sep_mode ();
    let acc_bsl_plugins = check_bsl_plugins () in
    let acc_js_libs = check_js_libs () in
    let acc_deps = check_deps () in
    (acc_js_libs, acc_deps, acc_bsl_plugins) in

  if exn then with_exn check_everything else check_everything ()

let set_extrapaths ~no_stdlib paths =
  (*
    removing the paths dedicated to ocaml
  *)
  let filter s = not (String.length s > 0 && s.[0] = '+') in
  let paths = List.filter filter paths in
  let all_paths =
    if no_stdlib then Lazy.force defaultpaths @ paths
    else Lazy.force defaultpaths @ Lazy.force defaultpaths_for_stdlib @ paths in
  extrapaths := all_paths

let set_bsl_plugins p = bsl_plugins := p

type 'code_elt block = filename * content * 'code_elt list
type 'code_elt blocks = 'code_elt block list
type 'code_elt block_dep = 'code_elt block * (*deps*) package list

let extract_package_from_code extract_package_decl lcode =
  List.fold_left_filter_map
    (fun ((names,deps) as acc) code_elt ->
       match extract_package_decl code_elt with
       | Some (`declaration,name,pos) ->
           ((name,pos) :: names,deps), None
       | Some (`import,name,pos) ->
           (names,(name,pos)::deps), None
       | None ->
           acc,Some code_elt
    ) ([],[]) lcode

(* for non separated mode only *)
let extract_package_from_codes extract_package_decl filename_content_lcodes =
  List.map
    (fun (filename,content,lcode) ->
       let _, lcode = extract_package_from_code extract_package_decl lcode in
       (filename,content,lcode))
    filename_content_lcodes

(* for separated mode only *)
(*
  returns
  + an option of the package which the filename belong to.
  + the filtered code, without packages and import directive.
  + the list of imported packages by the filename.
*)
let browse_code_and_build_map_aux extract_package_decl filename lcode =
  let (names, deps), lcode = extract_package_from_code extract_package_decl lcode in
  let conftable_infos = Hashtbl.find_opt conftable filename in
  let deps =
    match conftable_infos with
    | None -> deps
    | Some (package_name, _) ->
        let import = ListHashtbl.find_list confimport package_name in
        import @ deps
  in
  let package_name =
    match names, conftable_infos with
    | [], o -> o
    | [h], None -> Some h
    | [h], Some p ->
        if not (Package.equal h p) then
          OManager.serror "@[<2>A file and a conf file disagree on the package that %s belongs to:@\n%a@\nand@\n%a@]"
            filename
            Package.pp_full h
            Package.pp_full p;
        Some h
    | h :: _, o ->
        let names =
          match o with
          | None -> names
          | Some p -> p :: names in
        let pos_list = List.map snd names in
        let pos = FilePos.merge_pos_list pos_list in
        OManager.serror "%a@\n  Multiple package declarations in file %s."
          FilePos.pp_pos pos filename;
        Some h in
  package_name, lcode, deps

(* for separated mode only *)
let browse_code_and_build_map : bool -> _ -> (filename * content * _ list) list -> ('code_elt block_dep list ListPackageMap.t * 'code_elt block_dep list * bool) =
  fun no_stdlib extract_package_decl filename_content_lcodes ->
    let map,anon =
      List.fold_left
        (fun (map,anon) (filename,content,lcode) ->
           match browse_code_and_build_map_aux extract_package_decl filename lcode with
           | None, code, deps -> (map, ((filename,content,code),deps) :: anon)
           | Some name, code, deps -> (ListPackageMap.append name ((filename,content,code),deps) map, anon))
        (ListPackageMap.empty, [])
        filename_content_lcodes in
    let names = ListPackageMap.keys map in
    let expand_glob, _, _ =
      let module M = MakeMemo(Package) in
      M.memo_but_exn (expand_glob names) in

    let add_more_packages l =
      let more_stdlib =
        if not no_stdlib && is_separated () then
          let more = expand_glob ("stdlib.core{,.*}", FilePos.nopos "Builtin import") in
          more
        else
          []
      in
      let more_import_packages =
        let more = List.concat_map
          expand_glob
          (MutableList.to_list more_import_package_names) in
        more
      in
      more_stdlib
      @ more_import_packages
      @ l
    in
    let map = ListPackageMap.map (List.map (fun (v,deps) -> (v, add_more_packages (List.concat_map expand_glob deps)))) map in
    MutableList.update (fun l -> List.concat_map expand_glob l) more_link_package_names;
    let no_anonymous_packages = anon = [] in
    let anon =
      (("fake-linking-file","fake-content",[]), add_more_packages []) ::
        List.map (fun (v,deps) -> (v, List.concat_map expand_glob deps)) anon in
    map, anon, no_anonymous_packages

let inject x = Some x
let project = Option.get
let injects l = List.map inject l
let projects l = List.map project l

(* some utils to print the dependency graph of the standard library *)
module G = GraphUtils.String.G
module Viz = GraphUtils.DefaultGraphviz(G)(struct let vertex_name x = x end)

let reorder :
  extrajs:string list ->
  extract_more_deps:('code_elt list -> float option StringMap.t) ->
  'code_elt block_dep list ListPackageMap.t ->
  'code_elt block_dep list option ->
    (package -> bool) *
    (package list -> package list) *
    (package -> package -> int) *
    (package * 'code_elt block list * package list) list *
    ('code_elt block list * package list) option =
  fun ~extrajs ~extract_more_deps map anon ->
  let more_deps_of_package =
    let more_deps_of_package_no_memo package =
      let code_of_package package = List.concat_map (fun (_,_,code_elt) -> code_elt) (List.map fst (ListPackageMap.find package map)) in
      extract_more_deps (code_of_package package) in
    let module M = MakeMemo(Package) in
    let get, _set, _reset = M.memo_but_exn more_deps_of_package_no_memo in
    get in
  let deps_of_block_deps block_deps = List.concat_map snd block_deps in
  let deps_in_map package = deps_of_block_deps (ListPackageMap.find package map) in
  let module M =
      struct
        type t = package option
        let index = function
          | None -> "linking"
          | Some (s,_) -> "file"^s
        let index_of_package x = index (inject x)
        let deps_on_disk = ConsistencyCheckR.load_deps
        let deps_of package =
          if ListPackageMap.mem package map then
            deps_in_map package
          else
            deps_on_disk package
        let depends_t : t -> t list =
          let anon = Option.default [] anon in
          function
          | None -> injects (deps_of_block_deps anon @ List.concat_map deps_of (MutableList.to_list more_link_package_names))
          | Some package -> injects (deps_of package)
        let depends t = List.map index (depends_t t)
      end in
  let module S = TopologicSort.Make(M) in

  (* packages that are defined by the files on the command line only *)
  let packages = injects (ListPackageMap.keys map) @ [None] in
  (* This None should be the last element of ALL the lists *)
  (* all the packages, even those on the disk *)
  let all_shallow_packages =
    List.uniq_unsorted ~cmp:Package.compare_opt
      (injects (
         MutableList.to_list more_link_package_names @
         deps_of_block_deps (List.flatten (ListPackageMap.elts map)) @
         deps_of_block_deps (Option.default [] anon)
       ) @
       packages) in
  (* that could possibly be computed in a cleaner way *)
  (* shallow dependencies on already computed objects *)
  let shallow_disk_packages =
    List.filter (fun x -> not (List.exists (Package.equal_opt x) packages)) all_shallow_packages in
  (* transitive dependencies on already computed objects *)
  let disk_packages =
    let projected_shallow_disk_packages = projects shallow_disk_packages in
    injects (List.uniq_unsorted ~cmp:Package.compare (projected_shallow_disk_packages @ List.concat_map ConsistencyCheckR.load_deps projected_shallow_disk_packages)) in
  (* full transitive dependencies *)
  let all_packages =
    List.uniq_unsorted
      ~cmp:Package.compare_opt
      ~conflict:(fun o1 o2 ->
                   match o1, o2 with
                   | None, _
                   | _, None -> assert false
                   | Some (name,pos1), Some (_,pos2) ->
                       OManager.error
                         "@[<2>The command line contains files defining the package %s,@ but an already compiled package depends on it:@\n\
                               %a@\nand@\n%a@]@\n\
                          @[<2>Hint:@\nPerhaps you have a package name collision.@]"
                         name
                         FilePos.pp_pos pos1
                         FilePos.pp_pos pos2
                )
      (disk_packages @ packages) in

  let () =
    if !opadep && !autobuild <> None then (
      let escaped s = Printf.sprintf "%S" s in
      let graph = G.create () in
      let iter ((p1, _) as package) =
        G.add_vertex graph (escaped p1) ;
        List.iter (fun (p2, _) ->
                     G.add_vertex graph (escaped p2) ;
                     G.add_edge graph (escaped p2) (escaped p1))
          (M.deps_of package)
      in
      List.iter (Option.iter iter) all_packages ;
      (* kepp in synchro with passtracker and opadep *)
      let filename = "opadep__index.dot" in
      OManager.unquiet "opadep: outputting @{<bright>%s@}" filename ;
      Viz.to_file filename graph ;
      if not !opadep_all then exit 0
    )
  in

  #<If>
    Format.printf "packages: [%a]@." (Format.pp_list ";" Package.pp_option) packages;
    Format.printf "all_shallow_packages: [%a]@." (Format.pp_list ";" Package.pp_option) all_shallow_packages;
    Format.printf "shallow_disk_packages: [%a]@." (Format.pp_list ";" Package.pp_option) shallow_disk_packages;
    Format.printf "disk_packages: [%a]@." (Format.pp_list ";" Package.pp_option) disk_packages;
    Format.printf "all_packages: [%a]@." (Format.pp_list ";" Package.pp_option) all_packages;
  #<End>;

  let cache =
    try S.compute all_packages
    with S.CyclicDep t ->
      match t with
      | None -> assert false (* you shouldn't be able to have cycles with the linking part *)
      | Some (name,pos) ->
          OManager.error "%a@\n  Cyclic dependency on the package %s." FilePos.pp_pos pos name in
  let transitive_closure_one package =
    if ListPackageMap.mem package map then
      projects (S.transitive_dependencies cache (inject package))
    else
      ConsistencyCheckR.load_deps package in
  let transitive_closure packages =
    let r = List.uniq_unsorted ~cmp:Package.compare (packages @ List.concat_map transitive_closure_one packages) in
    #<If$minlevel 3>
      Format.printf "transitive_closure: [%a] -> [%a]@."
        (Format.pp_list ";" Package.pp) packages (Format.pp_list ";" Package.pp) r
    #<End>;
    r in
  let sorted_packages, remaining = S.get_order cache in
  if remaining <> [] then (
    Format.printf "@[all_packages:[%a]@\npackages:%a@\nsorted:[%a]@\nremaining: [%a]@]@."
      (Format.pp_list ";" Package.pp_option) all_packages
      (Format.pp_list ";" Package.pp) (ListPackageMap.keys map)
      (Format.pp_list ";" Package.pp_option) sorted_packages
      (Format.pp_list ";" (Package.pp_2 Format.pp_print_string (Format.pp_list "," Package.pp_option))) remaining;
    assert false
  );
  let all_but_last, last = List.extract_last sorted_packages in
  if last <> None then (
    (* i don't think it can be triggered by a user *)
    (* because that would mean that someone depends on the linking files,
     * but you can't say that you import these files because these files
     * are the ones that are anonymous
     * In linking mode, it is the same thing: since no package will be created
     * with the names declared in the linked sources, we can ignore the package
     * declarations of all linked sources
     *)
    Printf.printf "last: %s\n%!" (DebugPrint.print (Option.get last));
    assert false
  );
  let all_but_last = List.map Option.get all_but_last in

  let bsl_plugins = !bsl_plugins in

  let (js_libs, assoc, bsl_plugins) =
    let extrajs = List.map (fun jsfile -> let (_,_,digest) = find_js_file_content_digest jsfile in jsfile, digest) extrajs in
    let initial_acc = (extrajs, [], bsl_plugins) in
    List.fold_left
      (fun acc o ->
         let package = Option.get o in
         merge_consistency_info ~exn:false acc package) initial_acc shallow_disk_packages in
  #<If> Format.printf "@[assoc: [%a]@]@." (Format.pp_list ";" Package.pp) (List.map fst assoc) #<End>;

  let check_consistency_of_conf package =
    (* we should probably check something about [conftable], but i'm not sure about
     * what could go wrong if we don't recompile when it changes *)
    let import_from_conf = List.sort Package.compare (try ListHashtbl.find_list confimport (fst package) with Not_found -> []) in
    let import_from_disk = List.sort Package.compare (ConsistencyCheckR.load_conf_import package) in
    List.make_compare Package.compare import_from_conf import_from_disk = 0 in
    (*if List.make_compare Package.compare import_from_conf import_from_disk <> 0 then
      serror "the list of packages imported by %a has changed" Package.pp package in*)

  let check_date_of_static_inclusions package =
    let dates_from_disk = ConsistencyCheckR.load_static_include_deps package in
    let dates_from_sources = more_deps_of_package package in
    StringMap.equal ((=):float option -> _ -> _) dates_from_disk dates_from_sources in

  (* FIXME: this function is called many times, even though it doesn't do anything
   * expensive, it shouldn't called that often
   * in particular, we should be able to filter the packages for which the check has
   * been done already *)
  let check_consistency_with_package_on_disk assoc =
    (* this function checks that [assoc] (representing the current assumption about the packages)
     * is consistent with the packages that have already been compiled, ie the hash we think
     * is really the hash they have *)
    List.iter
      (fun package_opt ->
         let package = Option.get package_opt in
         try
           let hashed_content = PackageAssoc.find package assoc in
           let hashed_content' = ConsistencyCheckR.load_hash package in
           if hashed_content' <> hashed_content then
             serror "%s and one of the dependency of the current package disagree on %s."
               (Package.name package)
               (Package.name package);
           (* NOT SURE if we need to check that or if it is pointless: check_consistency_of_conf package *)
         with Not_found ->
           (* if you end up here, then [package] is a shallow dependency of your source files
            * that is not a deep dependency of any package on disk
            * in this case, there is no check we can do right now *)
           ()
      ) disk_packages in

  check_consistency_with_package_on_disk assoc;

  (* should be iterated upon in topological order *)
  let need_recompilation =
    let acc = ref (js_libs, assoc, bsl_plugins, []) in
    fun package digest ->
    let need_recomp =
      if !autobuild <> None then (
        if not !rebuild then (
          let (js_libs, assoc, bsl_plugins, valid_packages(*the command line packages that don't need recompilation*)) = !acc in
          assert (ListPackageMap.mem package map); (*package is from command line*)
          let command_line_deps =
            List.filter (fun x -> ListPackageMap.mem x map) (transitive_closure_one package) in
          if List.subset_eq ~eq:Package.equal command_line_deps valid_packages (*(dependency package) is included in valid_packages*) then (
            if exists_dir package (*package exists on disk*) then (
              try
                if ConsistencyCheckR.load_hash_exn package = digest (*package on disk has the same hash _as the files*) then (
                  try
                    if File.is_regular (filename_from_package package last_pass)
                    || File.is_directory (filename_from_package package "_build") then (
                      if check_consistency_of_conf package then
                        if check_date_of_static_inclusions package then (
                        (* merge_assoc succeeds -> consistent *)
                        let js_libs, assoc, bsl_plugins = merge_consistency_info ~exn:true (js_libs, assoc, bsl_plugins) package in
                        assert (not (PackageAssoc.mem package assoc));
                        let assoc = (package,digest) :: assoc in (* the current package cannot be already in
                                                                  * the list since we are going in topological
                                                                  * order and cycles have been detected already *)
                        with_exn (fun () -> check_consistency_with_package_on_disk assoc);
                        acc := (js_libs, assoc, bsl_plugins, package :: valid_packages);
                        None
                      ) else
                          Some `static_include_changed
                      else
                        Some `conf_changed
                    ) else
                      Some `not_complete
                  with No_exit s ->
                    Some (`inconsistent s)
                ) else
                  Some `different_sources
              with
                No_exit _s ->
                  Some `different_compiler
            ) else
              Some `no_opx
          ) else
            Some `newer_dep
        ) else
          Some `rebuild
      ) else
        Some `no_autobuild in
    match need_recomp with
    | None ->
        verbose "No recompilation for %a@." Package.pp package;
        false
    | Some reason ->
        let pp_reason f = function
          | `static_include_changed -> Format.pp_print_string f "a file that is included statically has changed"
          | `conf_changed -> Format.pp_print_string f "the import of the package has been modified by a configuration file"
          | `not_complete -> Format.pp_print_string f "the package exists and is up-to-date but is not complete"
          | `inconsistent s -> Format.fprintf f "the package exists, but is inconsistent (%s)" s
          | `different_sources -> Format.pp_print_string f "the package exists, but was compiled from different sources"
          | `different_compiler -> Format.pp_print_string f "the package exists, but was compiled with another compiler"
          | `no_opx -> Format.pp_print_string f "no previous object file"
          | `newer_dep -> Format.pp_print_string f "it depends on a package that needs recompilation"
          | `rebuild -> Format.pp_print_string f "--rebuild option is set"
          | `no_autobuild -> Format.pp_print_string f "not in autobuild mode" in
        verbose "Recompilation for %a: %a.@." Package.pp package pp_reason reason;
        let dir = dirname_from_package package in
        (* we have wrong information memoized
         * about this package, so discard it *)
        set_find_dir package dir;
        ConsistencyCheckR.unset_load1 package;
        (try (* if the package already exists, removing it so that we can be sure that
              * we shouldn't have any package.opx/last_pass from a previous compilation
              * and so the file last_pass exists only when the compilation went ok
              * (didn't stopped with an error, a Ctrl-c, etc.) *)
           File.remove_rec dir
         with
           Unix.Unix_error _ -> ());
        true in

  let save_if_need_recompilation package =
    let block_deps : 'code_elt block_dep list = ListPackageMap.find package map in
    let filename_content_codes = List.map fst block_deps in
    let filename_content_codes =
      (* sorting the files by their names so that changing
       * the order on the command line doesn't trigger a
       * recompilation *)
      List.sort (fun (filename1,_,_) (filename2,_,_) -> String.compare filename1 filename2) filename_content_codes in
    let my_digest = String.concat_map "\n" (fun (_filename,content,_) -> Digest.string content) filename_content_codes in
    if need_recompilation package my_digest then (
      let deep_dependencies = transitive_closure_one package in
      (* here we are loading from the disk all the hashes, even those of
       * the packages that are given on the command line only
       * (the hashes may not exists yet, or not be valid anymore)
       * this is ok only if we compile all the dependencies of a package
       * before saving the consistency check of the current package *)
      let my_deeps_deps = List.map (fun package -> (package, ConsistencyCheckR.load_hash package)) deep_dependencies in
      (* the current package is set at the time that this function is called *)
      assert (Package.equal !current_package package);
      let restricted_separated_mode =
        match !separated with
        | `no -> assert false
        | `partial | `full as v -> v in
      let conf_import = ListHashtbl.find_list confimport (fst package) in
      let static_include_deps = more_deps_of_package package in
      ConsistencyCheckR.save {
        sources = my_digest;
        separated_mode = restricted_separated_mode;
        dependencies = my_deeps_deps;
        js_libs;
        bsl_plugins;
        conf_import;
        static_include_deps;
      };
      true
    ) else
      false
  in

  let mk_name_blocks_deps block_deps =
    let blocks,depss = List.split block_deps in
    let deps = List.flatten depss in
    (* we need to keep the direct dependencies *)
    (blocks, deps) in
  let list =
    List.filter_map
      (fun package_name ->
         try
           let block_deps = ListPackageMap.find package_name map in
           let blocks,deps = mk_name_blocks_deps block_deps in
           Some (package_name,blocks,deps)
         with
         | Not_found -> (* for packages on the disk *)
             None
      ) all_but_last in
  let block_deps = Option.default [] anon in
  let blocks,deps = mk_name_blocks_deps block_deps in
  let anon =
    match anon with
    | None -> None
    | Some _ -> Some (blocks,deps) in

  let compare =
    (* packages are compared by their index in the sorted packages list *)
    let h = PackageTbl.create 10 in
    List.iteri (fun package_opt i ->
                  match package_opt with
                  | None -> ()
                  | Some package -> PackageTbl.add h package i) sorted_packages;
    fun package1 package2 ->
      compare (PackageTbl.find h package1) (PackageTbl.find h package2) in

  save_if_need_recompilation, transitive_closure, compare, list, anon

let setup compare transitive_closure ?package_name direct_deps =
  #<If> Printf.printf "setup: %s\n%!" (Option.default_map "linking" fst package_name) #<End>;
  (match compilation_mode (), package_name with
   | (`linking|`init|`prelude), None -> ()
   | `compilation, Some package_name -> current_package := package_name
   | _ -> assert false);
  let unique l = List.uniq ~cmp:compare (List.stable_sort compare l) in
  (* not sure if the stability is needed *)
  package_names := unique direct_deps;
  package_deep_names := unique (transitive_closure direct_deps);
  package_deep_names_and_more_deeps_names :=
    (match compilation_mode () with
     | `compilation -> !package_deep_names
     | `linking | `init | `prelude -> unique (transitive_closure (direct_deps @ (MutableList.to_list more_link_package_names))));
  package_names_and_more_names :=
    (match compilation_mode () with
     | `compilation -> !package_names
     | `linking | `init | `prelude -> unique (direct_deps @ (MutableList.to_list more_link_package_names)));
  #<If>
    Format.printf "package_names: %a@." (Format.pp_list ";" Package.pp) !package_names;
    Format.printf "package_deep_names: %a@." (Format.pp_list ";" Package.pp) !package_deep_names;
    Format.printf "package_deep_names_and_more_deeps_names: %a@." (Format.pp_list ";" Package.pp) !package_deep_names_and_more_deeps_names;
    Format.printf "package_names_and_more_names: %a@." (Format.pp_list ";" Package.pp) !package_names_and_more_names
  #<End>

let load ?(extrajs=[]) ~no_stdlib extract_package_decl extract_more_deps filename_content_lcodes (k : 'code_elt block list -> unit) =
  if not (is_separated ()) then (
    (* when not in separated mode, no check is done on packages declaration or imports *)
    let filename_content_lcodes = extract_package_from_codes extract_package_decl filename_content_lcodes in
    MutableList.clear more_import_package_names ;
    MutableList.clear more_link_package_names ;
    autobuild := None;
    compilation_mode_state := `linking;
    k filename_content_lcodes;
    compilation_mode_state := `init;
    k []
  ) else (
    let ((map, anon, no_anonymous_packages) : ('code_elt block_dep list ListPackageMap.t * 'code_elt block_dep list * bool)) =
      browse_code_and_build_map no_stdlib extract_package_decl filename_content_lcodes in
    let map, anon =
      match !autobuild with
      | Some link ->
        if not link && not no_anonymous_packages then
          OManager.error "The files %s do not belong to any package, don't know what to do with them."
            (String.concat_map ", " (fun ((filename,_,_),_) -> filename) anon);
        compilation_mode_state := `compilation; (* needed by the save function, or else you can't save anything
                                                 * (for consistencyCheck) *)
        (* we also link everything that we compile *)
        MutableList.append more_link_package_names (ListPackageMap.keys map);
        map, (if link then Some anon else None)
      | None ->
        match compilation_mode () with
        | `compilation ->
            (* check that we have exactly one package declared *)
            (* too many packages is an error *)
            (* too little packages too *)
            if ListPackageMap.size map = 1 then
              let (package_name,block_deps) = ListPackageMap.min map in
              let block_deps = block_deps @ anon in (* the anonymous files are merged with the named ones *)
              let map = ListPackageMap.add package_name block_deps ListPackageMap.empty in
              map, None
            else
              if ListPackageMap.size map = 0 then
                OManager.error "The name of current package is never declared."
              else
                let packages = ListPackageMap.keys map in
                OManager.error "@[<2>The current package is declared several times:@\n%a@]"
                  (Format.pp_list "@\n" Package.pp_full) packages
        | `linking | `init | `prelude ->
            (* don't care about the packages names, just make one big blob
             * perhaps we should check that the files declare at most one package?
             * perhaps we should check that the packages declared in this files do
             * not collide with some of their dependencies
             * compilation works fine without these checks, but perhaps it is error prone
             * to allow it (or perhaps it would be annoying to have them) *)
            let map, anon = ListPackageMap.empty, List.flatten (ListPackageMap.elts map) @ anon in
            map, Some anon in
    #<If$minlevel 2> Printf.printf "map: %s\n%!" (DebugPrint.print map) #<End>;
    let save_if_need_recompilation, transitive_closure, compare, list, anon = reorder ~extrajs ~extract_more_deps map anon in
    #<If$minlevel 2> Printf.printf "list: %s\n%!" (DebugPrint.print list) #<End>;

    OManager.flush_errors (); (* not compiling (nor saving) anything if there was an error before *)

    (* compiling the packages in the order of dependencies *)
    compilation_has_started := true;
    compilation_mode_state := `compilation;
    List.iter
      (fun (package_name,blocks,dep_list) ->
         setup compare transitive_closure ~package_name dep_list;
         if save_if_need_recompilation package_name then (
           reset_successfull_compilation ();
           verbose "Files at compiling: [%a]@."
             (Format.pp_list ";" Format.pp_print_string) (List.map (fun (filename,_,_) -> filename) blocks);
           k blocks
         ) else (
           compilation_is_successfull ()
         )
      ) list;

    (* linking (only in autobuild or --linking) *)
    (match anon with
     | None -> ()
     | Some (blocks,dep_list) ->
         current_package := linking_package; (* need to reset the current package or else
                                              * the current package will be the last package compiled
                                              * (in autobuild) *)
         verbose "Packages at linking: [%a]@."
           (Format.pp_list ";" Package.pp) (MutableList.to_list more_link_package_names);
         verbose "Files at linking: [%a]@."
           (Format.pp_list ";" Format.pp_print_string) (List.map (fun (filename,_,_) -> filename) blocks);
         verbose "Linking@.";
         compilation_mode_state := `linking;
         setup compare transitive_closure dep_list;
         k blocks;
         verbose "Init@.";
         compilation_mode_state := `init;
         k [])
  )


(* the packages that are allowed to contain definitions
 * that the compiler can call
 * ie the global map will only contain identifiers from these packages *)
let stdlib_package_names name =
  (String.is_prefix "stdlib.core." name)
  || ("stdlib.core" = name)
  || (not (is_separated ()))

let stdlib_packages (package_name,_pos) = stdlib_package_names package_name

module Arg =
struct
  module Arg = Base.Arg

  let add_import_package p = MutableList.add more_import_package_names (p, FilePos.nopos "command line")
  let add_import_packages p = List.iter add_import_package (Arg.split p)

  let add_link_package p = MutableList.add more_link_package_names (p, FilePos.nopos "command line")
  let add_link_packages p = List.iter add_link_package (Arg.split p)

  let public_options = [

    (* a *)

    "--autocompile",
    Arg.Unit (fun () ->
                autobuild := Some false ;
                turn_separated_on ();
                ()),
    " Recompile only what is needed, but do not link"
    ;

    (* c *)

    "-c",
    Arg.Unit (fun () ->
                autobuild := None ;
                compilation_mode_state := `compilation ;
                turn_separated_on ();
                ()),
    " Compile the current package, do not link"
    ;

    "--conf",
    Arg.String load_conf,
    " Load a package conf file"
    ;

    (* i *)

    "--import-package",
    Arg.String (fun p ->
                  add_import_packages p ;
                  turn_separated_on ();
                  ()),
    " Add packages(s) to import"
    ;

    (* l *)

    "-l",
    Arg.Unit (fun () ->
                autobuild := None ;
                compilation_mode_state := `linking ;
                turn_separated_on ();
             ),
    " Compile the remaining files and link them with the loaded packages (enforce separated mode)"
    ;

    "--link-package",
    Arg.String (fun p ->
                  add_link_packages p ;
                  compilation_mode_state := `linking ;
                  turn_separated_on ();
                  ()),
    " Add package(s) to link with (enforce separated and linking modes)"
    ;

    (* o *)

    "--odep",
    Arg.Unit (fun () ->
                turn_separated_on ();
                opadep := true ;
             ),
    " Generate the packages dependencies graph and exit"
    ;

    "--odep-all",
    Arg.Unit (fun () ->
                turn_separated_on ();
                opadep := true ;
                opadep_all := true ;
             ),
    " Generate all dependency graphs and continue"
    ;

    "--opx-dir",
    Arg.Set_string opxdir,
    " Defines the directory where opx are saved and loaded (default is .)"
    ;

    (* p *)

    (* r *)
    "--rebuild",
    Arg.Set rebuild,
    " Forces the compiler to rebuild all the sources in autobuild mode"
    ;

    (* s *)


    (* v *)

    "--verbose-build",
    Arg.Set verbose_build,
    " Explains what the build system is doing";

  ]

  let private_options = [

    (* a *)

    "--autobuild",
    Arg.Unit (fun () ->
                autobuild := Some true ;
                turn_separated_on ();
                ()),
    " Recompile only what is needed, and link to build the application"
    ;

    (* d *)

    "--dump-opx",
    Arg.String (fun package_name -> set_extrapaths ~no_stdlib:false [];
                  show_package (package_name,FilePos.nopos "command line");
                  exit 0),
    " Dumps the content of an opx to standard output"
    ;

    (* s *)

    "--separated",
    Arg.spec_of_opt_assoc separated default_separated ["full", `full; "partial", `partial; "off", `no],
    " Separated compilation mode of the compiler (WIP, transitional period only)"
    ;

  ]

  let no_packages () =
    MutableList.is_empty more_link_package_names
  let is_autobuild () = !autobuild <> None
  let is_separated = is_separated
  let is_fully_separated () = !separated = `full
  let is_opadep () = !opadep
end
