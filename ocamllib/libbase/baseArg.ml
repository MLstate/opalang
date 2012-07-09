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

type simple_completion =
  | Nothing
  | File of string
  | Dir
  | Oneof of string list
type completion = {params : simple_completion list; stop : bool}

type spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref
  | String of (string -> unit)
  | Set_string of string ref
  | Int of (int -> unit)
  | Set_int of int ref
  | Float of (float -> unit)
  | Set_float of float ref
  | Tuple of spec list
  | Symbol of string list * (string -> unit)
  | Rest of (string -> unit)
  | Complete of spec * completion

let spec_fun_of_assoc f assoc =
  Symbol (List.map fst assoc, (fun s -> f (List.assoc s assoc)))
let spec_of_assoc ref_ assoc =
  Symbol (List.map fst assoc, (fun s -> ref_ := List.assoc s assoc))
let spec_opt_of_assoc ref_ assoc =
  Symbol (List.map fst assoc, (fun s -> ref_ := Some (List.assoc s assoc)))
let spec_of_opt_assoc ref_ default assoc =
  Unit (fun () ->
          if !Arg.current+1 < Array.length Sys.argv then
            let s = Sys.argv.(!Arg.current+1) in
            if s <> "" && s.[0] = '-' then
              ref_ := default
            else (
              try
                ref_ := List.assoc s assoc;
                incr Arg.current;
              with Not_found ->
                ref_ := default
            )
          else
            ref_ := default
       )

let rec convert_spec_to_old_arg = function
  | Unit x -> Arg.Unit x
  | Bool x -> Arg.Bool x
  | Set x -> Arg.Set x
  | Clear x -> Arg.Clear x
  | String x -> Arg.String x
  | Set_string x -> Arg.Set_string x
  | Int x -> Arg.Int x
  | Set_int x -> Arg.Set_int x
  | Float x -> Arg.Float x
  | Set_float x -> Arg.Set_float x
  | Tuple x -> Arg.Tuple (List.map convert_spec_to_old_arg x)
  | Symbol (x,y) -> Arg.Symbol (x,y)
  | Rest x -> Arg.Rest x
  | Complete (x,_) -> convert_spec_to_old_arg x
let rec convert_spec_from_old_arg = function
  | Arg.Unit x -> Unit x
  | Arg.Bool x -> Bool x
  | Arg.Set x -> Set x
  | Arg.Clear x -> Clear x
  | Arg.String x -> String x
  | Arg.Set_string x -> Set_string x
  | Arg.Int x -> Int x
  | Arg.Set_int x -> Set_int x
  | Arg.Float x -> Float x
  | Arg.Set_float x -> Set_float x
  | Arg.Tuple x -> Tuple (List.map convert_spec_from_old_arg x)
  | Arg.Symbol (x,y) -> Symbol (x,y)
  | Arg.Rest x -> Rest x
let convert_from_old_arg_one (x,spec,y) = (x,convert_spec_from_old_arg spec,y)
let convert_to_old_arg_one (x,spec,y) = (x,convert_spec_to_old_arg spec,y)
let convert_to_old_arg l = List.map convert_to_old_arg_one l
let convert_from_old_arg l = List.map convert_from_old_arg_one l

(* stdlib's functions *)
let parse p = Arg.parse (convert_to_old_arg p)
let parse_argv ?current x p = Arg.parse_argv ?current x (convert_to_old_arg p)
let usage p = Arg.usage (convert_to_old_arg p)
let align p = convert_from_old_arg (Arg.align (convert_to_old_arg p))
let current = Arg.current
type key = Arg.key
type doc = Arg.doc
type usage_msg = Arg.usage_msg
type anon_fun = Arg.anon_fun
exception Help = Arg.Help
exception Bad = Arg.Bad

(* -- generate a simple manpage -- *)

let date_manpage () =
  let dt = Unix.gmtime (Unix.time ()) in
  (Date.fullmonth.(dt.Unix.tm_mon))
  ^ " " ^ (string_of_int (dt.Unix.tm_mday))
  ^ ", " ^ (string_of_int (dt.Unix.tm_year+1900))


(* todo: move to baseList *)
let pretty_list_to_string empty left separator right = function
  | [] -> empty
  | x::q -> (List.fold_left (fun s y -> s ^ separator ^ y) (left ^ x) q) ^ right

let split_option_args str =
  let reg = Str.regexp "[>)\"}]   " in
  try
    let pos = (Str.search_forward reg str 0) + 1
    in
    (String.ltrim (Str.string_before str pos)), (String.ltrim (Str.string_after str pos))
  with
    Not_found -> "", (String.ltrim str)

let print_spec file (key, spec, doc) =
  let key = String.replace key "-" "\\-" in
  let options, doc = split_option_args doc in
  match spec with
  | Symbol (l, _) -> Printf.fprintf file ".TP\n%s %s %s\n%s\n" key (pretty_list_to_string "<none>" "{" "|" "}" l) options doc
  | _ -> Printf.fprintf file ".TP\n%s %s\n%s\n" key options doc; ()

let add_help speclist =
  let add help =
    if List.exists (fun (x, _, _) -> x = help) speclist then []
    else [help, Unit (fun x->x), " Display this list of options"]
   in
  speclist @ (add "-help") @ (add "--help")

let write_simple_manpage
    ~cmdname ~section
    ?(centerfooter=(date_manpage ()))
    ?(leftfooter="") ?(centerheader="")
    ?(summary="") ?(synopsis="") ?(description="") ?options ?(other=[])
    file =
  Printf.fprintf file ".TH \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\n" cmdname (string_of_int section) centerfooter leftfooter centerheader;
  if summary <> "" then
    Printf.fprintf file ".SH NAME\n%s \\- %s\n" cmdname summary
  else
    Printf.fprintf file ".SH NAME\n%s\n" cmdname;
  if synopsis <> "" then Printf.fprintf file ".SH SYNOPSIS\n%s\n" synopsis;
  if description <> "" then Printf.fprintf file ".SH DESCRIPTION\n%s\n" description;
  begin match options with None -> () | Some(speclist) -> begin
    Printf.fprintf file ".SH OPTIONS\n";
    List.iter (print_spec file) (add_help speclist);
  end end;
  List.iter (fun (title, content) -> Printf.fprintf file ".SH %s\n%s\n" title content) other;
  ()
  
(* --- *)


let sort_by_name l = List.stable_sort (fun (x,_,_) (y,_,_) -> compare (x:string) y) l
let sort l = (* also makes names unique *)
  let rec aux acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | ((s1,_,_) as v1) :: ((s2,_,_) :: tl2 as tl) ->
        if s1 = s2 then (
          if (try (Sys.getenv "MLSTATE_TESTING") = "0" with Not_found -> true) then
            Printf.eprintf "Internal warning: The option %s is matched several times in the command line parser.\n%!" s1;
          (* cannot call omanager from here, nor debugVariables *)
          aux acc (v1 :: tl2) (* keeping the first one in the list *)
        ) else
          aux (v1 :: acc) tl in
  aux [] (sort_by_name l)

let merge ({params = l1; stop = b1} as left) {params = l2; stop = b2} =
  if b1 then left else {params = l1 @ l2; stop = b1 || b2}

let rec convert_spec = function
  | Bool _ -> {params = [Oneof ["true";"false"]]; stop = false} (* possibly case insensitive ? *)
  | Unit _
  | Set _
  | Clear _ -> {params = []; stop = false} (* nothing follows *)
  | String _
  | Set_string _
  | Int _
  | Set_int _
  | Float _
  | Set_float _ -> {params = [Nothing]; stop = false} (* no completion possible *)
  | Tuple specs ->
      let l = List.map convert_spec specs in
      List.fold_left merge {params = []; stop = false} l
  | Symbol (sl,_) -> {params = [Oneof sl]; stop = false}
  | Rest _ -> {params = []; stop = true}
  | Complete (_,comp) -> comp
let convert_one (key,spec,_) =
  (key, convert_spec spec)
let convert parse =
  let completion = List.map convert_one parse in
  List.fold_left (fun completion builtin_option ->
                    if List.mem_assoc builtin_option completion then
                      completion
                    else
                      (builtin_option,{params = []; stop = false}) :: completion) completion ["-help";"--help"]

let stringify_simple_completion = function
  | Nothing -> "COMPREPLY=()"
  | File pattern -> Printf.sprintf "_filedir %s" pattern
  | Dir -> "_filedir -d"
  | Oneof sl -> Printf.sprintf "COMPREPLY=($(compgen -W '%s' -- ${cur}))" (String.concat " " sl)
let stringify_completion (k,l) =
  if l = [] then
    ""
  else
    let main,_ =
      List.fold_left (fun (acc,i) sc ->
                        let acc =
                          if i = 0 then acc ^ Printf.sprintf "
            if [ \"$n\" -eq %d ]; then
                %s" (i+1) (stringify_simple_completion sc)
                          else acc ^ Printf.sprintf "
            elif [ \"$n\" -eq %d ]; then
                %s" (i+1) (stringify_simple_completion sc) in
                        acc,(i+1)
                       ) ("",0) l in
    Printf.sprintf "
        %s)%s
            else
                did_something=0
            fi;;" k main

(* making sure we have only letters in the name, to avoid function containing wierd chars
 * (like '/') *)
let remove_illegal_chars s =
  "prefix" ^ Str.global_replace (Str.regexp "[^a-zA-Z]") "" s

let stringify ?name ?(names=[]) ?(default=File "*") l =
  let one_name,names =
    match name,names with
    | None, [] -> Sys.argv.(0), [Filename.basename Sys.argv.(0)]
    | None, (h :: _ as l) -> h, l
    | Some v, l -> v, v :: l in
  let one_name = remove_illegal_chars one_name in
  let stops = List.map fst (List.filter (fun (_,{params=_; stop = b}) -> b) l) in
  let prologue = Printf.sprintf "\
# this file was generated by Base.Arg.add_bash_completion
# do not modify by hand

shopt -s extglob

_%s() {
    COMPREPLY=()
    local cur=`_get_cword`
" one_name in
  let check_stop = Printf.sprintf "
    local latest_minus=$COMP_CWORD
    local stop=(%s)

    for (( i = 0; i < $COMP_CWORD; i ++ )); do
      for j in \"${stop[@]}\"; do
        [ \"${COMP_WORDS[i]}\" = \"$j\" ] && return 0
      done
      [[ \"${COMP_WORDS[i]}\" =~ -.* ]] && latest_minus=$i
    done

    local n=$(( COMP_CWORD - latest_minus ))
"  (String.concat " " stops) in
  let complete = Printf.sprintf "
    local did_something=0
    if [ \"$n\" -gt 0 ]; then
       did_something=1
        case ${COMP_WORDS[latest_minus]} in%s
        *) did_something=0;;
        esac
    fi
" (String.concat "" (List.map (fun (k,{params=l;stop=_}) -> stringify_completion (k,l)) l)) in
  let default_completion = Printf.sprintf "
    if [ $did_something -eq 0 ]; then
        case ${cur} in
        -*) COMPREPLY=($(compgen -W '%s' -- ${cur}));;
        *) %s;;
        esac
    fi
" (String.concat " " (List.map fst l))
    (stringify_simple_completion default) in
  let epilogue = Printf.sprintf "\
}

complete -F _%s -o filenames %s
" one_name (String.concat " " names) in
  prologue ^ check_stop ^ complete ^ default_completion ^ epilogue

let generate ?name ?names ?default args =
  let completion = convert args in
  let s = stringify ?name ?names ?default completion in
  let c = open_out "bash_completion" in
  output_string c s;
  close_out c

let rec add_bash_completion ?name ?names ?default args =
  let rec new_args =
    ("--bash-completion",
     Unit (fun () -> generate ?name ?names ?default new_args; exit 0),
     " Dumps a bash completion in ./bash_completion") :: args in
  new_args

let split s = String.slice_chars " ,;" s
