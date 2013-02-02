(*
    Copyright © 2011, 2012 MLstate

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
module Arg = BaseArg
module List = BaseList
module String = BaseString

(* alias *)

(* - *)

(*
  To extend the type, follow EXTEND-OPT
  <!> the type should remain marshalable, because it is embedded in plugins.
*)
type optimized_conf = {
  cleanup : bool ;
  localrenaming : bool ;
  (*
    EXTEND-OPT
  *)
}

(*
  <!> Beware, this type is marshaled during the bsl preprocessing
*)
type conf =
  | Verbatim
  | Optimized of optimized_conf

let default_opt = {
  cleanup = false ;
  localrenaming = false ;
  (*
    EXTEND-OPT
  *)
}

let default =
  Optimized {cleanup = true; localrenaming = true}

let get_opt = function
  | Verbatim -> default_opt
  | Optimized opt -> opt

let compare_conf c1 c2 =
  match c1, c2 with
  | Verbatim, Verbatim -> 0
  | Verbatim, _ -> -1
  | _, Verbatim -> 1
  | Optimized {cleanup=c1; localrenaming=l1}, Optimized {cleanup=c2; localrenaming=l2} ->
      match compare c1 c2 with
      | 0 -> compare l1 l2
      | c -> c
      (*
        && EXTEND-OPT
      *)

let is_default opt =
  compare_conf default opt = 0

module Private :
sig

  (**
     The type parsed for an analysis, corresponding to a property with an argument
     Example:
     {[
     cleanup: false
     ]}
     will be parsed as: ["cleanup", "false"]
  *)
  type entry = string * string

  (**
     Folding a conf file.
     Conf are always folded from the default conf.
     May use [OManager] with the given pos in case of error.
  *)
  val fold : FilePos.pos -> entry -> conf -> conf

end
  =
struct
 type entry = string * string

 (*
    list of handled properties
 *)

 let cleanup = "cleanup"
 let localrenaming = "localrenaming"
 let verbatim = "verbatim"

 (*
   EXTEND-OPT
 *)

 let properties = [
   cleanup ;
   localrenaming ;
   verbatim ;
   (*
     EXTEND-OPT
   *)
 ]

 (*
   list of handled arguments, for each propertie
 *)

 let v_true = "true"
 let v_false = "false"

 let bool_arguments = [
   v_true ;
   v_false ;
 ]

 let invalid_argument pos property arguments typo =
   OManager.error (
     "%a@\n"^^
     "Property @{<bright>%s: invalid argument @{<bright>%s@}@\n"^^
     "%a"
   )
     FilePos.pp pos
     property
     typo
     (HintUtils.pp_suggestion arguments) typo

 let get_bool_argument pos property argument =
   if argument = v_true
   then
     true
   else if argument = v_false
   then
     false
   else
     invalid_argument pos property bool_arguments argument

 let fold pos (property, argument) conf =
   Return.set_checkpoint (
     fun label ->

       if property = cleanup
       then (
         let argument = get_bool_argument pos property argument in
         let opt = get_opt conf in
         let opt =
           if opt.localrenaming = argument then opt
           else {
             opt with
               cleanup = argument ;
           } in
         let conf = Optimized opt in
         Return.return label conf
       );

       if property = localrenaming
       then (
         let argument = get_bool_argument pos property argument in
         let opt = get_opt conf in
         let opt =
           if opt.localrenaming = argument then opt
           else {
             opt with
               localrenaming = argument ;
           } in
         let conf = Optimized opt in
         Return.return label conf
       );

       if property = verbatim
       then (
         let argument = get_bool_argument pos property argument in
         let conf =
           if argument
           then
             Verbatim
           else
             (*
               keep the previous setting, if one exists
             *)
             Optimized (get_opt conf)
         in
         Return.return label conf
       );

       (*
         EXTEND-OPT
       *)

       OManager.error (
         "%a@\n"^^
         "Unknown property @{<bright>%s@}@\n"^^
         "%a"
       )
         FilePos.pp pos
         property
         (HintUtils.pp_suggestion properties) property
   )
end

type filename = string

type confs = conf StringMap.t

type t = {
  confs : confs ;
  files : string list ;
  files_set : StringSet.t ;
  (*
    cache: same as files, but in a set format
  *)
}

let export t = t.confs

let empty files =
  let files_set = StringSet.from_list files in
  let confs =
    let fold file confs =
      StringMap.add file default confs
    in
    StringSet.fold fold files_set StringMap.empty
  in
  {
    files ;
    files_set ;
    confs ;
  }

(*
  Parsing the file
  A line can be [%s] or property:argument
*)
type parsed_line =
  | Files of StringSet.t
  | Entry of Private.entry

let regexp_file = Str.regexp "\\[\\([^]]*\\)\\]$"
let regexp_property = Str.regexp "\\([^:]*\\):\\([^:]*\\)$"

let parse_line pos files line =
  let line =
    match String.findi '#' line with
    | None -> line
    | Some i -> String.sub line 0 i
  in
  let line = String.trim line in
  if line = ""
  then None
  else Some (
    Return.set_checkpoint (
      fun label ->
        if Str.string_match regexp_file line 0
        then (
          let args = Str.matched_group 1 line in
          let regexps = Arg.split args in
          let fold set str =
            let regexp =
              try
                Str.regexp str
              with
              | Invalid_argument message
              | Failure message
                ->
                  OManager.error (
                    "%a@\n"^^
                    "@[<2>Invalid regexpression:@\n%s@]@\n"
                  )
                    FilePos.pp pos
                    message
            in
            let filter s = Str.string_match regexp (Filename.basename s) 0 in
            let files = List.rev_filter filter files in
            if files = [] then
              OManager.warning ~wclass:WarningClass.bsl_register (
                "%a@\n"^^
                "Regexp <@{<bright>%s@}> does not match any considered js files@\n"
              )
                FilePos.pp pos
                str
            ;
            let add a b = StringSet.add b a in
            List.fold_left add set files
          in
          let files = StringSet.empty in
          let files = List.fold_left fold files regexps in
          let parsed = Files files in
          Return.return label parsed
        );

        if Str.string_match regexp_property line 0
        then (
          let property = Str.matched_group 1 line in
          let property = String.trim property in
          let argument = Str.matched_group 2 line in
          let argument = String.trim argument in
          let parsed = Entry (property, argument) in
          Return.return label parsed
        );

        OManager.error (
          "%a@\n"^^
          "Syntax error.@\n"^^
          "Use one of the following form :@\n"^^
          "@[<2>@\n[file.js]@\n[*]@\n[file1.js, file2.js]@\nproperty : argument@]@\n"
        )
          FilePos.pp pos
    )
  )

(*
  Internal accumulator
*)

let fold conffile t =
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
  let files = t.files in
  let process_line ((set, confs) as acc) line line_number =
    let pos = FilePos.make_pos_from_line conffile line_number in
    match parse_line pos files line with
    | None -> acc
    | Some parsed -> (
        match parsed with
        | Files set -> (set, confs)
        | Entry entry ->
            let fold file confs =
              let conf =
                try
                  StringMap.find file confs
                with
                | Not_found ->
                    (*
                      Internal error.
                      We filter only files already known in the function parse_line,
                      and the initializer of type [t] builds the field [files]
                    *)
                    assert false
              in
              let conf = Private.fold pos entry conf in
              StringMap.add file conf confs
            in
            let confs = StringSet.fold fold set confs in
            set, confs
      )
  in
  let acc = t.files_set, t.confs in
  let _, confs = File.lines_foldi process_line acc conffile in
  { t with confs }
