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

(** This program is meant to quickly build manpages either from existing sections in proper files, or by a quick-and-dirty parsing of the result of --help *)

let cmdname =
  try
    Sys.argv.(1)
  with
    _ -> (Printf.eprintf ("Usage: %s <cmdname> [sectionbasename]\n") Sys.argv.(0); exit 1)

let sectionbasename =
  try
    Sys.argv.(2)
  with
    _ -> cmdname

let read_section name = File.content_opt (sectionbasename ^ "." ^ name)

let help_summary, help_synopsis, help_description, help_options =
  match read_section "help" with
    None -> "","","",""
  | Some help ->
    let reg0 = (* one line summary ? *)
      Str.regexp ("^.*"^ (Str.quote cmdname) ^"[ \t]*:[ \t]*\(.*\)$")
    in
    let is_blank x =
      BaseString.contains " \n\t" x
    in  
    let summary, pos0 =
      try
	if Str.string_partial_match reg0 help 0
	then
	  BaseString.ltrim ~is_space:is_blank (Str.matched_group 1 help), (Str.match_end () + 1) (* +1 is meant to skip \n *)
	else
	  "", 0
      with
	Not_found -> "", 0
    in
    let reg1 = (* one line synopsis ? *)
      Str.regexp ("^[ \t]*[Uu]sage[ \t]*:.*\("^ (Str.quote cmdname) ^".*\)$")
    in
    let synopsis, pos1 =
      try
	if Str.string_partial_match reg1 help pos0
	then
	  BaseString.ltrim ~is_space:is_blank (Str.matched_group 1 help), (Str.match_end () +1)
	else
	  "", pos0
      with
	Not_found -> "", pos0
    in
    let reg2 = (* beginning of the list of options after the description ? *)
      Str.regexp "^\(.*[Oo]ptions.*\):[ \t]*\n\([ \t]*--?[a-zA-Z0-0]+\)"
    in
    let description, options =
      try
	let pos1a = Str.search_forward reg2 help pos1
	in
	 (* description *)
	BaseString.ltrim ~is_space:is_blank (BaseString.sub help pos1 (pos1a-pos1)),
        (* options *)
        let first_words =
	  BaseString.trim ~is_space:is_blank (Str.matched_group 1 help)
	in
	(* N.B. we try keep the last line before the first option unless it's just "Options:" *)
	let pos2 = 
	  if first_words = "Options" || first_words = "options" then Str.group_beginning 2 else pos1a
	in
	BaseString.rtrim ~is_space:is_blank (Str.string_after help pos2)
      with
	Not_found ->
 	  BaseString.ltrim ~is_space:is_blank (Str.string_after help pos1), ""
        (* no option? then put everything in description *)
    in
    summary, synopsis, description, options    

let summary = 
  match read_section "summary", help_summary with
    None, s when s <> "" -> Some(s)
  | x, _ -> x

let synopsis =
  match read_section "synopsis", help_synopsis with
  | None, s when s <> "" -> Some(s)
  | x, _ -> x

let description =
  match read_section "description", help_description with
    None, s when s <> "" -> Some(s)
  | x, _ -> x

let options =
  match read_section "options", help_options with
    None, s when s <> "" -> Some(s)
  | x, _ -> x

let _ = BaseArg.write_simple_manpage
    ~cmdname
    ?summary
    ~section:1
    ~centerheader:"Opa Manual"
    ?synopsis
    ?description
    ?other:(match options with Some(str) -> Some["OPTIONS", str] | None -> None)
    stdout
