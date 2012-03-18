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
   Adding hyperlink to the generated svg.
   @author Mathieu Barbin
*)

(**
   This module is an ad-hoc implementation for adding hyperlinks to
   a svg file.
   The format of the svg should be very specific, there is no parser for svg.

   The format is the one seen in practice as the format produced by dot -Tsvg.
   The hypothesis done is that any node is surrounded by a comment, indicating
   the title of the node :
   {[
<!-- qmlslicer -->
<g id="node39" class="node"><title>qmlslicer</title>
<ellipse style="fill:none;stroke:black;" cx="645" cy="-162" rx="52.8111" ry="18"/>
<text text-anchor="middle" x="645" y="-157.9" style="font-family:Times New Roman;font-size:14.00;">qmlslicer</text>
</g>
   ]}


   The preprocess proposed by this file is meant to add extra href annotation,
   indexed by the name of the node.

   So, if in the map you have a binding :
   {[
   qmlslicer -> "odep__lib_qmlslicer.svg"
   ]}
   the preprocess will return :
<!-- qmlslicer -->
<a xlink:href="odep__lib_qmlslicer.svg">
<g id="node39" class="node"><title>qmlslicer</title>
<ellipse style="fill:none;stroke:black;" cx="645" cy="-162" rx="52.8111" ry="18"/>
<text text-anchor="middle" x="645" y="-157.9" style="font-family:Times New Roman;font-size:14.00;">qmlslicer</text>
</g>
</a>


   For using this application, you should give all the svg you want to process,
   the link added correspond to all found file, suffixed with name.
   In case of several matching file, the choosen link is unspecified.

   Exemple:

   odeplink toto.svg tutu.tata.svg

   link toto -> "toto".svg
   link tata -> tutu.tata.svg
*)

(* depends *)
module String = Base.String

let tmp = "odeplink.tmp"

(*
  The set contains file without extension
*)
let find_suffix set suffix =
  Return.set_checkpoint (
    fun label ->
      StringSet.iter (
        fun elt ->
          if String.is_suffix suffix elt
          then Return.return label (Some (elt^".svg"))
          else ()
      ) set ;
      None
  )

(*
  Given a list of file, compute the set of filename without extension
*)
let compute_set list =
  List.fold_left (fun set file -> StringSet.add (File.chop_extension file) set)
    StringSet.empty list

let pat_begin = "<!--\\(.*\\)-->"
let reg_begin = Str.regexp pat_begin

let pat_end = "</g>"
let reg_end = Str.regexp pat_end

let match_begin line =
  let line = String.trim line in
  if Str.string_match reg_begin line 0
  then (
    try
      let word = Str.matched_group 1 line in
      let word = String.trim word in
      Some word
    with
    | Not_found -> None
  )
  else
    None

let match_end line =
  let line = String.trim line in
  Str.string_match reg_end line 0

type env = string option
let start = None

let pp_line map env line output =
  Printf.fprintf output "%s\n" line ;
  match env with
  | Some _ -> (
      if match_end line
      then (
        Printf.fprintf output "</a>\n" ;
        None
      )
      else env
    )
  | None -> (
      (* search for a begin *)
      match match_begin line with
      | Some lib -> (
          match map lib with
          | Some link ->
              Printf.fprintf output "<a xlink:href=%S>\n" link ;
              Some lib
          | None ->
              env
        )
      | None -> env
    )

let preprocess map input output =
  try
    let rec aux env =
      let line = input_line input in
      let env = pp_line map env line output in
      aux env
    in
    aux start
  with
  | End_of_file ->
      flush output ;
      ()

let preprocess_file map filename =
  let oc = open_out tmp in
  let ic = open_in filename in
  preprocess map ic oc ;
  close_in ic ;
  close_out oc ;
  let _ = File.copy ~force:true tmp filename in
  ()

(*
  map a format, with NODE variable
*)
let node_pattern =
  let buf = Buffer.create 124 in
  let subst pat = function
    | "NODE" -> pat
    | e -> e
  in
  (fun pat node ->
     Buffer.clear buf ;
     Buffer.add_substitute buf (subst node) pat ;
     Buffer.contents buf)


let link = ref None
let svg = ref []

let spec = [
  (* l *)
  "--link",
  Arg.String (fun s -> link := Some s),
  " Add external link. e.g. : path/to/file/$(NODE).html"
]

let usage_msg = Printf.sprintf "%s: svg hyperlink adder\nUsage: %s *.svg\n" Sys.argv.(0) Sys.argv.(0)

let anon_fun file =
  match File.extension file with
  | "svg" ->
      svg := file :: !svg
  | _ ->
      Printf.eprintf "I don't know what to do with arg %S\n%s%!" file usage_msg ;
      exit 1

let parse () =
  Arg.parse spec anon_fun (usage_msg^"Options:")

let _ =
  parse ();
  let set = compute_set !svg in
  let map =
    match !link with
    | None -> find_suffix set
    | Some pat -> (
        fun node ->
          match find_suffix set node with
          | (Some _) as link  -> link
          | None ->
              if String.is_word node
              then
                Some (node_pattern pat node)
              else
                None
      )
  in
  List.iter (preprocess_file map) !svg
