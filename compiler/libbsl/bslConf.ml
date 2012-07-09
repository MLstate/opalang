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
module Arg = BaseArg
module Format = BaseFormat
module String = BaseString

(* - *)

type properties = {
  cclib : string list ;
  ccopt : string list ;
  mlcopt : string list ;
  mllopt : string list ;
  mlinclude : string list ;
  mllibs : string list ;
}

let pp_list fmt list = Format.pp_list " ; " Format.pp_print_string fmt list

let pp_if word fmt list =
  if list <> []
  then
    Format.fprintf fmt "@\n%s: [ %a ]" word pp_list list

let pp_properties fmt properties =
  Format.fprintf fmt "@[<2>{" ;
  pp_if "cclib" fmt properties.cclib ;
  pp_if "ccopt" fmt properties.ccopt ;
  pp_if "mlcopt" fmt properties.mlcopt ;
  pp_if "mllopt" fmt properties.mllopt ;
  pp_if "mlinclude" fmt properties.mlinclude ;
  pp_if "mllibs" fmt properties.mllibs ;
  Format.fprintf fmt "@]@\n}"

(*
  <!> Beware, this type is marshaled during the bsl preprocess
*)
type conf = {
  all_platform: properties ;
  linux: properties option ;
  mac:properties option ;
  windows: properties option ;
  cygwin: properties option ;
}

let pp_if word fmt opt =
  match opt with
  | None -> ()
  | Some props ->
      Format.fprintf fmt "@\n%s: %a" word pp_properties props

let pp fmt conf =
  Format.fprintf fmt "@[<2>{" ;
  pp_if "all-platform" fmt (Some conf.all_platform) ;
  pp_if "linux" fmt conf.linux ;
  pp_if "mac" fmt conf.mac ;
  pp_if "windows" fmt conf.windows ;
  pp_if "cygwin" fmt conf.cygwin ;
  Format.fprintf fmt "@]@\n}"

type t = conf

let default_properties = {
  cclib = [] ;
  ccopt = [] ;
  mlcopt = [] ;
  mllopt = [] ;
  mlinclude = [] ;
  mllibs = [] ;
}

let is_default_properties def =
     def.cclib = []
  && def.ccopt = []
  && def.mlcopt = []
  && def.mllopt = []
  && def.mlinclude = []
  && def.mllibs = []

let is_default_properties_opt = function
  | None -> true
  | Some def -> is_default_properties def

let default = {
  all_platform = default_properties ;
  linux = None ;
  mac = None ;
  windows = None ;
  cygwin = None ;
}

let default_conf = default

let is_default def =
  true
  && is_default_properties def.all_platform
  && is_default_properties_opt def.linux
  && is_default_properties_opt def.windows
  && is_default_properties_opt def.cygwin
  && is_default_properties_opt def.mac

(*
  Parsing the file.
  A line can be a new property, with an optional platform,
  or contents
*)
let regexp_property = Str.regexp "\\[\\([^]:]*\\)\\(:\\([^]:]*\\)\\)?\\]$"
let property_platform line =
  if Str.string_match regexp_property line 0
  then
    let property = Str.matched_group 1 line in
    let platform = try Some (Str.matched_group 3 line) with Not_found -> None in
    Some (property, platform)
  else
    None

let all_properties = [
  "mlcopt" ;
  "mllopt" ;
  "cclib" ;
  "ccopt" ;
  "mlinclude" ;
  "mllibs" ;
]

let all_platform = [
  "linux" ;
  "windows" ;
  "cygwin" ;
  "mac" ;
]

let check_property_platform pos property platform =
  let (!!) fmt = OManager.error ("%a@\n"^^fmt) FilePos.pp pos in
  let property =
    match String.trim property with
    | "mllopt" -> `mllopt
    | "mlcopt" -> `mlcopt
    | "cclib" -> `cclib
    | "ccopt" -> `ccopt
    | "mlinclude" -> `mlinclude
    | "mllibs" -> `mllibs
    | unknown -> !! "Invalid property %S@\n%a" unknown (HintUtils.pp_suggestion all_properties) unknown
  in
  let platform =
    match platform with
    | None -> `all
    | Some platform -> (
        match String.trim platform with
        | "linux" -> `linux
        | "max" -> `mac
        | "windows" -> `windows
        | "cygwin" -> `cygwin
        | unknown -> !! "Invalid platform %S@\n%a" unknown (HintUtils.pp_suggestion all_properties) unknown
      )
  in
  property, platform

(*
  Convention: everything lists are reversed during the fold
*)

let add_args acc line =
  let args = Arg.split line in
  List.rev_append args acc

let add_property properties property line =
  match property with
  | `cclib -> {
      properties with
        cclib = add_args properties.cclib line
    }
  | `ccopt -> {
      properties with
        ccopt = add_args properties.ccopt line
    }
  | `mlcopt -> {
      properties with
        mlcopt = add_args properties.mlcopt line
    }
  | `mllopt -> {
      properties with
        mllopt = add_args properties.mllopt line
    }
  | `mlinclude -> {
      properties with
        mlinclude = add_args properties.mlinclude line
    }
  | `mllibs -> {
      properties with
        mllibs = add_args properties.mllibs line
    }

let add property platform t line =
  match platform with
  | `all -> {
      t with
        all_platform = add_property t.all_platform property line
    }
  | `linux ->
      let properties = Option.default default_properties t.linux in
      { t with linux = Some (add_property properties property line) }
  | `windows ->
      let properties = Option.default default_properties t.windows in
      { t with windows = Some (add_property properties property line) }
  | `cygwin ->
      let properties = Option.default default_properties t.cygwin in
      { t with cygwin = Some (add_property properties property line) }
  | `mac ->
      let properties = Option.default default_properties t.mac in
      { t with mac = Some (add_property properties property line) }

let fold ~filename t =
  let () =
    if not (File.is_regular filename) then
      OManager.error "I/O error: @{<bright>%S@} -> No such file" filename
  in
  let () =
    let content =
      match File.content_opt filename with
      | Some content -> content
      | None ->
          OManager.error "I/O error: cannot read conf file @{<bright>%S@}" filename
    in
    FilePos.add_file filename content
  in
  let fold_line (((property, platform) as props, t) as acc) line line_number =
    let line = String.trim line in
    if String.length line = 0 || line.[0] = '#' then acc else
    let pos = FilePos.make_pos_from_line filename line_number in
    match property_platform line with
    | Some (property, platform) ->
        let props = check_property_platform pos property platform in
        props, t
    | None -> (
        match property with
        | `none ->
            OManager.error "%a@\nSyntax error. The conf file should start with a property." FilePos.pp pos
        | (
            `cclib
          | `ccopt
          | `mlcopt
          | `mllopt
          | `mlinclude
          | `mllibs
          ) as prop ->
            let t = add prop platform t line in
            props, t
      )
  in
  let _, t = File.lines_foldi fold_line ((`none, `all), t) filename in
  let () =
    #<If:BSL_REGISTER $contains "conf">
      OManager.printf "@[<2>@{<cyan>BslConf:@} folding file %S:@\n %a@]@\n" filename pp t ;
    #<End>
  in
  t

let export t =
  let rev = List.rev in
  let rev_prop prop =
    let { cclib ; ccopt ; mlcopt ; mllopt ; mlinclude ; mllibs } = prop in
    {
      cclib = rev cclib ;
      ccopt = rev ccopt ;
      mlcopt = rev mlcopt ;
      mllopt = rev mllopt ;
      mlinclude = rev mlinclude ;
      mllibs = rev mllibs ;
    } in
  let rev_prop_opt = Option.map rev_prop in
  let { all_platform ; linux ; mac ; windows ; cygwin } = t in
  {
    all_platform = rev_prop all_platform ;
    linux = rev_prop_opt linux ;
    mac = rev_prop_opt mac ;
    windows = rev_prop_opt windows ;
    cygwin = rev_prop_opt cygwin ;
  }
