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
(* CF mli *)
type filename = string
type contents = string

module ByPassMap = BslLib.BSL.ByPassMap
module D = BslDirectives

module List = Base.List
module String = Base.String

(* FIXME: think about where to put the function now *)
let now = DebugTracer.now

let (!?) pos fmt =
  if WarningClass.is_warn WarningClass.bsl_register
  then (
    OManager.printf "%a@\n" FilePos.pp_citation pos ;
    OManager.warning ~wclass:WarningClass.bsl_register fmt
  )
  else
    Format.ifprintf Format.std_formatter fmt

let (!!) pos fmt =
  OManager.printf "%a@\n" FilePos.pp_citation pos ;
  OManager.error fmt

let preprocess ~final_bymap decorated_file =
  let filename = decorated_file.D.filename in
  let browser = ByPassMap.Browser.init final_bymap in
  let fold_left buf parsed =
    let pos = D.pos parsed in
    let (!?) x = !? pos x in
    let (!!) x = !! pos x in
    match parsed with
      | D.Source (_, s) ->
          let s =
            if String.is_contained ";;" s then (
              !? "This line contains a toplevel separator @{<bright>';;'@}@\nIt will be removed to assure parser-compatibility@\n" ;
              String.replace s ";;" "  "
            ) else s
          in
          FBuffer.addln buf s

      (*
        When a format definition is found, BslRegisterParser stores it in a table,
        and then, the BSL Browser access this table to solve the inclusion.
        In opa syntax, bsl format definition can after preprocessing just be ignored
      *)
      | D.Directive (_, _, D.FormatDefinition _) -> buf

      | D.Directive (_, _, D.IncludeType strreg) ->

          let regexp = Str.regexp strreg in
          let match_any_type = ref false in
          let buf =
            ByPassMap.fold_types final_bymap (
              fun buf t ->
                let name =
                  match t with
                  | BslTypes.External (_, name, _) -> name
                  | _ -> assert false
                in
                if Str.string_match regexp name 0 then (
                  match_any_type := true ;
                  FBuffer.printf buf "%a@\n" BslTypesGeneration.Opa.pp_definition t
                )
                else buf
            ) buf
          in
          if not (!match_any_type) then (
            !? (
              "##include-type, regexpr=%S@\nThis inclusion produces an empty code@\n"^^
              "@[<2>@{<bright>Hint@}:@\n"^^
              "This inclusion may be deprecated, or the types may@\n"^^
              "have been renamed, and do not match the regexp anymore.@]@\n"
            )
              strreg ;
            ()
          );
          buf


      | D.Directive (_, _, D.Include (fmt, link)) ->
          let link = String.lowercase link in (
            match ByPassMap.Browser.Path.of_string link with
            | None ->
                !! "##include, format=<abstr>, path=%S@\nThis is not a valid syntax for a path.@\n" link
            | Some path -> (
                match ByPassMap.Browser.Path.cd browser path with
                | Some elt ->
                    (* TODO: fix whenever BslLib.include_format will uses Format *)
                    let fixme_string_instead_of_format = ByPassMap.Browser.include_format elt fmt in
                    FBuffer.addln buf fixme_string_instead_of_format

                | None ->
                    !! "##include, format=<abstr>, path=%S@\nCannot resolve this path.@\n@[<2>@{<brigh>Hint@}:@\n+ Check if your lib defines such path (ml or js)@\n+ use @{<bright>bslbrowser@} for previous plugins (depends)@]@\n" link
              )
          )

  in
  let buf = FBuffer.create ( 8 * 1024 ) in
  let buf = FBuffer.printf buf "/* File: %S -- auto preprocessing bsl : %S */\n" filename (now()) in
  let buf = List.fold_left fold_left buf decorated_file.D.decorated_source in
  filename, buf


(* Checking *)

(*
  TODO:
  when we will finaly remove mlstatebsl files, we will remove opa files from opa-plugin-builder files
  and remove the module BslOpa
*)

type true_means_error = bool

let checking_fail ~final_bymap:_ _opa_code =
  ( false : true_means_error) , []
