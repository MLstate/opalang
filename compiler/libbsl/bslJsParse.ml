(*
    Copyright © 2012 MLstate

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

(**
   @author Arthur Azevedo de Amorim
*)

module Format = BaseFormat
module List = BaseList
module String = BaseString
module BD = BslDirectives.Js
module BT = BslTypes
module BRS = BslRegisterParserState
module J = JsAst

type tag = string
type message = string
type pos = FilePos.pos

type parsed_file = {
  directives: (FilePos.pos * BslTags.t * BslDirectives.Js.t) list;
  code: JsAst.code;
}

(** When trying to interpret a comment as a bsl directive, we do the
    following:

    - One group of tags, such as "@opaName" or "@raise", is used to
    build a BslTags.t value, that should be associated with a
    directive later.

    - Other tags, such as "@externType" and "@register", denote the
    actual bsl directives. They are mutually exclusive: there can be
    no comment with two or more of them.

    - Tags that are recognized but badly formatted (e.g. "@register I
    have no type") trigger an error. Other tags (such as "@param") are
    silently ignored.

    - Comments that have no directives are ignored as well.
*)

(* Example:

   /**
     * @register {int -> int} my_bypass
     * @cpsBypass
     *
     */
   function my_bypass(val) {
       return 4;
   }

*)

(** Builds a set of bsl tags based on comment annotations.
    TODO: treat repeated tags *)
let collect_bsl_tags tags =

  (* Reads tags with an associated set of strings *)
  let strings tag update bsl_tags =
    let aux (_, tag', args) =
      if tag = tag' then
        let attributes = Str.split (Str.regexp "[ \t]+") args in
        Some attributes
      else
        None
    in
    match List.find_map aux tags with
    | Some attributes -> Some (update bsl_tags attributes)
    | None -> Some bsl_tags
  in

  (* Sets a flag to true if it finds the corresponding tags *)
  let bool tag update bsl_tags =
    let rec aux tags =
      match tags with
      | [] -> Some bsl_tags
      | (_, tag', args) :: rest ->
        if tag <> tag' then
          aux rest
        else if Str.string_match (Str.regexp "[ \t]*") args 0 then
          Some (update bsl_tags)
        else
          None
    in aux tags
  in

  (* List of tags, their formats and how they update a BslTags.t *)
  let updates = [
    strings "noProjection" (fun t v ->
      let tags =
        if List.is_empty v then
          None
        else
          Some (StringSet.from_list v) in
      {t with BslTags.no_projection = tags}
    );
    strings "backend" (fun t v ->
      {t with BslTags.backend_restriction = Some (StringSet.from_list v)}
    );
    strings "restricted" (fun t v ->
      let tags =
        if List.is_empty v then
          None
        else
          Some v in
      {t with BslTags.restricted = Some tags}
    );
    bool "opaName" (fun t -> {t with BslTags.opaname = true});
    bool "normalize" (fun t -> {t with BslTags.opaname = false});
    bool "raise" (fun t -> {t with BslTags.raise_ = true});
    bool "cpsBypass" (fun t -> {t with BslTags.cps_bypass = true});
    bool "pure" (fun t -> {t with BslTags.pure = true});
    bool "opacapi" (fun t -> {t with BslTags.opacapi = true});
  ] in

  let rec try_updates bsl_tags updates =
    match updates with
    | [] -> Some bsl_tags
    | update :: rest -> (
      match update bsl_tags with
      | Some bsl_tags' -> try_updates bsl_tags' rest
      | None -> None
    )
  in

  try_updates BslTags.default updates

(** The next set of tags corresponds to directive tags. We try to
    extract a directive from each comment line. In the end, we check
    if a unique directive was defined or not. *)

type global_read_result = [ `no_occurrences
                          | `wrong_format of pos * message
                          | `found of (pos * tag * BD.t) list ]

type local_read_result = [ `wrong_format of pos * message
                         | `found of BD.t ]

(** Extracts all occurrences of tag [keyword] *)
let try_read_args tag
    (arg_reader : pos -> string -> local_read_result)
    tags : global_read_result =
  let rec aux acc tags =
    match tags with
    | [] ->
        begin match acc with
        | [] -> `no_occurrences
        | _ -> `found acc
        end
    | (pos, tag', args) :: rest ->
        if tag <> tag' then aux acc rest
        else match arg_reader pos args with
        | `wrong_format _ as s -> s
        | `found directive ->
            aux ((pos, tag, directive) :: acc) rest
  in
  aux [] tags

let identifier_regexp =
  Str.regexp "^[ \t]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*$"

let get_identifier string =
  if Str.string_match identifier_regexp string 0 then
    Some (Str.matched_group 1 string)
  else
    None

(** Reads something of the form "@tag I.am.a_type('var1, 'var2)" *)
let extract_type_declaration =
  let type_regexp =
    Str.regexp (
      "^[ \t]*" ^ (* whitespace *)
      "\\([^( \t]*\\)" ^ (* type name *)
      "\\((\\([^)]*\\))\\)?" ^ (* optional type vars *)
      "[ \t]*$" (* ending white space *)
    )
  in
  let split_vars_regexp = Str.regexp "[ \t]*,[ \t]*" in
  let var_regexp = Str.regexp "'[a-z]*" in
  fun tag constructor ->
    try_read_args tag (fun pos args ->
      if Str.string_match type_regexp args 0 then
        let name = Str.matched_group 1 args in
        try
          let vars = Str.matched_group 3 args in
          let vars = Str.split split_vars_regexp vars in
          if List.for_all (fun s -> Str.string_match var_regexp s 0) vars then
            let vars = List.map (fun var ->
              let var = Str.string_after var 1 in
              BRS.TypeVar.var var
            ) vars in
            `found (constructor name vars)
          else
            `wrong_format (pos,
              Printf.sprintf
                "Couldn't read type in @%s directive"
                tag
            )
        with
          Not_found ->
            `found (constructor name [])
      else
        `wrong_format (pos,
          Printf.sprintf
            "Directive @%s requires a type"
            tag
        )
    )

(** The tag readers, one for each recognized tag *)

let extract_extern_type_def =
  extract_type_declaration "externType" (fun ty args ->
    BD.ExternalTypeDef (ty, args)
  )

let extract_opa_type_def =
  extract_type_declaration "opaType" (fun ty args ->
    BD.OpaTypeDef (ty, args)
  )

let extract_module =
  try_read_args "module" (fun pos args ->
    match get_identifier args with
    | Some i -> `found (BD.Module i)
    | None -> `wrong_format (pos, "Expected identifier in module declaration")
  )

let extract_side =
  try_read_args "side" (fun pos args ->
    match String.trim(args) with
    | "{client}" -> `found (BD.BslSide `client)
    | "{server}" -> `found (BD.BslSide `server)
    | "{both}" -> `found (BD.BslSide `both)
    | s ->
        `wrong_format (pos,
                       Printf.sprintf "@side%s declaration is invalid.\nside annotations takes only one of these value : [client,server,both]" s)
  )


let extract_end_module =
  try_read_args "endModule" (fun pos args ->
    if args = "" then
      `found BD.EndModule
    else
      `wrong_format (pos, "@endModule takes no arguments")
  )

let extract_register implementation =
  let re = Str.regexp (
    "^[{(]\\([^}]*\\)[)}]" ^ (* Type between brackets or paren *)
    "\\([ \t]+[a-zA-Z][a-zA-Z0-9_]*\\)?[ \t]*" ^ (* Optional bypass name *)
    "\\([^ \t]+\\)?[ \t]*$" (* Optional source code *)
  ) in
  try_read_args "register" (fun pos args ->
    if Str.string_match re args 0 then
      let ty = String.trim (Str.matched_group 1 args) in
      let ty =
        try
          `success (snd (BslRegisterParser.parse_bslregisterparser_bslty ty))
        with
          Trx_runtime.SyntaxError (_, message) ->
            `error (Printf.sprintf "Couldn't read type: %s" message)
      in
      let name =
        try
          `success (JsCons.Ident.native
                      (String.trim (Str.matched_group 2 args)))
        with
          Not_found ->
            match implementation with
            | `func (ident, args1) -> (
              (* Check if arities match *)
              match ty with
              | `success BT.Fun (_, args2, _) ->
                let l1 = List.length args1 in
                let l2 = List.length args2 in
                if l1 <> l2 then
                  `error (Printf.sprintf (
                    "Function definition takes %d arguments, "^^
                    "but its registred type expects %d"
                  ) l1 l2)
                else
                  `success ident
              | _ -> `success ident
            )
            | `var ident -> `success ident
            | `none -> `error "Missing bypass name in @register declaration"
      in
      let definition =
        try
          `success (BD.Inline (Str.matched_group 3 args))
        with
          Not_found ->
            match implementation with
            | `func (ident, _)
            | `var ident -> `success (BD.Regular ident)
            | `none -> `error "Missing definition in @register declaration"
      in
      match name, definition, ty with
      | `success name, `success definition, `success ty ->
        `found (BD.Register (JsIdent.to_string name, definition, ty))
      | `success _, `error message, _
      | `error message, `success _, _
      | _, _, `error message -> `wrong_format (pos, message)
      | `error m1, `error m2, _ -> `wrong_format (pos, Printf.sprintf "%s, %s" m1 m2)
    else
      `wrong_format
        (pos, "Format of @register is \"@register {type} key [optional source]\"")
  )

let readers implementation = [
  extract_side;
  extract_extern_type_def;
  extract_opa_type_def;
  extract_module;
  extract_end_module;
  extract_register implementation;
]

type extract_result =
| NoOccurrences
| Error of string
| Found of (pos * BslTags.t * BD.t) list

(** Try to extract a bsl directive from a list of tags *)
let maybe_extract_directive implementation tags : extract_result =
  let extracted_directives = List.map (fun extract ->
    extract tags
  ) (readers implementation) in
  let rec aux acc extracted_directives =
    match extracted_directives with
    | [] -> (
      match acc with
      | [] -> NoOccurrences
      | _ -> Found acc
      )
    | extracted :: rest -> (
      match extracted with
      | `no_occurrences -> aux acc rest
      | `found truc -> (
          match collect_bsl_tags tags with
          | Some bsl_tags ->
              let acc =
                (List.map (fun (p, _, d) -> (p, bsl_tags, d)) truc) @ acc
              in
              aux acc rest
          | None -> Error "Badly formatted BSL tags"
      )
      | `wrong_format (pos, message) ->
          Error (Format.sprintf "%a%s" FilePos.pp_citation pos message)
      )
  in
  aux [] extracted_directives

let filter_lines lines = List.filter_map (fun line ->
  match line with
  | JsLex.CommentLine _ -> None
  | JsLex.CommentTag (pos, tag, args) -> Some (pos, tag, args)
) lines

let rec analyze_comments directives code =
  match code with
  | J.Js_comment (_, J.Jc_doc (_, lines)) :: rest -> (
    let tags = filter_lines lines in
    let implementation, rest = match rest with
      | J.Js_function (_, ident, args, _) :: rest
      | J.Js_var (_, ident, Some (J.Je_function (_, _, args, _))) :: rest ->
        `func (ident, args), rest
      | J.Js_var (_, ident, _) :: rest ->
        `var ident, rest
      | _ -> `none, rest in
    match maybe_extract_directive implementation tags with
    | NoOccurrences -> analyze_comments directives rest
    | Error e -> `error e
    | Found found ->
        analyze_comments (found @ directives) rest
    )
  | _ :: rest -> analyze_comments directives rest
  | [] -> `success (List.rev directives)

let process code =
  match analyze_comments [] code with
  | `error e -> `error e
  | `success directives ->
      `success {directives; code}

let parse_file filename =
  try
    let code = JsParse.File.code ~comments:true ~throw_exn:true filename in
    process code
  with
    JsParse.Exception e ->
      `error (Format.to_string JsParse.pp e)

let parse_string ?filename content =
  try
    let code = JsParse.String.code ~comments:true ?filename ~throw_exn:true content in
    process code
  with
    JsParse.Exception e ->
      `error (Format.to_string JsParse.pp e)
