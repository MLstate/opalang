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

module List = BaseList
module String = BaseString
module BD = BslDirectives
module BT = BslTypes
module BRS = BslRegisterParserState

open JsLex

(* No source positions for now *)
let dummy_pos = FilePos.nopos "BslJsParse"
let label () = Annot.next_label dummy_pos

type tag = string
type message = string

let whitespace = Str.regexp "[ \t]*"

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

(** Builds a set of bsl tags based on comment annotations *)
let collect_bsl_tags tags =

  (* Reads tags with an associated set of strings *)
  let string_set tag update bsl_tags =
    let aux (tag', args) =
      if tag = tag' then
        let attributes = Str.split whitespace args in
        Some (StringSet.from_list attributes)
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
      | (tag', args) :: rest ->
        if tag <> tag' then
          aux rest
        else if Str.string_match whitespace args 0 then
          Some (update bsl_tags true)
        else
          None
    in aux tags
  in

  (* List of tags, their formats and how they update a BslTags.t *)
  let updates = [
    string_set "noProjection" (fun t v ->
      {t with BslTags.no_projection = Some v}
    );
    bool "opaName" (fun t v -> {t with BslTags.opaname = v});
    bool "raise" (fun t v -> {t with BslTags.raise_ = v});
    bool "cpsBypass" (fun t v -> {t with BslTags.cps_bypass = v});
    bool "opacapi" (fun t v -> {t with BslTags.opacapi = v});
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
                          | `wrong_format of message
                          | `multiple_occurrences of tag
                          | `found of tag * BD.bypasslang_directive ]

type local_read_result = [ `wrong_format of message
                         | `found of BD.bypasslang_directive ]

(** Extracts all occurrences of tag [keyword] *)
let try_read_args tag
    (arg_reader : string -> local_read_result)
    tags =
  let rec aux acc tags =
    match tags with
    | [] ->
      Option.default_map `no_occurrences
        (fun dir -> `found (tag, dir))
        acc
    | (tag', args) :: rest ->
      if tag <> tag' then
        aux acc rest
      else if Option.is_some acc then
        `multiple_occurrences tag
      else
        match arg_reader args with
        | `wrong_format _ as s -> s
        | `found directive -> aux (Some directive) rest
  in
  aux None tags

let identifier_regexp =
  Str.regexp "^[ \t]*\\([a-zA-Z][a-zA-Z0-9]*\\)[ \t]*$"

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
    try_read_args tag (fun args ->
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
            `wrong_format (
              Printf.sprintf
                "Couldn't read type in @%s directive"
                tag
            )
        with
          Not_found ->
            `found (constructor name [])
      else
        `wrong_format (
          Printf.sprintf
            "Directive @%s requires a type"
            tag
        )
    )

(** The tag readers, one for each recognized tag *)

let extract_extern_type_def =
  extract_type_declaration "externType" (fun ty args ->
    BD.ExternalTypeDef (ty, args, None)
  )

let extract_opa_type_def =
  extract_type_declaration "opaType" (fun ty args ->
    BD.OpaTypeDef (ty, args)
  )

let extract_module =
  try_read_args "module" (fun args ->
    match get_identifier args with
    | Some i -> `found (BD.Module (i, None))
    | None -> `wrong_format "Expected identifier in module declaration"
  )

let extract_end_module =
  try_read_args "endModule" (fun args ->
    if args = "" then
      `found BD.EndModule
    else
      `wrong_format "@endModule takes no arguments"
  )

let extract_register implementation =
  let re = Str.regexp (
    "^{\\([^}]*\\)}" ^ (* Type between brackets *)
    "\\([ \t]+[a-zA-Z][a-zA-Z0-9]*\\)?[ \t]*" ^ (* Optional bypass name *)
    "\\([^ \t]+\\)?[ \t]*$" (* Optional source code *)
  ) in
  try_read_args "register" (fun args ->
    if Str.string_match re args 0 then
      (* TODO:
         - Find a way of telling apart injected and not injected with
         different name *)
      let ty = Str.matched_group 1 args in
      let (_, ty) = BslRegisterParser.parse_bslregisterparser_bslty ty in
      let name =
        try
          Some (String.trim (Str.matched_group 2 args))
        with
          Not_found -> implementation in
      let source =
        try
          Some (Str.matched_group 3 args)
        with
          Not_found -> implementation in
      let injected = Option.is_some source in
      match name with
      | Some name -> `found (BD.Register (name, source, injected, ty))
      | None -> `wrong_format "Missing bypass name in @register declaration"
    else
      `wrong_format
        "Format of @register is \"@register {type} key [optional source]\""
  )

let readers implementation = [
  extract_extern_type_def;
  extract_opa_type_def;
  extract_module;
  extract_end_module;
  extract_register implementation;
]

type extract_result =
| NoOccurrences
| Error of string
| Found of BslTags.t * BD.bypasslang_directive

(** Try to extract a bsl directive from a list of tags, *)
let maybe_extract_directive implementation tags : extract_result =
  let extracted_directives = List.map (fun extract ->
    extract tags
  ) (readers implementation) in
  let rec aux acc extracted_directives =
    match extracted_directives with
    | [] -> (
      match acc with
      | None -> NoOccurrences
      | Some (_, d) ->  (
        match collect_bsl_tags tags with
        | Some bsl_tags -> Found (bsl_tags, d)
        | None -> Error "Badly formatted BSL tags"
      )
    )
    | extracted :: rest -> (
      match extracted with
      | `no_occurrences -> aux acc rest
      | `found (name, directive) -> (
        match acc with
        | None -> aux (Some (name, directive)) rest
        | Some (name', _directive') ->
          Error (
            Printf.sprintf
              "Multiple directives have been found: @%s and @%s"
              name name'
          )
      )
      | `wrong_format message -> Error message
      | `multiple_occurrences name ->
        Error (
          Printf.sprintf "Multiple occurrences of tag @%s" name
        )
    )
  in
  aux None extracted_directives

let filter_lines lines = List.filter_map (fun line ->
  match line with
  | CommentLine _ -> None
  | CommentTag (tag, args) -> Some (tag, args)
) lines

let rec doc_comment acc = parser
  | [< 'DocComment lines; stream >] -> (
    let tags = filter_lines lines in
    let implementation =
      match Stream.npeek 2 stream with
      | [Function; Ident name] -> Some name
      | [Var; Ident name] -> Some name
      | _ -> None
    in
    match maybe_extract_directive implementation tags with
    | NoOccurrences -> doc_comment acc stream
    | Error e -> `error e
    | Found (bsl_tags, d) ->
      doc_comment ((dummy_pos, bsl_tags, d) :: acc) stream
  )
  | [< _ = Stream.next; stream >] -> doc_comment acc stream
  | [< >] -> `success (List.rev acc)

let parse filename =
  let stream, _lexbuf = JsLex.stream_of_file ~lex_comments:true filename in
  doc_comment [] stream
