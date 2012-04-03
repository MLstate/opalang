/*
    Copyright © 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * Library for treating MIME.
 *
 * @author Frederic Ye, 2012
 * @category Email
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

import stdlib.crypto

/**
 * Type definitions
 */

type Mime.EncodedWords.encoding =
  {Q_encoding}
/ {B_encoding}

type Mime.Multipart.encoding =
  {`7bit`}
/ {`8bit`}
/ {binary}

type Mime.header = {
  name : string
  value : string
}
type Mime.headers = list(Mime.header)

type Mime.attachment = {
  filename : string
  mimetype : string
  data : string
}

type Mime.body_part =
  { plain : string }
/ { html : xhtml }
/ { attachment : Mime.attachment }
/ { multipart : list(Mime.entity) }

type Mime.entity = {
  headers : Mime.headers
  body : Mime.body_part
}

type Mime.message = {
  raw : string
  content : Mime.entity
}

/**
 * Module for treating MIME message.
 * @see http://tools.ietf.org/html/{rfc2045/rfc2046/rfc2047}
 *
 * FIXME: Encoding is not handled, we consider everything as UTF-8
 */
Mime = {{

  @private crlf = "\r\n"
  @private crlf_parser = parser Rule.of_string(crlf) -> void
  @private double_crlf_parser = parser Rule.rep(2, crlf_parser) -> void
  @private default_charset = "utf8"

  @private Q = {{

    q_parser = parser
    | "=" fst=Rule.hexadecimal snd=Rule.hexadecimal -> 16 * fst + snd
    | "_" -> 320 // <=> =20
    | c=. -> c

    decode(s:string, _charset:string) =
      p = parser l=q_parser+ -> Text.to_string(Text.lcconcat(l))
      match Parser.try_parse(p, s)
      {some=s} -> s
      {none} -> s

  }}

  /**
   * http://tools.ietf.org/html/rfc2047
   */
  @private EncodedWords = {{

    string_to_encoding(s:string) : option(Mime.EncodedWords.encoding) =
      match String.uppercase(s)
      | "Q" -> some({Q_encoding})
      | "B" -> some({B_encoding})
      | _ -> do warning("EncodedWords.string_to_encoding: unknown encoding {s}") none

    /** FIXME: limit to 75 long... */
    encoded_word = parser
    | "=?" ~charset "?" ~encoding "?" ~encoded_text "?=" "  "? -> {
      charset = String.lowercase(charset)
      encoding = String.lowercase(encoding)
      ~encoded_text
    }

    charset = token

    encoding = token

    token = parser
    | s=(!(especials|" ") .)+ ->
      Text.to_string(Text.ltconcat(s))

    especials = parser
    | c=("("|")"|"<"|">"|"@"|","|";"|":"|"\""|"/"|"["|"]"|"?"|"."|"=") ->
      Text.to_string(c)

    encoded_text = parser
    | s=(!(" "|"?") .)+ -> Text.to_string(Text.ltconcat(s))

    ew_parser = parser
    | ew=encoded_word ->
      match string_to_encoding(ew.encoding)
      | {some={Q_encoding}} -> Q.decode(ew.encoded_text, ew.charset)
      | {some={B_encoding}} -> Crypto.Base64.decode(ew.encoded_text)
      | {none} -> ew.encoded_text
      end
    | c=. -> Cactutf.cons(c)

    decode(s:string) : string =
      p = parser value=ew_parser+ -> List.to_string_using("", "", "", value)
      match Parser.try_parse(p, s)
      {some=s} -> s
      {none} -> s

  }}

  @private QuotedPrintable = {{

    qp_parser = parser
    | "=" fst=Rule.hexadecimal snd=Rule.hexadecimal -> some(16 * fst + snd)
    | "=" crlf_parser -> none
    | "=" -> none
    | c=. -> some(c)

    decode(s:string) : string =
      p = parser l=QuotedPrintable.qp_parser+ ->
        l = List.filter_map(identity, l)
        Text.to_string(Text.lcconcat(l))
      match Parser.try_parse(p, s)
      {some=s} -> s
      {none} -> s

  }}

  Header = {{

    find(name:string, headers:Mime.headers) : option(string) =
      List.find_map(e ->
        if String.lowercase(e.name) == String.lowercase(name) then some(e.value)
        else none
      , headers)

    decode_value(s:string) : string =
      EncodedWords.decode(s)

    extract_value(name, values) =
      name = String.lowercase(name)
      List.fold(line, acc ->
        q = parser "\"" -> "\""
        s_parser = parser
                   | q s=(!q .)* q -> Text.to_string(Text.ltconcat(s))
                   | s=(.*) -> Text.to_string(s)
        v_parser = parser Rule.of_string(name) "=" s=s_parser -> s
        match Parser.try_parse(v_parser, line)
        {some=v} -> String.trim(v)
        {none} -> acc
      , values, "")

  }}

  /**
   * http://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
   */
  @private Multipart = {{

    parser(b:string) =
      delimiter =
        parser crlf_parser? (!("--"|crlf_parser) .)* "--" Parser.of_string(b) -> void
      close_delimiter =
        parser delimiter "--" -> void
      body_part =
        parser bp=(!(delimiter|close_delimiter) .)* ->
          Text.to_string(Text.ltconcat(bp))
      encapsulation =
        parser delimiter crlf_parser content=body_part -> content
      parser
      | (!delimiter .)* parts=encapsulation+ close_delimiter (.*) ->
        List.map(content ->
          match parse_entity(content)
          {some=part} -> part
          {none} -> { headers=[] body={multipart=[]} }
        , parts) |> some
      | .* -> none

  }}

  /**
   * Decode a string given a content transfer encoding
   */
  @private
  decode_body(s:string, cte:string) =
    match String.lowercase(cte)
    | "quoted-printable" -> QuotedPrintable.decode(s)
    | "base64" -> Crypto.Base64.decode2(s)
    | _ -> String.replace(crlf, "\n", s)

  /**
   * Parsing
   */

  @private
  multipart(body, boundary) =
    match Parser.try_parse_opt(Multipart.parser(boundary), body)
    {none} -> {plain=body}
    {some=content} -> {multipart=content}

  @private
  split_header_body(s:string) : option((string, string)) =
    Parser.try_parse(
      parser header=(!double_crlf_parser .)* double_crlf_parser content=(.*) ->
        (Text.to_string(Text.ltconcat(header)), Text.to_string(content))
    , s)

  // FIXME: use TRX for that
  @private
  parse_header(s:string) : Mime.headers =
    merge_fields(l) =
      (a, b) =
        List.fold_map(e, acc ->
          if String.contains(e, ":") then
            ([acc], e)
          else
            ([], "{acc} {e}")
        , l, "")
      res = [[b]|a] |> List.filter(e->e!=[""], _) |> List.flatten(_)
      res
    split_header(h) =
      match String.index(":", h)
      {some=i} ->
        name = String.substring(0, i, h)
               |> String.trim(_)
        value = String.substring(i+1, String.length(h)-i-1, h)
                |> String.trim(_)
        some({ ~name ~value })
      {none} -> none
    String.explode("\r\n", s)
    |> merge_fields(_)
    |> List.filter_map(split_header, _)

  @private
  parse_body_part(headers:Mime.headers, body:string) : Mime.body_part =
    match Header.find("Content-Type", headers)
    {none} -> {plain=body} // No Content-Type, treat as plain text
    {some=content_type} ->

      content_type_list =
        String.explode(";", content_type)
        |> List.map(String.trim, _)
      charset =
        cs = Header.extract_value("charset", content_type_list)
        if cs == "" then default_charset else cs
      content_type =
        if List.length(content_type_list) <= 1 then content_type
        else List.head(content_type_list)
      decoded_body =
        match Header.find("Content-Transfer-Encoding", headers)
        {none} -> decode_body(body, charset)
        {some=cte} -> decode_body(body, cte)

      match content_type
      | "multipart/mixed" ->
        boundary = Header.extract_value("boundary", content_type_list)
        multipart(body, boundary)
      | "multipart/alternative" ->
        boundary = Header.extract_value("boundary", content_type_list)
        multipart(body, boundary)
      | "multipart/related" ->
        boundary = Header.extract_value("boundary", content_type_list)
        multipart(body, boundary)
      | "text/html" -> {html=Xhtml.of_string(decoded_body)}
      | _ ->
        match Header.find("Content-Disposition", headers)
        {none} -> {plain=decoded_body}
        {some=cd} ->
          if content_type == "text/plain" && cd == "inline" then
            {plain=decoded_body}
          else
            filename = String.explode(";", cd)
                       |> List.map(String.trim, _)
                       |> Header.extract_value("filename", _)
            if String.is_empty(filename) then {plain=decoded_body}
            else
            { attachment = {
              ~filename
              mimetype = content_type
              data = decoded_body
            }}

  @private
  parse_entity(s:string) : option(Mime.entity) =
    match split_header_body(s)
    {none} -> none
    {some=(header, body)} ->
      headers = parse_header(header)
      body = parse_body_part(headers, body)
      some({ ~headers body=body })

  /**
   * Parse a raw string into an MIME message
   */
  parse(s:string) : option(Mime.message) =
    match parse_entity(s)
    {none} -> none
    {some=content} -> some({
      raw = s
      content = content
    })

  /**
   * Access
   */

  // Text

  @private
  body_to_string(body:Mime.body_part) : string =
    match body
    {~plain} -> plain
    {html=_} -> ""
    {attachment=_} -> ""
    {multipart=parts} ->
      List.map(get_text_aux, parts)
      |> List.to_string_using("", "", "", _)

  @private
  get_text_aux(entity:Mime.entity) =
    body_to_string(entity.body)

  /**
   * Get the textual content of a MIME message
   */
  get_text(message:Mime.message) =
    get_text_aux(message.content)

  // HTML

  @private
  body_to_xhtml(body:Mime.body_part) : xhtml =
    match body
    {plain=_} -> <></>
    {~html} -> html
    {attachment=_} -> <></>
    {multipart=parts} ->
      List.map(get_xhtml_aux, parts)
      |> Xhtml.createFragment(_)

  @private
  get_xhtml_aux(entity:Mime.entity) =
    body_to_xhtml(entity.body)

  /**
   * Get the xhtml content of a MIME message
   */
  get_xhtml(message:Mime.message) =
    get_xhtml_aux(message.content)

  // Attachments

  @private
  content_to_attachments(body:Mime.body_part, acc) =
    match body
    {plain=_} -> acc
    {html=_} -> acc
    {~attachment} -> [attachment|acc]
    {multipart=parts} ->
      List.fold(get_attachments_aux, parts, acc)

  @private
  get_attachments_aux(entity:Mime.entity, acc) =
    content_to_attachments(entity.body, acc)

  /**
   * Get the attachments of a MIME message
   */
  get_attachments(message:Mime.message) : list(Mime.attachment) =
    get_attachments_aux(message.content, [])

}}
