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
 * Library for handling MIME.
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
  data : binary
}

type Mime.charset = string

type Mime.body_part =
  { plain : (Mime.charset, binary) }
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
 * Module for handling MIME message.
 * @see http://tools.ietf.org/html/{rfc2045/rfc2046/rfc2047}
 *
 * Encoding can be handled afterwards, with the Iconv module
 */
Mime = {{

  @private crlf = "\r\n"
  @private crlf_parser = parser Rule.of_string(crlf) -> void
  @private double_crlf_parser = parser Rule.rep(2, crlf_parser) -> void
  @private default_charset = "utf-8"

  @private Q = {{

    q_parser = parser
    | c=((!"=" !"_" .)+) -> Text.to_string(c)
    | "=" fst=Rule.hexadecimal snd=Rule.hexadecimal -> String.of_byte_val(16 * fst + snd)
    | "_"  -> " "

    decode(s:string) =
      p = parser l=q_parser+ -> String.flatten(l)
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
      | _ -> do Log.warning("Mime", "EncodedWords.string_to_encoding: unknown encoding {s}") none

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

    ew_parser(decoder) = parser
    | ew=encoded_word ->
      match string_to_encoding(ew.encoding)
      | {some={Q_encoding}} -> decoder(ew.charset, Q.decode(ew.encoded_text))
      | {some={B_encoding}} -> decoder(ew.charset, Crypto.Base64.decode(ew.encoded_text))
      | {none} -> decoder(ew.charset, ew.encoded_text)
      end
    | c=. -> Cactutf.cons(c)

    decode(s:string, decoder) : string =
      p = parser value=ew_parser(decoder)+ -> List.to_string_using("", "", "", value)
      match Parser.try_parse(p, s)
      {some=s} -> s
      {none} -> s

  }}

  @private QuotedPrintable = {{

    qp_parser = parser
    | c=((!"=" .)+) -> some(Text.to_string(c))
    | "=" fst=Rule.hexadecimal snd=Rule.hexadecimal -> some(String.of_byte_val(16 * fst + snd))
    | "=" crlf_parser -> none
    | "=" -> none

    decode(s:string) : string =
      p = parser l=QuotedPrintable.qp_parser+ ->
        l = List.filter_map(identity, l)
        String.flatten(l)
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

    decode_value(s:string, decoder) : string =
      EncodedWords.decode(s, decoder)

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
      not_double_dash = parser !"--" !crlf_parser . -> void
      delimiter =
        parser crlf_parser? not_double_dash* "--" "{b}" -> void
      close_delimiter =
        parser delimiter "--" -> void
      body_part =
        parser bp=((!delimiter Rule.full_line)*) -> Text.to_string(bp)
      encapsulation =
        parser delimiter crlf_parser content=body_part -> content
      parser
      | (!delimiter .)* parts=encapsulation+ close_delimiter .* ->
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
    {none} -> {plain=(default_charset, body |> binary_of_string)}
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
          if String.has_prefix("\t", e) then
            ([], "{acc} {String.trim(e)}")
          else if String.contains(e, ":") then
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
    {none} -> {plain=(default_charset, body |> binary_of_string)} // No Content-Type, handle as plain text
    {some=content_type} ->

      content_type_list =
        String.explode_with(";", content_type, false)
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
      binary_body = binary_of_string(decoded_body)

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
      | _ ->
        match Header.find("Content-Disposition", headers)
        {none} ->
          if content_type == "text/html" then
            {html=Xhtml.of_string(decoded_body)}
          else
            {plain=(charset, binary_body)}
        {some=cd} ->
          if content_type == "text/plain" && cd == "inline" then
            {plain=(charset, binary_body)}
          else if content_type == "text/html" && cd == "inline" then
            {html=Xhtml.of_string(decoded_body)}
          else
            filename = String.explode(";", cd)
                       |> List.map(String.trim, _)
                       |> Header.extract_value("filename", _)
            if String.is_empty(filename) then {plain=(charset, binary_body)}
            else
            { attachment = {
              ~filename
              mimetype = content_type
              data = binary_body
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
   *
   * @param raw the raw string message
   * @return an option MIME message
   */
  parse(raw:string) : option(Mime.message) =
    match parse_entity(raw)
    {none} -> none
    {some=content} -> some({~raw ~content})

  /**
   * Accessors
   */

  // Text

  @private
  body_to_string(body:Mime.body_part, decoder) : string =
    match body
    {~plain} -> decoder(plain.f1, plain.f2)
    {html=_} -> ""
    {attachment=_} -> ""
    {multipart=parts} ->
      List.fold(part, acc ->
        acc + get_text_aux(part, decoder)
      , parts, "")

  @private
  get_text_aux(entity:Mime.entity, decoder) =
    body_to_string(entity.body, decoder)

  /**
   * Get the textual content of a MIME message
   *
   * @param message the MIME message
   * @param decoder the function to use to decode each textual part of the message
   * @return the textual content of the MIME message
   */
  get_text(message:Mime.message, decoder:(string, binary -> string)) : string =
    get_text_aux(message.content, decoder)

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
   *
   * @param message the Mime message
   * @return the xhtml content of the MIME message
   */
  get_xhtml(message:Mime.message) : xhtml =
    get_xhtml_aux(message.content)

  // Attachments

  @private
  content_to_attachments(body:Mime.body_part, decoder, acc) =
    match body
    {plain=_} -> acc
    {html=_} -> acc
    {~attachment} ->
      attachment = { attachment with filename = EncodedWords.decode(attachment.filename, decoder) }
      [attachment|acc]
    {multipart=parts} ->
      List.fold(get_attachments_aux(_, decoder, _), parts, acc)

  @private
  get_attachments_aux(entity:Mime.entity, decoder, acc) =
    content_to_attachments(entity.body, decoder, acc)

  /**
   * Get the attachments of a MIME message
   *
   * @param message the MIME  message
   * @return a list of MIME attachements
   */
  get_attachments(message:Mime.message, decoder) : list(Mime.attachment) =
    get_attachments_aux(message.content, decoder, [])

}}
