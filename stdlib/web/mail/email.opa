/*
    Copyright © 2011, 2012 MLstate

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
 * @author Adam Koprowski, Hugo Heuzard 2011
 * @author Frederic Ye 2012
 *
 * @destination public
 */

import stdlib.crypto

/**
 * {1 Types}
 */

/**
 * This type represents an email address. It contains two fields: [local] and [domain].
 * [local] is the local part of the address (before the @), usually the username.
 * [domain] is the fully qualified address domain name (after the @).
 */
type Email.address = {
  local : string
  domain : string
}

/**
 * This type represents a rich email address. It contains two fields: [name] and [address].
 * [name] is a optional user-friendly name (string) and [address] is an [Email.address].
 * For instance
 * "John Smith" <john.smith@some.server.com>
 * will give the following data instance:
 * [{ name = some("John Smith"); address = john.smith@some.server.com }].
 */
type Email.email = {
  name : option(string)
  address : Email.address
}

/**
 * Type of a file attachment
 */
type Email.file =
  { filename:string content:string mime_type:string }
/ { filename:string content:string mime_type:string encoding:string }
/ { filename:string resource:resource }

/**
 * A list of file attachments
 */
type Email.attachments = list(Email.file)

/**
 * Type of an email content
 * if you give {~html} then a text will be computed out of the xhtml,
 * to be non html part to your email
 */
type Email.content =
  {text:string}
/ {html:xhtml}
/ {text:string html:xhtml}

/**
 * Return status of an email sending.
 */
type Email.send_status =
   { bad_sender }
 / { bad_recipient }
 / { sending }
 / { ok }
 / { error : string }

/**
 * Options for sending an email.
 * Most fields correspond to the email headers.
 * If to is an empty list, the recipient address will be used for the {b To} field.
 * We do NOT check if custom_headers already contains {b To:} or {b Cc:} fields.
 */
type Email.options = {
  to : list(Email.email)
  cc : list(Email.email)
  custom_headers : list((string, string))
  files : Email.attachments
  via : option(string)
  server_addr : option(string)
  server_port : option(int)
  auth : option(string)
  user : option(string)
  pass : option(string)
  dryrun : bool
  secure_type : option(SSL.secure_type)
}

type Email.imap_command =
    { ImapNoop }
  / { ImapFetch : (bool, string, string) }
  / { ImapStore : (bool, string, string, string) }
  / { ImapSearch : (bool, string) }
  / { ImapSearchCs : (bool, string, string) }
  / { ImapList : (string, string) }
  / { ImapCreate : string }
  / { ImapDelete : string }
  / { ImapRename : (string, string) }
  / { ImapExpunge }

type Email.imap_status = {
  flags : string
  exists : int
  recent : int
  oks : string list
  rwstatus : string
}

type Email.imap_result =
    { Ok : string }
  / { No : string }
  / { Bad : string }
  // { NoopResult : Email.imap_status }
  / { NoopResult : (string,int,int,list(string),string) }
  / { SearchResult : list(int) }
  / { FetchResult : list((int, string)) }
  / { StoreResult : list((int, string)) }
  / { ListResult : list((string, string, string)) }
  / { ExpungeResult : list(int) }
  / { Error : string }

type caml_tuple_2('a,'b) = external
type caml_tuple_4('a,'b,'c,'d) = external

/**
 * This module is meant to deal with email address and email sending
 */
Email = {{

  default_options = {
    to = []
    cc = []
    custom_headers = []
    files = []
    via = none
    server_addr = none
    server_port = none
    auth = none
    user = none
    pass = none
    dryrun = false
    secure_type = none
  } : Email.options

  /**
   * {1 Parsing email addresses}
   */

  @private
  dblquote = parser [\"];

  /**
   * A parser accepting characters allowed as parts of an email
   * address, that is:
   *  - small and capital letters,
   *  - digits,
   *  - characters: !#$%&'*+\-/=?^_`{|}~
   * See: http://en.wikipedia.org/wiki/Email_address#Local_part
   */
  @private
  char = parser [A-Za-z0-9!#$%&'*+\-/=?^_`{|}~];

  @private
  word = parser w=char+ -> Text.to_string(Text.ltconcat(w))

  /**
   * A parser for a local part of the email address, i.e.
   * a series of email allowed {!char}s, separated by dots.
   */
  @private
  local = Rule.parse_list_sep_min_length(1, false, word, parser "." -> void)
   // full RFC compliant list of special characters: ! # $ % & ' * + - / = ? ^ _ ` { | }

  /**
   * A parser for an email address, i.e. a local part of the
   * address ({!id}), followed by the 'at' character (@),
   * followed by the {!domain} address.
   */
  address_parser : Parser.general_parser(Email.address) = parser local=local [@] domain=UriParser.domain -> {local=String.concat(".",local) ~domain};

  /**
   * A parser for a rich email address giving a value of type {!Email.email}.
   * An email address should be of the form:
   *  "John Smith" <john.smith@some.server.com>
   * or
   *  John Smith <john.smith@some.server.com>
   * or
   *  john.smith@some.server.com
   *
   * For a more detailed description of the accepted format of emails see:
   * {{:http://en.wikipedia.org/wiki/E-mail_address#RFC_specification}E-mail address: RFC specification (Wikipedia)}
   */
  email_parser : Parser.general_parser(Email.email) =
    email_name =
      parser
        // we either have a name in double quotes (needs to be followed by "<" for the address), "..." <...>
      | dblquote name=(!dblquote .)* dblquote Rule.ws &[<] -> Text.ltconcat(name) |> Text.to_string |> some
        // or a name without double quotes (again needs to be followed by "<"), ... <...>
      | name=(![<] .)* &[<] -> Text.ltconcat(name) |> Text.to_string |> String.trim |> some
        // otherwise there's no name component
      | {Rule.succeed} -> none
    email_address =
      parser
        // email address can be in between angle brackets <...>
      | [<] Rule.ws email=address_parser Rule.ws [>] -> email
        // ... or without them
      | email=address_parser -> email
    parser Rule.ws name=email_name Rule.ws address=email_address Rule.ws -> ~{name address}

  /**
   * A simpler parser, not looking for email name.
   */
  email_simple_parser : Parser.general_parser(Email.email) = parser Rule.ws email=address_parser Rule.ws -> {name=none ; address=email}

  /**
   * {1 String conversion}
   */

  /**
   * Convertion from a string to {!Email.email}.
   *
   * @param s string for conversion.
   * @return an optional email represented by the given string.
   */
  of_string_opt(s:string) = Parser.try_parse(Email.email_parser, s)

  /**
   * Convertion from a string to email.
   *
   * @param s string for conversion.
   * @return email represented by the given string or error
   *         if [s] did not represesent a valid email address.
   */
  of_string(s:string) =
    of_string_opt(s) ? error("Wrong email: {s}")

  @stringifier(Email.email) to_string(email:Email.email) =
    match email.name with
    | {~some} -> "\"{some}\" <{email.address.local}@{email.address.domain}>"
    | {none} -> "{email.address.local}@{email.address.domain}"

  @stringifier(Email.email) to_name(email:Email.email) =
    match email.name with
    | {~some} -> some
    | {none} -> "{email.address.local}@{email.address.domain}"

  @xmlizer(Email.email) to_xml(email) =
    <>{"{email}"}</>

  address_to_string(address:Email.address) =
    "{address.local}@{address.domain}"

  to_string_only_address(email:Email.email) =
    "{email.address.local}@{email.address.domain}"

  is_valid_string(s:string) = Option.is_some(of_string_opt(s))

  /**
   * {1} Sending Email.
   */

  string_of_send_status(s : Email.send_status) =
    match s with
    | { bad_sender } -> "bad sender"
    | { bad_recipient } -> "bad recipient"
    | { sending } -> "sending"
    | { ok } -> "ok"
    | { error=e } -> "error : {e}"

  @private
  caml_list2(l) =
    rec aux(l,acc) =
      match l with
        | [] -> acc
        | [hd|tl] -> aux(tl, %%BslNativeLib.cons%%(%%BslNativeLib.ocaml_tuple_2%%(hd),acc))
    aux(List.rev(l),%%BslNativeLib.empty_list%%)

  @private
  caml_list4(l) =
    rec aux(l,acc) =
      match l with
        | [] -> acc
        | [hd|tl] -> aux(tl, %%BslNativeLib.cons%%(%%BslNativeLib.ocaml_tuple_4%%(hd),acc))
    aux(List.rev(l),%%BslNativeLib.empty_list%%)

  @private
  send_mail = %% BslMail.Mailserve.mail_send_fun %% : string , string, string , string , string , string, string, caml_list(caml_tuple_4(string,string,string,string)), caml_list(caml_tuple_2(string, string)), option(string), option(string), option(string), option(string), option(string), bool, (Email.send_status -> void) -> void

  @private
  send_mail_secure = %% BslMail.Mailserve.mail_send_fun_secure %% : string , string, string , string , string , string, string, caml_list(caml_tuple_4(string,string,string,string)), caml_list(caml_tuple_2(string, string)), option(string), option(string), option(int), option(string), option(string), option(string), bool, SSL.secure_type, (Email.send_status -> void) -> void

  @private
  send_async(
    from : Email.email,
    to : Email.email,
    subject : string,
    mail_content : Email.content,
    options : Email.options,
    k : (Email.send_status -> void)) : void =
    (text, html) = match mail_content with
                   | {~text} -> (text, "")
                   | {~text ~html} -> (text, Xhtml.serialize_as_standalone_html(html))
                   | {~html} -> (Xhtml.to_readable_string(html), Xhtml.serialize_as_standalone_html(html))
                   end
    files = List.filter_map(x ->
              match x with
              | ~{filename content mime_type} ->
                 if String.has_prefix("text/", mime_type)
                 then some((filename,mime_type,"8bit",content))
                 else some((filename,mime_type,"base64",Crypto.Base64.encode(content)))
              | ~{filename content mime_type encoding} -> some((filename,mime_type,encoding,content))
              | ~{filename resource} ->
                 match Resource.export_data(resource) with
                 | {some=~{data mimetype}} ->
                   if String.has_prefix("text/", mimetype)
                   then some((filename,mimetype,"8bit",data))
                   else some((filename,mimetype,"base64",Crypto.Base64.encode(data)))
                 | {none} -> {none}
                 end
            , options.files) |> caml_list4(_)
    custom_headers = caml_list2(options.custom_headers)
    custom_headers = if List.is_empty(options.cc) then custom_headers
                     else
                       s = List.foldi(i, e, acc ->
                             if i == 0 then to_string(e)
                             else acc ^ ", " ^ to_string(e)
                           , options.cc, "")
                       %%BslNativeLib.cons%%(%%BslNativeLib.ocaml_tuple_2%%(("Cc", s)), custom_headers)
    mto = if List.is_empty(options.to) then ""
          else
            List.foldi(i, e, acc ->
              if i == 0 then to_string(e)
              else acc ^ ", " ^ to_string(e)
            , options.to, "")
    match options.secure_type with
    | {some=secure_type} ->
      send_mail_secure(
        to_string(from), to_string_only_address(from), to_string_only_address(to), mto,
        subject, text, html, files, custom_headers,
        options.via, options.server_addr, options.server_port, options.auth, options.user, options.pass, options.dryrun,
        secure_type, k
      )
    | {none} ->
      send_mail(
        to_string(from), to_string_only_address(from), to_string_only_address(to), mto,
        subject, text, html, files, custom_headers,
        options.via, options.server_addr, options.auth, options.user, options.pass, options.dryrun, k
      )

  /**
   * Try to send a mail {b synchronously}
   */
  try_send(from : Email.email, to : Email.email, subject : string, content : Email.content, options : Email.options) : Email.send_status =
    k(cont) =
      f(r) = Continuation.return(cont, r)
      send_async(from, to, subject, content, options, f)
    @callcc(k)

  /**
   * Try to send a mail {b asynchronously}
   */
  try_send_async(from : Email.email, to : Email.email, subject : string, content : Email.content, options : Email.options, continuation : (Email.send_status -> void)) : void =
    send_async(from, to, subject, content, options, continuation)

}}

Imap = {{

  command = %% BslMail.Imap.command %% : int , string, SSL.secure_type, string , bool, string , string, list(Email.imap_command), (list(Email.imap_result) -> void) -> void

}}

