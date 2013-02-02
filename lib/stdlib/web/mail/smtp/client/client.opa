/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin mail

import stdlib.web.mail
import stdlib.crypto

// type Email.attachment = {
//   filename: string
//   mimetype: string
//   encoding: string
//   content: string
// }

SmtpClient = {{

  /**
   * {1} Sending Email.
   */

  string_of_send_status(s : Email.send_status) =
    match s with
    | { bad_sender } -> "bad sender"
    | { bad_recipient } -> "bad recipient"
    | { sending } -> "sending"
    | { ok=s } -> "ok : {s}"
    | { error=e } -> "error : {e}"

  emails_list_to_string(emails) =
    List.map(Email.to_string, emails)
    |> List.to_string_using("", "", ", ", _)

  @private
  send_async(
    from : Email.email,
    to : Email.email,
    subject : string,
    mail_content : Email.content,
    options : Email.options,
    k : (Email.send_status -> void)
  ) : void =
    (text, html) =
      match mail_content with
      | {~text} -> (text, "")
      | {~text ~html} -> (text, Xhtml.serialize_as_standalone_html(html))
      | {~html} -> (Xhtml.to_readable_string(html), Xhtml.serialize_as_standalone_html(html))

    files =
      List.filter_map(x ->
        match x with
        | ~{filename content mime_type} ->
           if String.has_prefix("text/", mime_type)
           then some((filename,mime_type,"8bit",content))
           else some((filename,mime_type,"base64",
                      Crypto.Base64.encode(binary_of_string(content))))
        | ~{filename content mime_type encoding} ->
          some((filename,mime_type,encoding,content))
        | ~{filename resource} ->
           match Resource.export_data(resource) with
           | {some=~{data mimetype}} ->
             if String.has_prefix("text/", mimetype)
             then some((filename, mimetype, "8bit", string_of_binary(data)))
             else some((filename, mimetype, "base64", Crypto.Base64.encode(data)))
           | {none} -> {none}
           end
      , options.files)

    f = %% BslNativeLib.ocaml_tuple_4 %%
    files = %% BslNativeLib.opa_list_to_ocaml_list %%(f, files)
    f = %% BslNativeLib.ocaml_tuple_2 %%
    custom_headers = %% BslNativeLib.opa_list_to_ocaml_list %%(f, options.custom_headers)
    mto =
      if List.is_empty(options.to) then Email.to_string(to)
      else emails_list_to_string(options.to)
    mcc = emails_list_to_string(options.cc)
    mbcc = emails_list_to_string(options.bcc)
    dst = [to] ++ options.to ++ options.cc ++ options.bcc
    dst = List.map(Email.to_string_only_address, List.unique_list_of(dst))
    dst = %% BslNativeLib.opa_list_to_ocaml_list %%(identity, dst)
    %% BslMail.SmtpClient.mail_send_fun %%(
      Email.to_string(from), Email.to_string_only_address(from),
      dst, mto, mcc, mbcc,
      subject, text, html,
      files, custom_headers,
      options.via,
      options.server_addr, options.server_port,
      options.auth, options.user, options.pass,
      options.dryrun,
      options.secure_type, k
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
