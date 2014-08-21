/*
    Copyright © 2011, 2012, 2013 MLstate

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

/**
 * Type of the SMTP options. They define the configuration of the server.
 */
type Smtp.options = {
  via : option(string)
  host : option(string)
  port : option(int)
  auth : option(string)
  user : option(string)
  pass : option(string)
  secure : option(SSL.secure_type)
}

SmtpClient = {{

  default_options = {
    via = none
    host = none
    port = none
    auth = none
    user = none
    pass = none
    secure = none
  }: Smtp.options

  /**
   * {1} Sending Email.
   */

  string_of_send_status(s : Email.send_status) =
    match s with
    | { bad_sender } -> "bad sender"
    | { bad_recipients=r } -> "bad recipients : {r}"
    | { sending } -> "sending"
    | { ok=s } -> "ok : {s}"
    | { error=e } -> "error : {e}"

  emails_list_to_string(emails) =
    List.map(Email.to_string, emails)
    |> List.to_string_using("", "", ", ", _)

  @private
  send_async(mail_options: Email.options, text: string, html: string, smtp_options: Smtp.options, k: (Email.send_status -> void)): void =
    tuple2 = %% BslNativeLib.ocaml_tuple_2 %%
    tuple4 = %% BslNativeLib.ocaml_tuple_4 %%

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
      , mail_options.files)

    files = %% BslNativeLib.opa_list_to_ocaml_list %%(tuple4, files)
    custom_headers = %% BslNativeLib.opa_list_to_ocaml_list %%(tuple2, mail_options.custom_headers)

    mfrom = Email.to_string(mail_options.from)
    mto = emails_list_to_string(mail_options.to)
    mcc = emails_list_to_string(mail_options.cc)
    mbcc = emails_list_to_string(mail_options.bcc)

    %% BslSmtp.SmtpClient.send_mail %%(
      mfrom, mto, mcc, mbcc,
      mail_options.subject, text, html,
      files, custom_headers,
      smtp_options.via, smtp_options.host, smtp_options.port,
      smtp_options.auth, smtp_options.user, smtp_options.pass,
      smtp_options.secure, k
    )

  /**
   * Try to send a mail {b synchronously}
   */
  try_send(mail_options: Email.options, content: Email.content, smtp_options: Smtp.options): Email.send_status =
    (text, html) =
      match content with
      | {~text} -> (text, "")
      | {~text ~html} -> (text, Xhtml.serialize_as_standalone_html(html))
      | {~html} -> (Xhtml.to_readable_string(html), Xhtml.serialize_as_standalone_html(html))
    k(cont) =
      f(r) = Continuation.return(cont, r)
      send_async(mail_options, text, html, smtp_options, f)
    @callcc(k)

  /**
   * Try to send a mail {b asynchronously}
   */
  try_send_async(mail_options: Email.options, content: Email.content, smtp_options: Smtp.options, continuation: (Email.send_status -> void)): void =
    (text, html) =
      match content with
      | {~text} -> (text, "")
      | {~text ~html} -> (text, Xhtml.serialize_as_standalone_html(html))
      | {~html} -> (Xhtml.to_readable_string(html), Xhtml.serialize_as_standalone_html(html))
    send_async(mail_options, text, html, smtp_options, continuation)

  /**
   * Try to send a mail {b asynchronously}
   */
  try_send_async_preserialized(mail_options: Email.options, text: string, html: string, smtp_options: Smtp.options, continuation: (Email.send_status -> void)): void =
    send_async(mail_options, text, html, smtp_options, continuation)

  /**
   * Perform a dryrun to test the validity of the SMTP server.
   */
  check_smtp(options: Smtp.options): Email.send_status =
    k(cont) =
      f(r) = Continuation.return(cont, r)
      %% BslSmtp.SmtpClient.check_smtp %%(
        options.via, options.host, options.port,
        options.auth, options.user, options.pass,
        options.secure, f)
    @callcc(k)


}}
