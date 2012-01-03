(*
    Copyright © 2011, 2012 MLstate

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

##module mailserve

  let status_ok = ServerLib.static_field_of_name "ok"
  let status_bad_sender = ServerLib.static_field_of_name "bad_sender"
  let status_bad_recipient = ServerLib.static_field_of_name "bad_recipient"
  let status_error = ServerLib.static_field_of_name "error"

  ##opa-type Email.send_status

  ##register [cps-bypass] mail_send_fun : string, string, string, string, string, string, string,\
    caml_list(caml_tuple_4(string,string,string,string)), \
    caml_list(caml_tuple_2(string,string)), \
    (opa[email_send_status], continuation(opa[void]) -> void), \
    continuation(opa[void]) -> void
  let mail_send_fun mfrom mfrom_address_only mdst mto subject mdata html files custom_headers cont k =
    let cont = BslUtils.proj_cps k cont in
    let cont x =
      let res =
        match x with
        | SmtpClientCore.Ok -> ServerLib.make_simple_record status_ok
        | SmtpClientCore.Bad_Sender -> ServerLib.make_simple_record status_bad_sender
        | SmtpClientCore.Bad_Recipient -> ServerLib.make_simple_record status_bad_recipient
        | SmtpClientCore.Error err ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc status_error (ServerLib.wrap_string err) in
            ServerLib.make_record rc
        | SmtpClientCore.Error_MX ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc status_error (ServerLib.wrap_string "Error MX") in
            ServerLib.make_record rc
        | SmtpClientCore.Delayed i ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc status_error (ServerLib.wrap_string ("Delayed "^(string_of_int i))) in
            ServerLib.make_record rc
      in cont (wrap_opa_email_send_status res)
    in
    let html = if html = "" then None else Some html
    and mto = if mto = "" then None else Some mto in
    SmtpClient.mail_send_aux BslScheduler.opa ~charset:"UTF-8" ~subject mfrom mdst ?mto:mto mdata ?html:html ~files ~custom_headers ~return_path:mfrom_address_only 10 cont ();
    QmlCpsServerLib.return k ServerLib.void

##endmodule


##module mailserver

  ##register init_server : int, string, SSL.secure_type, \
    (opa[string], opa[list(string)], opa[string] -> opa[tuple_2(int, string)]) -> void
  let init_server port addr secure_type handler =
    let ssl_certificate, ssl_verify_params = secure_type in
    let caml_handler email =
      let f = ServerLib.wrap_string email.SmtpServerCore.from in
      let c = Rcontent.get_content email.SmtpServerCore.body in
      let c = ServerLib.wrap_string c in
      let t = BslNativeLib.caml_list_to_opa_list ServerLib.wrap_string email.SmtpServerCore.dests in
      let res = handler f t c in
      let i, s = BslNativeLib.ocaml_tuple_2 res in
      ServerLib.unwrap_int i, ServerLib.unwrap_string s
    in
    Runtime.add_smtpServer "smtpServer" {
      SmtpServer.default_options with
        SmtpServer.opt_addr = addr;
        SmtpServer.opt_port = port;
        SmtpServer.opt_ssl_certificate   = ssl_certificate;
        SmtpServer.opt_ssl_verify_params = ssl_verify_params;
        SmtpServer.opt_email_handler = caml_handler;
    }

##endmodule
