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
    option(string), \
    (opa[email_send_status], continuation(opa[void]) -> void), \
    continuation(opa[void]) -> void
  let mail_send_fun mfrom mfrom_address_only mdst mto subject mdata html files custom_headers via cont k =
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
    SmtpClient.mail_send_aux BslScheduler.opa ~charset:"UTF-8" ~subject mfrom mdst ?mto:mto mdata ?html:html ~files ~custom_headers ~return_path:mfrom_address_only 10 ?via:via cont ();
    QmlCpsServerLib.return k ServerLib.void

##endmodule


##module imap

  ##opa-type Email.imap_command
  ##opa-type Email.imap_result

  let ok = ServerLib.static_field_of_name "Ok"
  let searchresult = ServerLib.static_field_of_name "SearchResult"
  let fetchresult = ServerLib.static_field_of_name "FetchResult"
  let no = ServerLib.static_field_of_name "No"
  let bad = ServerLib.static_field_of_name "Bad"
  let error = ServerLib.static_field_of_name "Error"

  ##register [cps-bypass] command : int, string, SSL.secure_type, \
    string, string, string, opa[email_imap_command], \
    (opa[email_imap_result], continuation(opa[void]) -> void), \
    continuation(opa[void]) -> void

  let command port addr secure_type mailbox username password command cont k =
    let cont = BslUtils.proj_cps k cont in

    let wrap_is (i,s) = BslNativeLib.opa_tuple_2 (ServerLib.wrap_int i, ServerLib.wrap_string s) in

    let cont x =
      let res =
        match x with
        | ImapClientCore.Ok str ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc ok (ServerLib.wrap_string str) in
            ServerLib.make_record rc
        | ImapClientCore.No str ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc no (ServerLib.wrap_string str) in
            ServerLib.make_record rc
        | ImapClientCore.Bad str ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc bad (ServerLib.wrap_string str) in
            ServerLib.make_record rc
        | ImapClientCore.Error err ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc error (ServerLib.wrap_string err) in
            ServerLib.make_record rc
        | ImapClientCore.SearchResult il ->
            let opa_il = BslNativeLib.caml_list_to_opa_list ServerLib.wrap_int il in
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc searchresult opa_il in
            ServerLib.make_record rc
        | ImapClientCore.FetchResult isl ->
            let opa_isl = BslNativeLib.caml_list_to_opa_list wrap_is isl in
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc fetchresult opa_isl in
            ServerLib.make_record rc
      in cont (wrap_opa_email_imap_result res)
    in

    let command = unwrap_opa_email_imap_command command in
    let command =
      ServerLib.fold_record
        (fun f value _cmd ->
           let value = Obj.magic(value) in
           match ServerLib.name_of_field f with
           | Some "ImapNoop" -> ImapClientCore.ImapNoop
           | Some "ImapSearch" -> ImapClientCore.ImapSearch (ServerLib.unwrap_string value)
           | Some "ImapSearchCs" ->
               let charset, params = BslNativeLib.ocaml_tuple_2 value in
               ImapClientCore.ImapSearchCs (ServerLib.unwrap_string charset, ServerLib.unwrap_string params)
           | Some "ImapFetch" ->
               let seq, items = BslNativeLib.ocaml_tuple_2 value in
               ImapClientCore.ImapFetch (ServerLib.unwrap_string seq, ServerLib.unwrap_string items)
           | _ -> assert false)
        command ImapClientCore.ImapNoop
    in

    let client_certificate, verify_params = secure_type in

    ImapClient.mail_recv ?client_certificate ?verify_params ~secure:true BslScheduler.opa ~addr ~port
      ~mailbox ~username ~password ~command (cont:ImapClientCore.result -> unit) ();
    QmlCpsServerLib.return k ServerLib.void

##endmodule


##module mailserver

  ##register [cps-bypass] init_server : int, string, SSL.secure_type, \
    (opa[string], opa[list(string)], opa[string], continuation(opa[tuple_2(int, string)]) -> void), continuation(opa[void]) -> void
  let init_server port addr secure_type handler cvoid =
    let ssl_certificate, ssl_verify_params = secure_type in
    let caml_handler email k =
      let f = ServerLib.wrap_string email.SmtpServerCore.from in
      let c = Rcontent.get_content email.SmtpServerCore.body in
      let c = ServerLib.wrap_string c in
      let t = BslNativeLib.caml_list_to_opa_list ServerLib.wrap_string email.SmtpServerCore.dests in
      handler f t c (QmlCpsServerLib.cont_ml (
                       fun res -> let i, s = BslNativeLib.ocaml_tuple_2 res in
                       k (ServerLib.unwrap_int i, ServerLib.unwrap_string s)))
    in
    let _ = Runtime.add_smtpServer "smtpServer" {
      SmtpServer.default_options with
        SmtpServer.opt_addr = addr;
        SmtpServer.opt_port = port;
        SmtpServer.opt_ssl_certificate   = ssl_certificate;
        SmtpServer.opt_ssl_verify_params = ssl_verify_params;
        SmtpServer.opt_email_handler = caml_handler;
    } in QmlCpsServerLib.return cvoid ServerLib.void

##endmodule

