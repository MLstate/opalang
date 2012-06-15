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

module BslUtils = OpabslgenMLRuntime.BslUtils
module BslNativeLib = OpabslgenMLRuntime.BslNativeLib
(** TODO - plugins dependencies *)
##property[mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##extern-type caml_tuple_2('a,'b) = ('a*'b)
##extern-type caml_tuple_4('a,'b,'c,'d) = ('a*'b*'c*'d)
##extern-type caml_list('a) = 'a list
##extern-type SSL.secure_type = SslAS.secure_type
##property[endmli]

##opa-type list('a)
##opa-type tuple_2('a, 'b)

let caml_list_to_opa_list f l =
  wrap_opa_list (BslNativeLib.unwrap_opa_list (BslNativeLib.caml_list_to_opa_list f l))
let opa_list_to_ocaml_list f l =
  BslNativeLib.opa_list_to_ocaml_list f (BslNativeLib.wrap_opa_list (unwrap_opa_list l))
(** *****************************)

##module mailserve

  let status_ok = ServerLib.static_field_of_name "ok"
  let status_bad_sender = ServerLib.static_field_of_name "bad_sender"
  let status_bad_recipient = ServerLib.static_field_of_name "bad_recipient"
  let status_error = ServerLib.static_field_of_name "error"

  ##opa-type Email.send_status

  ##register [cps-bypass] mail_send_fun : \
    string, string, \
    caml_list(string), \
    string, string, string, \
    string, string, string, \
    caml_list(caml_tuple_4(string,string,string,string)), \
    caml_list(caml_tuple_2(string,string)), \
    option(string), option(string), option(int), option(string), option(string), option(string), opa[bool], option(SSL.secure_type), \
    (opa[email_send_status], continuation(opa[void]) -> void), \
    continuation(opa[void]) -> void
  let mail_send_fun mfrom mfrom_address_only mdst mto mcc mbcc subject mdata html files custom_headers via addr port auth user pass dryrun secure_type cont k =
    let cont = BslUtils.proj_cps k cont in
    let cont x =
      let res =
        match x with
        | SmtpClientCore.Ok str ->
            let rc = ServerLib.empty_record_constructor in
            let rc = ServerLib.add_field rc status_ok (ServerLib.wrap_string str) in
            ServerLib.make_record rc
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
    and mto = if mto = "" then None else Some mto
    and dryrun = Some (ServerLib.unwrap_bool dryrun)
    in
    let mcc = if mcc = "" then None else Some mcc in
    let mbcc = if mbcc = "" then None else Some mbcc in
    List.iter (
      fun mdst ->
	match secure_type with
	| Some (client_certificate, verify_params) ->
	    SmtpClient.mail_send_aux ?client_certificate ?verify_params ~secure:true Scheduler.default ~charset:"UTF-8" ~subject mfrom mdst ?mto:mto ?mcc:mcc ?mbcc:mbcc mdata ?html:html ~files ~custom_headers ~return_path:mfrom_address_only 10 ?via:via ?addr:addr ?port:port ?auth:auth ?user:user ?pass:pass ?dryrun:dryrun cont ();
	    QmlCpsServerLib.return k ServerLib.void
	| None ->
	    SmtpClient.mail_send_aux Scheduler.default ~charset:"UTF-8" ~subject mfrom mdst ?mto:mto ?mcc:mcc ?mbcc:mbcc mdata ?html:html ~files ~custom_headers ~return_path:mfrom_address_only 10 ?via:via ?addr:addr ?port:port ?auth:auth ?user:user ?pass:pass ?dryrun:dryrun cont ();
	    QmlCpsServerLib.return k ServerLib.void
    ) mdst

##endmodule


##module imap

  ##opa-type Email.imap_command
  ##opa-type Email.imap_result
  ##opa-type Email.imap_status

  let ok = ServerLib.static_field_of_name "Ok"
  let selectresult = ServerLib.static_field_of_name "SelectResult"
  let examineresult = ServerLib.static_field_of_name "ExamineResult"
  let noopresult = ServerLib.static_field_of_name "NoopResult"
  let searchresult = ServerLib.static_field_of_name "SearchResult"
  let fetchresult = ServerLib.static_field_of_name "FetchResult"
  let storeresult = ServerLib.static_field_of_name "StoreResult"
  let statusresult = ServerLib.static_field_of_name "StatusResult"
  let listresult = ServerLib.static_field_of_name "ListResult"
  let expungeresult = ServerLib.static_field_of_name "ExpungeResult"
  let no = ServerLib.static_field_of_name "No"
  let bad = ServerLib.static_field_of_name "Bad"
  let error = ServerLib.static_field_of_name "Error"

  let flags = ServerLib.static_field_of_name "flags"
  let exists = ServerLib.static_field_of_name "exists"
  let recent = ServerLib.static_field_of_name "recent"
  let oks = ServerLib.static_field_of_name "oks"
  let rwstatus = ServerLib.static_field_of_name "rwstatus"

  ##register [cps-bypass] command : int, string, SSL.secure_type, \
    string, string, opa[list(email_imap_command)], \
    (opa[list(email_imap_result)], continuation(opa[void]) -> void), \
    continuation(opa[void]) -> void

  let command port addr secure_type username password commands cont k =
    let cont = BslUtils.proj_cps k cont in

    let wrap_s fld s =
      let rc = ServerLib.empty_record_constructor in
      let rc = ServerLib.add_field rc fld (ServerLib.wrap_string s) in
      ServerLib.make_record rc
    in
    let wrap_is (i,s) =
      BslNativeLib.opa_tuple_2 (ServerLib.wrap_int i, ServerLib.wrap_string s) in
    let wrap_ss (s1,s2) =
      BslNativeLib.opa_tuple_2 (ServerLib.wrap_string s1, ServerLib.wrap_string s2) in
    let wrap_iss (i1,s2,s3) =
      BslNativeLib.opa_tuple_3 (ServerLib.wrap_int i1, ServerLib.wrap_string s2, ServerLib.wrap_string s3) in
    let wrap_sss (s1,s2,s3) =
      BslNativeLib.opa_tuple_3 (ServerLib.wrap_string s1, ServerLib.wrap_string s2, ServerLib.wrap_string s3) in

    let wrap_sl sl = caml_list_to_opa_list ServerLib.wrap_string sl in

(*  This segfaults...
    let wrap_status (status:ImapClientCore.status) =
      let rc = ServerLib.empty_record_constructor in
      let rc = ServerLib.add_field rc flags (ServerLib.wrap_string status.ImapClientCore.flags) in
      let rc = ServerLib.add_field rc exists (ServerLib.wrap_int status.ImapClientCore.exists) in
      let rc = ServerLib.add_field rc recent (ServerLib.wrap_int status.ImapClientCore.recent) in
      let rc = ServerLib.add_field rc oks (BslNativeLib.caml_list_to_opa_list ServerLib.wrap_string status.ImapClientCore.oks) in
      let rc = ServerLib.add_field rc rwstatus (ServerLib.wrap_string status.ImapClientCore.rwstatus) in
      wrap_opa_email_imap_status (ServerLib.make_record rc)
    in
*)

    let wrap_status (status:ImapClientCore.status) =
      BslNativeLib.opa_tuple_5 (ServerLib.wrap_string status.ImapClientCore.flags,
                                ServerLib.wrap_int status.ImapClientCore.exists,
                                ServerLib.wrap_int status.ImapClientCore.recent,
                                wrap_sl status.ImapClientCore.oks,
                                ServerLib.wrap_string status.ImapClientCore.rwstatus)
    in

    let cont results =
      let results =
        caml_list_to_opa_list
          (fun result ->
             wrap_opa_email_imap_result
               (match result with
                | ImapClientCore.Ok str -> wrap_s ok str
                | ImapClientCore.No str -> wrap_s no str
                | ImapClientCore.Bad str -> wrap_s bad str
                | ImapClientCore.Error str -> wrap_s error str
                | ImapClientCore.SelectResult status ->
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc selectresult (wrap_status status) in
                    ServerLib.make_record rc
                | ImapClientCore.ExamineResult status ->
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc examineresult (wrap_status status) in
                    ServerLib.make_record rc
                | ImapClientCore.NoopResult status ->
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc noopresult (wrap_status status) in
                    ServerLib.make_record rc
                | ImapClientCore.SearchResult il ->
                    let opa_il = caml_list_to_opa_list ServerLib.wrap_int il in
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc searchresult opa_il in
                    ServerLib.make_record rc
                | ImapClientCore.ExpungeResult il ->
                    let opa_il = caml_list_to_opa_list ServerLib.wrap_int il in
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc expungeresult opa_il in
                    ServerLib.make_record rc
                | ImapClientCore.FetchResult issl ->
                    let opa_issl = caml_list_to_opa_list wrap_iss issl in
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc fetchresult opa_issl in
                    ServerLib.make_record rc
                | ImapClientCore.StoreResult isl ->
                    let opa_isl = caml_list_to_opa_list wrap_is isl in
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc storeresult opa_isl in
                    ServerLib.make_record rc
                | ImapClientCore.StatusResult ssl ->
                    let opa_ssl = caml_list_to_opa_list wrap_ss ssl in
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc statusresult opa_ssl in
                    ServerLib.make_record rc
                | ImapClientCore.ListResult sssl ->
                    let opa_sssl = caml_list_to_opa_list wrap_sss sssl in
                    let rc = ServerLib.empty_record_constructor in
                    let rc = ServerLib.add_field rc listresult opa_sssl in
                    ServerLib.make_record rc))
          results
      in
      cont results
    in

    let unwrap_bs value =
      let b1, s2 = BslNativeLib.ocaml_tuple_2 value in
      ServerLib.unwrap_bool b1, ServerLib.unwrap_string s2
    in
    let unwrap_ss value =
      let s1, s2 = BslNativeLib.ocaml_tuple_2 value in
      ServerLib.unwrap_string s1, ServerLib.unwrap_string s2
    in
    let unwrap_bss value =
      let b, s1, s2 = BslNativeLib.ocaml_tuple_3 value in
      ServerLib.unwrap_bool b, ServerLib.unwrap_string s1, ServerLib.unwrap_string s2
    in
    let unwrap_ssss value =
      let s1, s2, s3, s4 = BslNativeLib.ocaml_tuple_4 value in
      ServerLib.unwrap_string s1, ServerLib.unwrap_string s2, ServerLib.unwrap_string s3, ServerLib.unwrap_string s4
    in
    let unwrap_bsss value =
      let b, s1, s2, s3 = BslNativeLib.ocaml_tuple_4 value in
      ServerLib.unwrap_bool b, ServerLib.unwrap_string s1, ServerLib.unwrap_string s2, ServerLib.unwrap_string s3
    in
    let commands =
      opa_list_to_ocaml_list
        (fun command ->
           ServerLib.fold_record
             (fun f value _cmd ->
                let value = Obj.magic(value) in
                match ServerLib.name_of_field f with
                | Some "ImapSelect" -> ImapClientCore.ImapSelect (ServerLib.unwrap_string value)
                | Some "ImapExamine" -> ImapClientCore.ImapExamine (ServerLib.unwrap_string value)
                | Some "ImapNoop" -> ImapClientCore.ImapNoop
                | Some "ImapFetch" -> ImapClientCore.ImapFetch (unwrap_bss value)
                | Some "ImapStore" -> ImapClientCore.ImapStore (unwrap_bsss value)
                | Some "ImapSearch" -> ImapClientCore.ImapSearch (unwrap_bs value)
                | Some "ImapSearchCs" -> ImapClientCore.ImapSearchCs (unwrap_bss value)
                | Some "ImapCopy" -> ImapClientCore.ImapCopy (unwrap_bss value)
                | Some "ImapList" -> ImapClientCore.ImapList (unwrap_ss value)
                | Some "ImapCreate" -> ImapClientCore.ImapCreate (ServerLib.unwrap_string value)
                | Some "ImapDelete" -> ImapClientCore.ImapDelete (ServerLib.unwrap_string value)
                | Some "ImapRename" -> ImapClientCore.ImapRename (unwrap_ss value)
                | Some "ImapStatus" -> ImapClientCore.ImapStatus (unwrap_ss value)
                | Some "ImapAppend" -> ImapClientCore.ImapAppend (unwrap_ssss value)
                | Some "ImapExpunge" -> ImapClientCore.ImapExpunge
                | _ -> assert false)
             (unwrap_opa_email_imap_command command) ImapClientCore.ImapNoop)
        commands
    in

    let client_certificate, verify_params = secure_type in

    let (err_cont:ImapClientCore.ecsa) =
      fun (exn,name,_bt_opt) _runtime sched conn mailbox _ec ->
        ImapClientCore.close_conn sched conn mailbox;
        cont [ImapClientCore.Error (Printf.sprintf "Exception(at %s): %s" name (Printexc.to_string exn))]
    in

    ImapClient.mail_recv ?client_certificate ?verify_params ~secure:true Scheduler.default ~addr ~port
      ~username ~password ~commands
      (cont:ImapClientCore.results -> unit) ~err_cont ();
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
      let t = caml_list_to_opa_list ServerLib.wrap_string email.SmtpServerCore.dests in
      handler f t c (QmlCpsServerLib.cont_ml (
                       fun res -> let i, s = BslNativeLib.ocaml_tuple_2 (BslNativeLib.wrap_opa_tuple_2 (unwrap_opa_tuple_2 res)) in
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

