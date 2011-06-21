(*
    Copyright © 2011 MLstate

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
(** implements http://www.rfcsearch.org/rfcview/RFC/821.html
    @author Henri Binsztok *)

module String = Base.String
module List = Base.List
let (|>) = InfixOperator.(|>)
let sprintf = Printf.sprintf

 (** communication type for statistics *)
let smtp = NetAddr.mk_protocol "SMTP"

type email = string

(** SMTP envelope *)
type envel = { mfrom : email
             ; mto : email list
             ; mdata : string
             ; mquit : bool
             ; datamode : bool
             }


let resolve_UNIX name =
  try
    (Unix.gethostbyname name).Unix.h_addr_list.(0)
     |> Unix.string_of_inet_addr
     |> String.split ((=) '.')
     |> List.map int_of_string
     |> function [a;b;c;d] -> Some (a,b,c,d) | _ -> None
  with Unix.Unix_error _ -> None

let resolve_additional r n =
  let rec aux = function
    | hd :: tl ->
        if hd.Dig.domain = n then
          match hd.Dig.dst with
          | Dig.Ip i -> Some i
          | _ -> aux tl
        else aux tl
    | _ -> resolve_UNIX (List.fold_left (fun acc x -> sprintf "%s.%s" acc x) (List.hd n) (List.tl n))
  in
  aux (List.assoc "ADDITIONAL" r)


external get_mx_dns : string -> (string * int) array = "get_mx_dns"

let get_mx name : string list =
  let arr = get_mx_dns name in
  Array.sort (fun x y -> compare (snd x) (snd y)) arr;
  arr
   |> Array.to_list
   |> List.map fst

let resolve_mx name =
  let output = File.process_output (sprintf "dig %s MX" name) in
  try
    let _pos, r = Dig.parse_dig_dig output in
      List.assoc "ANSWER" r
      |> List.filter_map (fun x ->
          match x.Dig.category with
          | Dig.Mx pri -> Some (pri, x.Dig.dst)
          | _ -> None)
      |> List.sort (fun (pri1, _) (pri2, _) -> compare pri1 pri2)
      |> List.filter_map (function
          | (_, Dig.Ip i) -> Some i
          | (_, Dig.Name n) ->
              if List.mem_assoc "ADDITIONAL" r then resolve_additional r n
              else resolve_UNIX (String.concat "." n)
          )
  with _ ->
    []


let empty = { mfrom = "" ; mto = [] ; mdata = "" ; mquit = false ; datamode = false}

let make_email mfrom mto mdata =
  { mfrom = mfrom
  ; mto = mto
  ; mdata = mdata
  ; mquit = true
  ; datamode = true }

let errors = [
  (** Temporary *)
  (* This may be a reply to any command if the service knows it must shut down *)
  (421, "Service not available, closing transmission channel");
  (* E.g., mailbox busy *)
  (450, "Requested mail action not taken: mailbox unavailable");
  (451, "Requested action aborted: local error in processing");
  (452, "Requested action not taken: insufficient system storage");

  (** Permanent *)
  (* This may include errors such as command line too long *)
  (500, "Syntax error, command unrecognized") ;
  (501, "Syntax error in parameters or arguments") ;
  (502, "Command not implemented") ;
  (503, "Bad sequence of commands") ;
  (504, "Command parameter not implemented") ;
  (* E.g., mailbox not found, no access *)
  (550, "Requested action not taken: mailbox unavailable") ;
  (551, "User not local; please try") ;
  (552, "Requested mail action aborted: exceeded storage allocation") ;
  (* E.g., mailbox syntax incorrect *)
  (553, "Requested action not taken: mailbox name not allowed") ;
  (554, "Transaction fail") ]

let others = [
  (211, "System status, or system help reply") ;
  (* Information on how to use the receiver or the meaning of a particular non-standard command; this reply is useful only to the human user *)
  (214, "Help message") ;
  (220, "Service ready SMTP HMS (MLstate Mail Server)") ;
  (221, "Service closing transmission channel") ;
  (250, "Ok") ;
  (251, "User not local; will forward to") ;
  (354, "Start mail input; end with . (a dot)") ]

let error n = sprintf "%d Error: %s" n (List.assoc n errors)
let msg n = sprintf "%d %s" n (List.assoc n others)
let crlf = "\r\n"

(* Commands required by RFC 821 *)

(** Identify the SMTP sender to the SMTP receiver *)
let helo e hostname =
  e, "250 Hello " ^ hostname

(** Set the envelope return path (sender) and clear the list of envelope recipient addresses *)
let mail e address =
  { e with mfrom=address ; mto = [] }, msg 250

(** Add one address to the list of envelope recipient addresses *)
let default_valid_emails = ["contact" ; "henri" ; "henri.binsztok" ; "hb"]
let default_validate a = List.mem a default_valid_emails

let id address =
  let re = Str.regexp ".*<\\([^@]+\\).+" in
  if Str.string_match re address 0 then Str.replace_matched "\\1" address
  else address

let rcpt ?(validate=default_validate) e address =
  if e.mfrom = "" then e, error 503
  else
    if validate (id address) then
      { e with mto = address::e.mto }, msg 250
    else (* Attention, anti-spam, non valide / RFC *)
      { e with mquit=true }, error 553

(** Consider the lines following the command to be e-mail from the sender *)
let data e =
  if e.mto = [] then e, error 503
  else { e with datamode=true }, msg 354

(** Reset the envelope *)
let rset _e = empty, msg 250

(** Ask the receiver to send a valid reply (but specify no other action) *)
let noop e = e, msg 250

(** Ask the receiver to send a valid reply, and then close the transmission channel *)
let quit e = {e with mquit=true}, msg 221

(* Evaluation *)
let zero_arg e f arg = if arg=[] then f e else e, error 501
let one_arg e f = function [arg] -> f e arg | _ -> e, error 501
let two_arg e f test = function
  | [t2; arg] when (String.uppercase t2=test) -> f e arg
  | _ -> e, error 501

let eval e s =
  let re = Str.regexp "[ :\n\r]+" in
  match Str.split re s with
  | command::arg ->
      begin match String.uppercase command with
      | "HELO" -> one_arg e helo arg
      | "MAIL" -> two_arg e mail "FROM" arg
      | "RCPT" -> two_arg e rcpt "TO" arg
      | "DATA" -> zero_arg e data arg
      | "RSET" -> zero_arg e rset arg
      | "NOOP" -> zero_arg e noop arg
      | "QUIT" -> zero_arg e quit arg
      | _ -> e, error 502 end
  | _ -> e, error 500

(* Dialog *)

let read_line (sched: Scheduler.t) conn cont =
  let rec retry buf =
   Scheduler.read_more sched conn buf ~timeout:(Time.seconds 300) (fun (_, buf) ->
      let str = FBuffer.contents buf in
      if not (String.is_contained "\r\n" str) then
        retry buf
      else (
        Logger.debug "<<< %s" str;
        cont str
      )
    )
  in retry (FBuffer.make 0)

let write_line (sched: Scheduler.t) str conn cont =
  Logger.debug ">>> %s" str;
  Scheduler.write sched conn (str ^ crlf) cont

let mail_recv save_mail conn (sched: Scheduler.t) cont =
  let send s cont =
    Logger.debug "OUT: %s" s;
    write_line sched s conn cont in
  let rec f e cont =
    if e.mquit then ()
    else
      read_line sched conn (fun inp ->
      Logger.debug "IN: %s" inp;
      if e.datamode then
        if Str.string_match (Str.regexp "^\\.[\r\n]+") inp 0 then
          begin save_mail e ;
            send (msg 250 ^ ": queued as " ^ string_of_int (Random.int max_int)) (fun _ ->
            f empty cont) end
        else f { e with mdata=e.mdata ^ inp ^ "\n" } cont
      else
        let ne, out = eval e inp in
        send out (fun _ -> f ne cont)
      ) in
  try
      send (msg 220) (fun _ ->
      f empty cont )
  with
    _ -> cont()

let read_code s =
  let get i = int_of_char (String.unsafe_get s i) - 48 in
  let l = String.length s in
  if l > 3 then 100 * get 0 + 10 * get 1 + get 2, String.sub s 4 (4 - 3)
  else 0, "unknown server answer"

exception Bad_address of string
exception Unknown_address of string

let simple_mail s =
  try
    let _, (_, (user, domain)) = Email.parse_email_email s in
    sprintf "%s@%s" user domain
  with _ -> raise (Bad_address s)

let valid_email s =
  try
    ignore (Email.parse_email_email s);
    true
  with _ -> false

module MailSend =
struct

  (* Error_MX = can't connect to the MX server, let's try another one *)
  type mail_res = Ok | Error | Error_MX | Delayed of int

  let analyze_error = Mailerror.parse_mailerror_error

  let mail_send_fun_aux (sched: Scheduler.t) domain mfrom mto mdata back_fun attempt conn cont =
    let wait_and_retry x mdata attempt cont =
      Scheduler.remove_connection sched conn;
      ignore(Scheduler.sleep sched x (fun () ->
        back_fun mdata (succ attempt) cont))
    in
    let send expect s cont =
      write_line sched s (conn: Scheduler.connection_info) (fun _ ->
      let rec aux res code cont =
          if code = 220 then
            read_line sched conn (fun res ->
            let code, _ = read_code res in
            aux res code cont)
          else
            cont (res, code)
      in
      aux "" 220 (fun (res, code) ->
      if List.mem code expect then cont None
      else cont (Some res)))
    in
    let dialog_list =
      [ (sprintf "HELO %s" domain, [250])
      ; (sprintf "MAIL FROM:<%s>" (simple_mail mfrom), [250])
      ; (sprintf "RCPT TO:<%s>" (simple_mail mto), [250])
      ; ("DATA", [354])
      ; (mdata, [250])
      ; ("QUIT", [221])
      ] in
    let rec aux x cont = match x with
      | (message, expected) :: tl ->
          begin

            send expected message (function
            | None -> aux tl cont
            | Some err ->
                begin
                  Logger.debug "mail_send_fun_new error : %s" err;
                  try
                    let _pos, res = analyze_error err in
                    match res with
                    | Mailerror.GreylistedSec x ->
                        let x = if x < 90 then 90 else x in
                        Logger.debug "::: greylisted (%d secs)" x;
                        wait_and_retry (Time.seconds x) mdata (succ attempt) cont
                    | Mailerror.GreylistedMin x ->
                        Logger.debug "::: greylisted (%d mins)" x;
                        let x = x * 60 in
                        wait_and_retry (Time.seconds x) mdata (succ attempt) cont
                    | Mailerror.Add_cc s ->
                        wait_and_retry (Time.seconds 1) (sprintf "Cc: %s\r\n%s" s mdata) (succ attempt) cont
                    | _ when fst (read_code err) = 451 ->
                        let x = 60 * attempt * attempt in
                        Logger.debug "::: waiting (%d sec)" x;
                        wait_and_retry (Time.seconds x) mdata (succ attempt) cont
                    | _ -> cont Error
                  with _ -> cont Error
                end)
          end
      | _ -> cont Ok
    in
    read_line sched conn (fun sr ->
    if fst (read_code sr) = 220 then
      aux dialog_list cont
    else
      let error () =
        Logger.debug "couldn't initiate server dialog";
        cont Error_MX
      in
      try
        let _pos, res = analyze_error sr in
        match res with
        | Mailerror.GreylistedSec x ->
            let x = max x 60 in
            Logger.debug "::: greylisted (%d secs)" x;
            Logger.debug "waiting %d secs" x;
            wait_and_retry (Time.seconds x) mdata (succ attempt) cont
        | Mailerror.GreylistedMin x ->
            Logger.debug "::: greylisted (%d mins)" x;
            let x = x * 60 in
            Logger.debug "waiting %d secs" x;
            wait_and_retry (Time.seconds x) mdata (succ attempt) cont
        | Mailerror.Add_cc s ->
            wait_and_retry Time.zero (sprintf "Cc: %s\r\n%s" s mdata) (succ attempt) cont
        | _ -> error ()
      with _ -> error ()
    )

  let mail_send_fun_new (sched: Scheduler.t) domain mfrom mto mdata back_fun attempt conn cont =
      let rec aux acc =
      try
          mail_send_fun_aux (sched: Scheduler.t) domain mfrom mto mdata back_fun attempt conn cont
      with | Unix.Unix_error(_,"recv","") as err ->
                 if acc >= 10 then raise err
                 else aux (acc + 1)
      in aux 0

end

let split_email s =
  try
    let _, (_, user_domain) = Email.parse_email_email s in
    user_domain
  with _ -> raise (Bad_address s)

exception Too_much_try

let full_email mfrom mto mdata =
    sprintf "From: %s\r\nTo: %s\r\nMessage-ID: <%s.%s>\r\nX-Mailer: MLstate mailserve\r\n%s\r\n."
    mfrom mto (String.random 10) mfrom mdata

let mail_send (sched: Scheduler.t) mfrom mto mdata attempt cont =
  let mdata = full_email mfrom mto mdata in
  let _user_from, domain_from = split_email mfrom
  and _user_to, dst = split_email mto in
  let ip_list = resolve_mx dst in
  let rec try_mx ip_list mdata attempt cont =
    match ip_list with
    | [] ->
        Logger.warning "No working MX server found - can't send mail to %s" mto;
        cont MailSend.Error
    | _ when attempt >= 10 -> cont MailSend.Error
    | dst_ip :: mx_servers ->
        let addr = Network.addr_of_ipv4 dst_ip in
        let port_spec = Network.make_port_spec ~protocol:smtp addr 25 in
        let connect_cont conn =
          let rec retry_fun mdata attempt cont =
            MailSend.mail_send_fun_new sched domain_from mfrom mto mdata (try_mx ip_list) attempt conn
              (function
               | MailSend.Error_MX -> Scheduler.remove_connection sched conn; try_mx mx_servers mdata attempt cont
               | res -> Scheduler.remove_connection sched conn; cont res)
          in
          retry_fun mdata attempt cont
        in
        Network.connect sched port_spec Network.Unsecured connect_cont
  in
  try_mx ip_list mdata attempt cont

let mail_content ?(charset="ISO-8859-1") subject body =
  sprintf "Content-Type: text/plain; charset=%s\r\nSubject: %s\r\n\r\n%s\r\n" charset subject body
