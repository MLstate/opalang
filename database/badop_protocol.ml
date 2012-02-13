(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(*
    @author Louis Gesbert
**)

module Dialog = Badop_lib.Dialog
module Dialog_aux = Badop_lib.Dialog_aux

(** This file defines types used for message exchange between Badop_server and
    Badop_client. As thus it's shared between the two *)

(* on non-asynchronous cps function, masquerade the result in the return value to get it back *)
let nocps : (('a -> unit) -> unit) -> 'a = fun f ->
  let (r: unit) = f (fun x -> (Obj.magic x : unit)) in
  Obj.magic r

module F
  (Host : sig
     type spoken
     type understood
     type revision
   end)
  =
struct
  type revision = Host.revision

  type tr_version = int

  type 'which read_op = ('which,revision) Badop.generic_read_op

  type 'which internal_write_op = ('which,unit,revision) Badop.generic_write_op

  (* 'transaction is a parameter, because we change it to string when we want to serialise *)
  type ('which,'transaction_channel) poly_transaction_op =
    | Read of Badop.path
        * ('which, tr_version * Dialog.query read_op, Dialog.response read_op Badop.answer) Dialog.t
    | Write of Badop.path * tr_version * 'which internal_write_op
    | WriteList of tr_version * ('which, (Badop.path * Dialog.query internal_write_op) list, unit) Dialog.t
    | Prepare of ('which, tr_version, bool) Dialog.t
    | Commit of ('which, tr_version, bool) Dialog.t
    | Abort of ('which, tr_version, unit) Dialog.t
    | Fork of ('which, tr_version, 'transaction_channel) Dialog.t

  type transaction_channel =
      ((Host.spoken,transaction_channel) poly_transaction_op, (Host.understood,transaction_channel) poly_transaction_op)
        Hlnet.channel

  type 'which transaction_op =
      ('which,transaction_channel) poly_transaction_op

  type transaction = {
    channel : transaction_channel;
    version : tr_version;
    mutable last : bool;
  }

  type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

  type 'which database_query =
    | Transaction of ('which, unit, transaction_channel) Dialog.t
    | Transaction_at of ('which, revision, transaction_channel) Dialog.t
    | Status of ('which, unit, Badop.status) Dialog.t


  (* Just maps on transactions *)
  let map_transaction_op
      : 'which 'transaction1 'transaction2.
        ('transaction1 -> 'transaction2) -> ('which,'transaction1) poly_transaction_op
        -> ('which,'transaction2) poly_transaction_op
      = fun f op ->
    match op with
    | Write (path, v, internal_write_op) -> Write (path, v, internal_write_op)
    | WriteList (v, dialog) -> WriteList (v, dialog)
    | Prepare dialog -> Prepare dialog
    | Read (path, op) -> Read (path, op)
    | Commit op -> Commit op
    | Abort op -> Abort op
    | Fork dialog ->
        let dialog = nocps
          (Dialog_aux.map_dialog ~query:(fun x k -> k x) ~response:(fun tr k -> k (f tr)) dialog)
        in
        Fork dialog

  (* We need to expand this functions even if we use marshal internally, because
     embedded transactions need to be processed through
     [Hlnet.channel_(un)serialise]. Maybe a map on the operation type to bind
     'transaction to string and back just for the transmission would be nicer. *)
  let transaction_op_serialise
      : 'which transaction_op -> string
      = fun op ->
    Marshal.to_string (map_transaction_op Hlnet.serialise_channel op : ('which,string) poly_transaction_op) []
  let rec transaction_op_unserialise
      : ('a,'b) Hlnet.channel -> 'which transaction_op Hlnet.stream_unserialise
      = fun channel s offset ->
    let unserialise_channel tr =
      match Hlnet.unserialise_remote_channel transaction_channel_spec channel tr 0
      with `data (x,_) -> x | _ -> raise Exit
    in
    try
      Hlnet.Aux.map_unserialise (map_transaction_op unserialise_channel) Hlnet.Aux.magic_unserialise
        s offset
    with Exit -> `failure "Bad embedded transaction"
  and transaction_channel_spec
      : (Host.spoken transaction_op, Host.understood transaction_op) Hlnet.channel_spec
      = {
      Hlnet.
        service = Hlnet.make_service_id ~name:"badop/trans" ~version:2;
        out_serialise = transaction_op_serialise;
        in_unserialise = transaction_op_unserialise;
    }

  let database_op_serialise = function
    | Transaction (Dialog.Query ()) -> "\000"
    | Transaction (Dialog.Response transaction) -> "\100" ^ Hlnet.serialise_channel transaction
    | Transaction_at (Dialog.Query rev) -> "\001" ^ Marshal.to_string rev []
    | Transaction_at (Dialog.Response transaction) -> "\101" ^ Hlnet.serialise_channel transaction
    | Status (Dialog.Query ()) -> "\002"
    | Status (Dialog.Response status) -> "\102" ^ Marshal.to_string status []

  let database_op_unserialise channel s offset = match s.[offset] with
    | '\000' -> `data (Transaction (Dialog_aux.make_unsafe_query ()), offset + 1)
    | '\100' ->
        Hlnet.Aux.map_unserialise (fun tr -> Transaction (Dialog_aux.make_unsafe_response tr))
          (Hlnet.unserialise_remote_channel transaction_channel_spec channel)
          s (offset+1)
    | '\001' ->
        Hlnet.Aux.map_unserialise
          (fun (rev:revision) -> Transaction_at (Dialog_aux.make_unsafe_query rev)) Hlnet.Aux.magic_unserialise s (offset+1)
    | '\101' ->
        Hlnet.Aux.map_unserialise (fun tr -> Transaction_at (Dialog_aux.make_unsafe_response tr))
          (Hlnet.unserialise_remote_channel transaction_channel_spec channel)
          s (offset+1)
    | '\002' -> `data (Status (Dialog_aux.make_unsafe_query ()), offset + 1)
    | '\102' ->
        Hlnet.Aux.map_unserialise (fun st -> Status (Dialog_aux.make_unsafe_response st))
          (Hlnet.Aux.magic_unserialise)
          s (offset+1)
    | _ -> `failure "Bad database message"

  type database = (Host.spoken database_query, Host.understood database_query) Hlnet.channel
  let database_channel_spec
      : (Host.spoken database_query, Host.understood database_query) Hlnet.channel_spec
      = {
      Hlnet.
        service = Hlnet.make_service_id ~name:"badop/db" ~version:2;
        out_serialise = database_op_serialise;
        in_unserialise = database_op_unserialise;
    }
end
