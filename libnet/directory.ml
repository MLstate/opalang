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
open Cps.Ops

module Hashtbl = Base.Hashtbl

#<Debugvar:HLDIR_DEBUG>

(* ********************************************************)
(* DEFINE TYPE FOR HLNET PROTOCOL *************************)
type kind = Dir | Loc

type who = Me | Other of Unix.inet_addr

type iwho = IMe | IYou | IOther of Unix.inet_addr

(* Types for [kind=Dir] *)
type ('key, 'value) request =
  | FindOrReplace of ('key * 'value)
  | Add of ('key * 'value)
  | Find of 'key
  | All of 'key
  | Remove of 'key

type ('key, 'value) dir_request_channel =
    (('key, 'value) request, ('value * iwho) list) Hlnet.channel

type ('key, 'value) dir_response_channel =
    (('value * iwho) list, ('key, 'value) request) Hlnet.channel

(* Types for [kind=Loc] *)

type loc_channel = (Unix.inet_addr, Unix.inet_addr) Hlnet.channel

(* Type of a directory. *)

type ('key, 'value) sdirectory = {
  sched : Scheduler.t;
  mutable k_myaddr : (Unix.inet_addr -> unit) list;
  mutable myaddr : Unix.inet_addr option;
  htbl : ('key, 'value * who) Hashtbl.t;
}

type ('key, 'value) cdirectory = {
  mutable cmyaddr : Unix.inet_addr option;
  chan : ('key, 'value) dir_request_channel;
}

type ('key, 'value) t =
  | Server of ('key, 'value) sdirectory
  | Client of ('key, 'value) cdirectory



(* ********************************************************)
(* CONVERSION FUNCTIONS ***********************************)
let endpoint_to_inet = function
    Hlnet.Tcp (x, _) | Hlnet.Ssl (x, _, _) (* | Hlnet.Udp (x, _) *) -> x

let inet_of_channel c =
  endpoint_to_inet (Hlnet.remote_of_channel c)

let who_to_iwho (response_channel:('a, 'b)dir_response_channel) who =
  let response_addr = inet_of_channel response_channel in
  match who with
  | Me -> IMe
  | Other addr when addr = response_addr -> IYou
  | Other x -> IOther x

let iwho_to_who (request_channel:('a, 'b)dir_request_channel) iwho =
  match iwho with
  | IMe -> Other (inet_of_channel request_channel)
  | IYou -> Me
  | IOther x -> Other x

let list_to_opt chan = function
  | [] -> None
  | [(v,x)] -> Some (v, iwho_to_who chan x)
  | _ -> failwith("list_to_opt : Unexpected list")

let opt_to_list chan = function
  | None -> []
  | Some (v, x) -> ([v, who_to_iwho chan x] : ('a * iwho) list)

let list_to_ilist chan list =
  List.map
    (function (v,x) -> v, who_to_iwho chan x)
    list

let ilist_to_list chan list =
  List.map
    (function (v,x) -> v, iwho_to_who chan x)
    list

let who_to_string = function
  | Me -> "Me"
  | Other x -> Unix.string_of_inet_addr x



(* ********************************************************)
(* ********************************************************)
module ExtendHash = struct
  let find_or_replace htbl key value =
    match Hashtbl.find_opt htbl key with
    | None ->
        Hashtbl.add htbl key value;
        None
    | x -> x
end

let rec make ?(err_cont=fun _ ->
                 #<If> Logger.warning "[DIRECTORY] Make : Uncaught exn"
                 #<Else> ()
                 #<End>)
    sched endpoint path kind k =
  let dir_service = Hlnet.make_service_id ~name:(Printf.sprintf "dir/%s" path) ~version:1 in
  match kind with
  | `client ->
      #<If>
        Logger.debug "[DIRECTORY] Try to create a client directory (%s@ %s)"
        (Hlnet.print_service_id dir_service) (Hlnet.endpoint_to_string endpoint)
      #<End>;
      Hlnet.open_channel sched ~on_disconnect:(fun () -> err_cont Exit; `retry (Time.seconds 5))
        endpoint (Hlnet.Aux.magic_spec dir_service)
      @> fun chan -> Client {chan = chan; cmyaddr = None} |> k
  | `server ->
      #<If>
        Logger.debug "[DIRECTORY] Try to create a server directory (%s)"
        (Hlnet.endpoint_to_string endpoint)
      #<End>;
      let server = {
        sched = sched;
        k_myaddr = [];
        htbl = Hashtbl.create 1024;
        myaddr = None;
      } in
      (try
         let safe = true in
         Hlnet.accept ~safe sched endpoint (Hlnet.Aux.magic_spec dir_service)
           (fun (hlchan : ('key, 'value) dir_response_channel) ->
              Hlnet.setup_respond hlchan
                (fun request k ->
                   match request with
                   | FindOrReplace (key, value) ->
                       let addr = inet_of_channel hlchan in
                       let res =
                         ExtendHash.find_or_replace server.htbl key
                           (value, Other addr) in
                       opt_to_list hlchan res |> k
                   | Add (key, value) ->
                       let addr = inet_of_channel hlchan in
                       Hashtbl.add server.htbl key (value, Other addr)
                   | Find key ->
                       let res = Hashtbl.find_opt server.htbl key in
                       opt_to_list hlchan res |> k
                   | All key ->
                       list_to_ilist hlchan (Hashtbl.find_all server.htbl key) |> k
                   | Remove key ->
                       Hashtbl.remove server.htbl key
                )
           );
         Hlnet.accept ~safe sched endpoint
           (Hlnet.Aux.magic_spec (Hlnet.make_service_id ~name:"dir/localize" ~version:1))
           (fun (hlchan : loc_channel) ->
              Hlnet.setup_respond hlchan
                (fun myaddr k ->
                   server.myaddr <- Some myaddr;
                   List.iter
                     (fun k -> Scheduler.push server.sched (fun () -> k myaddr))
                     server.k_myaddr;
                   server.k_myaddr <- [];
                   k (inet_of_channel hlchan))
           );
         Server server |> k
       with e -> err_cont e)

let add t key value =
  match t with
  | Client r -> Hlnet.send r.chan (Add (key, value))
  | Server r -> Hashtbl.add r.htbl key (value, Me)

let find_or_replace t key value k =
  match t with
  | Client r ->
      Hlnet.sendreceive r.chan (FindOrReplace (key, value)) @>
        (function res -> list_to_opt r.chan res |> k)
  | Server r ->
      (ExtendHash.find_or_replace r.htbl key (value, Me)) |> k

let find_opt t key k =
  match t with
  | Client r ->
      Hlnet.sendreceive r.chan (Find key)
        (function res -> list_to_opt r.chan res |> k)
  | Server r ->
      Hashtbl.find_opt r.htbl key |> k

let find_all t key k =
  match t with
  | Client r ->
      Hlnet.sendreceive r.chan (All key)
        (function l -> k (ilist_to_list r.chan l))
  | Server r ->
      Hashtbl.find_all r.htbl key |> k

let remove t key =
  match t with
  | Client r -> Hlnet.send r.chan (Remove key)
  | Server r -> Hashtbl.remove r.htbl key

let my_public_addr_opt t k =
  match t with
  | Client r ->
      (match r.cmyaddr with
       | Some _ as a -> k a
       | None ->
           let endpoint = Hlnet.remote_of_channel r.chan in
           let addr = endpoint_to_inet endpoint in
           let sched = Hlnet.scheduler_of_channel r.chan in
           Hlnet.open_channel sched endpoint
             (Hlnet.Aux.magic_spec (Hlnet.make_service_id ~name:"dir/localize" ~version:1))
           @> (function (chan : loc_channel) ->
                Hlnet.sendreceive chan addr @> function a ->
                  r.cmyaddr <- Some a; r.cmyaddr |> k))
  | Server r -> r.myaddr |> k

let my_public_addr t k =
  match t with
  | Client _ -> my_public_addr_opt t @> (fun x -> Option.get x |> k)
  | Server r ->
      (match r.myaddr with
       | Some x -> x |> k
       | None -> r.k_myaddr <- k::r.k_myaddr)
