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
(*
    @author Laurent Le Brun
**)

(* This module provides a way to have secure cookies. Each time a client
 * comes on the webpage, a new cookie is generated, and the old cookie
 * will expire a few seconds later. This way, it's quite difficult to
 *)

(* ic: internal cookie, doesn't change, the server can use it to identify someone *)
(* ec: external cookie, change often, given to the browser *)

(* depends *)
module String = BaseString

let (|>) = InfixOperator.(|>)

(* FIXME: use String.random instead? *)
(* let random() = *)
(*   let randN()= Random.int64 Int64.max_int in *)
(*   (Digest.to_hex (Digest.string( sprintf "%Ld %Ld %f" (randN()) (randN()) (Unix.time ())))) *)

let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let nchars = 26*2 + 10
let cookie_len = 32

let random() =
   Base.String.init cookie_len (fun _ -> chars.[Random.int nchars])


let to_internal = Hashtbl.create 1000
let to_external = Hashtbl.create 1000

let expire_time = ref 50			(* in seconds *)
let set_expire_time t = expire_time := t

(* Remove expired cookies *)
let remove_expired now li =
  List.filter (fun (c, date) ->
    assert (date > 0);
    if date + !expire_time < now then (
      Hashtbl.remove to_internal c;
      false)
    else
      true) li

(* Check if the given external cookie is expired *)
let check_date ic ec =
  try
    let now = truncate (Unix.time()) in
    match Hashtbl.find to_external ic with
    | (fst, _date)::li ->
        assert (_date < 0);     (* negative value means "not expired" *)
        (* Set expire date for the last cookie *)
        let li = (fst, _date) :: remove_expired now li in
        let res = List.exists (fun (c, _) -> c = ec) li in
        Hashtbl.replace to_external ic li;
        if not res then Logger.warning "Invalid cookie! ic:%s ec:%s" ic ec;
        res
    | [] -> assert false
  with Not_found ->
    false

let create() =
  let ic = random() in
  let ec = random() in
  Hashtbl.add to_external ic [];
  Hashtbl.add to_internal ec ic;
  ic

let id_of_cookie str =
(*  let () = prerr_endline str in *)
  let cookies = String.slice ';' str |>
      List.map
      (fun x ->
        let a, b = String.split_char '=' x in
        (String.trim a), b)
  in
(*  let () = List.iter (fun (name, value) -> prerr_endline (Printf.sprintf "%s -> %s" name value)) cookies in *)
  let out = List.fold_left (fun acc (name, val') ->
                              if name = "ec" then Some(val') else acc
                           ) None cookies
    |> Option.default str
  (*in let () = prerr_endline out *)
  in out


(* Check the cookie given by the browser *)
(* Return the internal cookie *)
let get_internal ec =
  let ec = id_of_cookie ec in
  try
    let ic = Hashtbl.find to_internal ec in
    if check_date ic ec then
      ic
    else
      create()
  with Not_found -> create()

let get_internal ec =
  let ec = id_of_cookie ec in
  let res = get_internal ec in
  res

(* Return the external cookie *)
let get_external ic =
  let now = truncate (Unix.time()) in
  let ec = match Hashtbl.find to_external ic with
  | (c, -1) :: li ->
      let rnd = random() in
      let li = (rnd, -1) :: (c, now) :: li in
      Hashtbl.replace to_external ic li;
      Hashtbl.add to_internal rnd ic;
      rnd
  | [] ->
      let rnd = random() in
      Hashtbl.replace to_external ic [rnd, -1];
      Hashtbl.add to_internal rnd ic;
      rnd
  | _ -> assert false
  in
  (Printf.sprintf "ec=%s; path=/" ec), (Printf.sprintf "ic=%s; path=/" ic)

(* Check if internal and external cookies match *)
let check ic ec =
  try Hashtbl.find to_internal ec = ic
  with Not_found -> false
