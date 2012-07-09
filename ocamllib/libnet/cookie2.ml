(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(*
    @author Laurent Le Brun
**)
#<Debugvar:HTTP_DEBUG>

(* This module provides a way to have secure cookies. Each time a client
 * comes on the webpage, a new cookie is generated, and the old cookie
 * will expire a few seconds later. This way, it's quite difficult to
 *)

(* ic: internal cookie, doesn't change, the server can use it to identify someone *)
(* ec: external cookie, change often, given to the browser *)

let sprintf = Printf.sprintf

module List = Base.List
module String = Base.String

module HST = HttpServerTypes

(* Configuration parameters *)
let cookie_gc_period = ref 100
let cookie_pool_size_min = ref 100
let cookie_pool_size_max = ref 10000
let cookie_connection_count = ref 0
let cookie_timer_stop = ref false
let cookie_timer_interval = ref 1
let cookie_rotation_period = ref 1
let cookie_rotation_period_max = ref 5
let cookie_rotation_period_ultimate = ref 10
let cookie_rotation_connection_rate_max = ref 5.0
let cookie_rotation_connection_rate_ultimate = ref 10.0
let cookie_expires_short = ref (Time.seconds 5)
let cookie_expires_long = ref (Time.seconds 50)
let cookie_max_external_cookies = ref 25
let cookie_rotate_cookies = ref true

let cookie_timer_functions = ref []

type ext_entry = (string * Time.t) list

(* The hash tables, int->ext and ext->int *)
let to_internal = ((Hashtbl.create 1000):(string, string) Hashtbl.t);;
let to_external = ((Hashtbl.create 1000):(string, ext_entry) Hashtbl.t);;
let to_resource = ((Hashtbl.create 1000):(string,(unit,unit)ResourceTracker.t) Hashtbl.t)

let chars = "abcdefghijklmnopqrstuvwxyz0123456789"
let nchars = String.length chars
let chars256 = String.init 256 (fun i -> chars.[i mod nchars])
let rand4 () =
  let r = Random.bits () in
  (chars256.[r land 0x7f],
   chars256.[(r lsr 7) land 0x7f],
   chars256.[(r lsr 14) land 0x7f],
   chars256.[(r lsr 21) land 0x7f])
let cookie_len = 32
let random _ = String.init cookie_len (fun _ -> chars.[Random.int nchars])
let randomd str =
  let rec aux = function
    | n when n < 0 -> str
    (*| n -> String.unsafe_set str n (String.unsafe_get chars (Random.int nchars)); aux (n-1)*)
    | 0 ->
        let (c1,_,_,_) = rand4 () in
        String.unsafe_set str 0 c1;
        aux (-1)
    | 1 ->
        let (c1,c2,_,_) = rand4 () in
        String.unsafe_set str 0 c1;
        String.unsafe_set str 1 c2;
        aux (-1)
    | 2 ->
        let (c1,c2,c3,_) = rand4 () in
        String.unsafe_set str 0 c1;
        String.unsafe_set str 1 c2;
        String.unsafe_set str 2 c3;
        aux (-1)
    | n ->
        let (c1,c2,c3,c4) = rand4 () in
        String.unsafe_set str n c1;
        String.unsafe_set str (n-1) c2;
        String.unsafe_set str (n-2) c3;
        String.unsafe_set str (n-3) c4;
        aux (n-4)
  in
  aux (cookie_len - 1)

let cookie_pool = ref ([]:string list)
let cookie_dead = ref ([]:string list)
let cookie_pool_size = ref 0
let cookie_dead_size = ref 0

let get_dead_cookie () =
  match !cookie_dead with
  | str::rest -> cookie_dead := rest; decr cookie_dead_size; str
  | [] -> String.create cookie_len

let return_dead_cookie str =
  #<If>Logger.debug "return_dead_cookie: %s" str#<End>; cookie_dead := str::!cookie_dead; incr cookie_dead_size

let populate_cookie_dead cnt =
  while !cookie_dead_size < cnt do
    cookie_dead := (String.create cookie_len)::!cookie_dead;
    incr cookie_dead_size
  done

let populate_cookie_pool cnt =
  while !cookie_pool_size < cnt do
    cookie_pool := (randomd (get_dead_cookie()))::!cookie_pool;
    incr cookie_pool_size
  done

let get_cookie_string () =
  match !cookie_pool with
  | cookie_string::rest ->
      (#<If>Logger.debug "get_cookie_string: pool"#<End>; cookie_pool := rest; decr cookie_pool_size; cookie_string)
  | [] -> (#<If>Logger.debug "get_cookie_string: random"#<End>; random ())

let last_cookie_check_time = ref Time.zero
let cookie_connect_rate = ref 0.0
let last_cookie_rotation_period = ref 0

let check_cookie_pool now =
  let dT = Time.to_unix_time (Time.difference !last_cookie_check_time now) in
  last_cookie_check_time := now;
  if dT <= 0.0
  then
    (cookie_connection_count := 0;
     cookie_rotation_period := 1;
     #<If>Logger.debug "check_cookie_pool: dT=%f <= 0.0" dT#<End>)
  else
    (cookie_connect_rate := (float_of_int !cookie_connection_count) /. dT;
     #<If$minlevel 10>Logger.debug "check_cookie_pool: dT=%f cookie_connection_count=%d cookie_connect_rate=%f"
             dT !cookie_connection_count !cookie_connect_rate#<End>;
     cookie_connection_count := 0;
     cookie_rotation_period := (if !cookie_connect_rate <= !cookie_rotation_connection_rate_max
                                then 1
                                else if !cookie_connect_rate <= !cookie_rotation_connection_rate_ultimate
                                then !cookie_rotation_period_max
                                else !cookie_rotation_period_ultimate);
     if !last_cookie_rotation_period <> !cookie_rotation_period
     then (#<If>Logger.debug "check_cookie_pool: setting cookie rotation to %d" !cookie_rotation_period#<End>;
           last_cookie_rotation_period := !cookie_rotation_period);
     if !cookie_pool_size < !cookie_pool_size_min && !cookie_connect_rate <= !cookie_rotation_connection_rate_max
     then
       (let cnt = max !cookie_pool_size_min (!cookie_pool_size_max - !cookie_pool_size) in
        #<If>Logger.debug "check_cookie_pool: repopulating %d" cnt#<End>;
        populate_cookie_pool cnt);
    )
let _ = cookie_timer_functions := check_cookie_pool::!cookie_timer_functions

let update_current_time now =
  let lc = Time.localtime now in
  HST.current_time_string :=
    sprintf "%02d/%02d/%02d:%02d:%02d:%02d %s" lc.Unix.tm_mday (lc.Unix.tm_mon + 1) (lc.Unix.tm_year + 1900)
                                               lc.Unix.tm_hour lc.Unix.tm_min lc.Unix.tm_sec !(HST.time_diff);
  #<If$minlevel 10>Logger.debug "update_current_time: current_time_string=%s" !(HST.current_time_string)#<End>
let _ = cookie_timer_functions := update_current_time::!cookie_timer_functions

let check_cookie_timer () =
  if !cookie_timer_stop
  then raise Scheduler.StopTimer
  else
    let now = Time.now () in
    List.iter (fun f -> f now) !cookie_timer_functions

let abbrev s = if String.length s < 5 then s else String.sub s 0 5

let strint () = (Hashtbl.fold (fun k e s -> s^" "^(abbrev k)^" -> "^(abbrev e)^"\n |") to_internal "[|")^"]";;
let pintern () = Logger.debug "%s" (strint()); flush stderr;;

let sp32 = "             ";;
let strexl sep l =
  "["^(String.concat sep (List.map (fun (a,c) ->
                                      Printf.sprintf "(%s,%d)" (abbrev a) (Time.in_milliseconds c)) l))^"]";;
let strext () = (Hashtbl.fold (fun k e s -> s^" "^(abbrev k)^" -> "^(strexl (";\n"^sp32) e)^"\n |") to_external "[|")^"]";;
let pextern () = Logger.debug "%s" (strext()); flush stderr;;

(* Remove expired cookies *)
let remove_expired (now:Time.t) (li:ext_entry) (ec:string) : bool * bool * ext_entry =
  List.fold_right (fun ((c, date) as e) (alt,has_ec,li) ->
                    if date < now
                    then (Hashtbl.remove to_internal c;
                          #<If>Logger.debug "remove_expired(to_internal[%d])=%s" (Hashtbl.length to_internal) (abbrev c)#<End>;
                          (true,has_ec,li))
                    else (alt,(has_ec||c=ec),e::li)) li (false,false,[])

let rth = ResourceTracker.Default.handler
  "Cookie"
  (fun _ _ -> ())
  (fun _ -> None)
  (fun _ _ _ -> (),())
  (fun _ _ _ -> ())

let create ?(expires=Time.seconds 5) now () =
  let ic = get_cookie_string () in
  let ec = get_cookie_string () in
  let l = [(ec,Time.add now expires)] in
  Hashtbl.add to_external ic l;
  #<If>Logger.debug "create: add(to_external[%d])=%s -> %s" (Hashtbl.length to_external) (abbrev ic) (strexl ";" l)#<End>;
  Hashtbl.add to_internal ec ic;
  #<If>Logger.debug "create: add(to_internal[%d])=%s -> %s" (Hashtbl.length to_internal) (abbrev ec) (abbrev ic)#<End>;
  ec, ic

exception Unknown_cookie

let get_resource_tracker ic =
  try Hashtbl.find to_resource ic
  with Not_found ->
    if Hashtbl.mem to_external ic
    then
      let r = ResourceTracker.Default.resource rth () () in
      Hashtbl.add to_resource ic r; r
    else raise Unknown_cookie

let split_cookie str =
  List.map (fun x -> let a, b = String.split_char '=' x in ((String.trim a), b)) (String.slice ';' str)

let collect_cookies (ics,ecs,ic_updts) =
  List.iter (fun ec ->
               #<If>Logger.debug "GC(ec): %s" (abbrev ec)#<End>;
               return_dead_cookie ec;
               Hashtbl.remove to_internal ec) ecs;
  List.iter (fun ic ->
               #<If>Logger.debug "GC(ic): %s" (abbrev ic)#<End>;
               return_dead_cookie ic;
               Hashtbl.remove to_external ic;
    try
      let r = Hashtbl.find to_resource ic in
      ResourceTracker.Default.kill r `Expired
    with Not_found -> ()) ics;
  List.iter (fun (ic,ecs) -> #<If>Logger.debug "GC(ecs): %s(%d)" (abbrev ic) (List.length ecs)#<End>;
                             Hashtbl.replace to_external ic ecs) ic_updts

(* Garbage collect cookies: mark and sweep... *)
let gc_cookies now =
  #<If$minlevel 20>Logger.debug "gc_cookies"#<End>;
  collect_cookies
    (Hashtbl.fold
       (fun ic ecs (ic_exps,ec_exps,ic_updts) ->
          match ecs with
          | [] -> (ic::ic_exps,ec_exps,ic_updts)
          | (ec1,last1)::lst ->
              if now > last1
              then
                let ec_exps = List.fold_left (fun ec_exps (ec,last) -> if now > last then ec::ec_exps else ec_exps)
                                             ec_exps lst in
                (ic::ic_exps,ec1::ec_exps,ic_updts)
              else
                let altered,_,lst = remove_expired now lst "" in
                let ic_updts = if altered then (ic,(ec1,last1)::lst)::ic_updts else ic_updts in
                (ic_exps,ec_exps,ic_updts))
       to_external ([],[],[]))
let _ = cookie_timer_functions := gc_cookies::!cookie_timer_functions

(* Return the internal cookie *)
let get_internal hr =
  let now = Time.now () in
  let ec = hr.HST.hr_ec in
  try
    let ic = Hashtbl.find to_internal ec in
    if ic <> hr.HST.hr_ic then raise Not_found;
    #<If$minlevel 10>Logger.debug "get_internal: ec=%s hr_ec=%s ic=%s hr_ic=%s" ec hr.HST.hr_ec ic hr.HST.hr_ic#<End>;
    (None,(#<If>Logger.debug "get_internal: found ec=%s ic=%s" (abbrev ec) (abbrev ic)#<End>; (hr,true,(ec,ic))))
  with Not_found ->
    let new_ec, new_ic = create now () in
    #<If>Logger.debug "get_internal: not found ec='%s' new_ic='%s'" (abbrev ec) (abbrev new_ic)#<End>;
    let hr = { hr with HST.hr_ec = new_ec; hr_ic = new_ic } in
    (None,(hr,false,(new_ec,new_ic)))

let llast l =
  (* Not TR. *)
  let rec aux =
    function
    | [] -> [], None
    | [x] -> [], Some x
    | h::t -> let l, sx = aux t in h::l, sx
  in
  aux l

let limit_list mx li =
  if List.length li > mx
  then
    match llast li with
    | li2, Some (ec,_) ->
        #<If>Logger.debug "Limit(ec): %s" (abbrev ec)#<End>; return_dead_cookie ec; Hashtbl.remove to_internal ec; li2
    | _, _ -> li
  else li

let cookie_rotation_count = ref 0

(* Return the external cookie *)
let get_external ic =
  try
    let now = Time.now () in
    let rnd = get_cookie_string () in
    let max_age, (ec, ic) =
      match Hashtbl.find to_external ic with
      | [] -> Time.zero, ("", "")
      | (ec,  _) :: li ->
          incr cookie_rotation_count;
          let cookie_rotation_enable = !cookie_rotation_count >= !cookie_rotation_period in
          if cookie_rotation_enable then cookie_rotation_count := 0;
          if !cookie_rotate_cookies && cookie_rotation_enable
          then
            let expires_time = Time.add now !cookie_expires_long in
            let li = (rnd,  expires_time) :: (ec,  expires_time) :: li in
            let li = limit_list !cookie_max_external_cookies li in
            Hashtbl.replace to_external ic li;
            #<If>Logger.debug "get_external(to_external[%d])=%s -> %s"
                         (Hashtbl.length to_external) (abbrev ic)
                         (strexl ";\n                                       " li)#<End>;
            Hashtbl.add to_internal rnd ic;
            #<If>Logger.debug "get_external(to_internal[%d])=%s -> %s"
                         (Hashtbl.length to_internal) (abbrev rnd) (abbrev ic)#<End>;
            !cookie_expires_long, (rnd,ic)
          else
            let expires_time = Time.add now !cookie_expires_long in
            let li = (ec, expires_time) :: li in
            Hashtbl.replace to_external ic li;
            #<If>Logger.debug "get_external(reuse to_internal[%d])=%s -> %s"
                         (Hashtbl.length to_internal) (abbrev ec) (abbrev ic)#<End>;
            !cookie_expires_long, (ec,ic)
    in
    max_age, "ec="^ec, "ic="^ic
  with
  | Not_found -> Time.zero, "", ""
  | exn -> Logger.warning "Cookie2.get_external: Unknown exception %s" (Printexc.to_string exn); Time.zero, "", ""

let init_cookies ~sched
                 ?(gc_period=100) ?(accept_client_values=false) ?(pool_min=100) ?(pool_max=10000) ?(timer_interval=1)
                 ?(rate_max=5.0) ?(period_max=5) ?(rate_ultimate=10.0) ?(period_ultimate=100)
                 ?(expires_short=Time.seconds 5) ?(expires_long=Time.seconds 50) ?(dt1=Time.days 10) ?(dt2=Time.infinity)
                 ?(max_external_cookies=25) ?(rotate_cookies=true) ?(cookies_filename="")
                 () =
  let _, _, _, _ = dt1, dt2, cookies_filename, accept_client_values in
  cookie_gc_period := gc_period;
  cookie_pool_size_min := pool_min;
  cookie_pool_size_max := pool_max;
  cookie_timer_interval := timer_interval;
  cookie_rotation_connection_rate_max := rate_max;
  cookie_rotation_period_max := period_max;
  cookie_rotation_connection_rate_ultimate := rate_ultimate;
  cookie_rotation_period_ultimate := period_ultimate;
  cookie_expires_short := expires_short;
  cookie_expires_long := expires_long;
  cookie_max_external_cookies := max_external_cookies;
  cookie_rotate_cookies := rotate_cookies;
  last_cookie_check_time := Time.now();
  cookie_timer_stop := false;
  populate_cookie_dead pool_max;
  populate_cookie_pool pool_max;
  Scheduler.timer sched (Time.seconds timer_interval) check_cookie_timer
