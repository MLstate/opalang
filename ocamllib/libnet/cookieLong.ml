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

(* Long-term cookies:
 *   - we retain the ic/ec cookie names in order to avoid rewriting OPA's cookie parser.
 *)

let sprintf = Printf.sprintf

module List = Base.List
module String = Base.String

module HST = HttpServerTypes

(* Configuration parameters *)
(* Note: the rotation paramters have not been deleted in case they are needed later. *)
let cookie_gc_period = ref 100
let cookie_pool_size_min = ref 100
let cookie_pool_size_max = ref 10000
let cookie_connection_count = ref 0
let cookie_timer_stop = ref false
let cookie_timer_interval = ref 1
let cookie_gc_blocksize = ref 1
(*let cookie_rotation_period = ref 1
let cookie_rotation_period_max = ref 5
let cookie_rotation_period_ultimate = ref 10*)
let cookie_rotation_connection_rate_max = ref 5.0
(*let cookie_rotation_connection_rate_ultimate = ref 10.0*)
(*let cookie_expires_short = ref (Time.seconds 5)*)
(*let cookie_expires_long = ref (Time.seconds 50)*)
let cookie_dt1 = ref (Time.days 10) (* Variable expiration *)
let cookie_dt2 = ref Time.infinity (* Fixed deadline *)
(*let cookie_max_external_cookies = ref 25*)
(*let cookie_rotate_cookies = ref true*)
let cookies_txt_filename = ref ""
let cookie_accept_client_values = ref false

type expiration_callback = (string -> string -> unit)
let cookie_expiration_callback =
  ref ((fun _ec _ic ->
          #<If$minlevel 10>Logger.debug "cookie_expiration_callback: (ec,ic)=(%s,%s)" _ec _ic#<End>;
          ()
       ):expiration_callback)

let cookie_timer_functions = ref []

(* The hash table *)
(* We now have a single hash table: (ec,ic) -> expiry time *)
let to_longcook = ((Hashtbl.create 1000):((string * string), (Time.t * Time.t)) Hashtbl.t)

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
  #<If$minlevel 10>Logger.debug "return_dead_cookie: %s" str#<End>;
  cookie_dead := str::!cookie_dead; incr cookie_dead_size

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
      (#<If$minlevel 10>Logger.debug "get_cookie_string: pool"#<End>;
       cookie_pool := rest; decr cookie_pool_size; cookie_string)
  | [] -> (#<If$minlevel 10>Logger.debug "get_cookie_string: random"#<End>; random ())

let last_cookie_check_time = ref Time.zero
let cookie_connect_rate = ref 0.0
(*let last_cookie_rotation_period = ref 0*)

let check_cookie_pool now =
  let dT = Time.to_unix_time (Time.difference !last_cookie_check_time now) in
  last_cookie_check_time := now;
  if dT <= 0.0
  then
    (cookie_connection_count := 0;
     (*cookie_rotation_period := 1;*)
     #<If$minlevel 10>Logger.debug "check_cookie_pool: dT=%f <= 0.0" dT#<End>)
  else
    (cookie_connect_rate := (float_of_int !cookie_connection_count) /. dT;
     #<If$minlevel 10>Logger.debug "check_cookie_pool: dT=%f cookie_connection_count=%d cookie_connect_rate=%f"
             dT !cookie_connection_count !cookie_connect_rate#<End>;
     cookie_connection_count := 0;
     (*cookie_rotation_period := (if !cookie_connect_rate <= !cookie_rotation_connection_rate_max
                                then 1
                                else if !cookie_connect_rate <= !cookie_rotation_connection_rate_ultimate
                                then !cookie_rotation_period_max
                                else !cookie_rotation_period_ultimate);*)
     (*if !last_cookie_rotation_period <> !cookie_rotation_period
     then (#<If>Logger.debug "check_cookie_pool: setting cookie rotation to %d" !cookie_rotation_period#<End>;
           last_cookie_rotation_period := !cookie_rotation_period);*)
     if !cookie_pool_size < !cookie_pool_size_min && !cookie_connect_rate <= !cookie_rotation_connection_rate_max
     then
       (let cnt = max !cookie_pool_size_min (!cookie_pool_size_max - !cookie_pool_size) in
        #<If$minlevel 10>Logger.debug "check_cookie_pool: repopulating %d" cnt#<End>;
        populate_cookie_pool cnt))

let update_current_time now =
  let lc = Time.localtime now in
  HST.current_time_string :=
    sprintf "%02d/%02d/%02d:%02d:%02d:%02d %s" lc.Unix.tm_mday (lc.Unix.tm_mon + 1) (lc.Unix.tm_year + 1900)
                                               lc.Unix.tm_hour lc.Unix.tm_min lc.Unix.tm_sec !(HST.time_diff);
  #<If$minlevel 10>Logger.debug "update_current_time: current_time_string=%s" !(HST.current_time_string)#<End>

let check_cookie_timer () =
  if !cookie_timer_stop
  then raise Scheduler.StopTimer
  else
    let now = Time.now () in
    List.iter (fun f -> f now) !cookie_timer_functions

let abbrev s = if String.length s < 5 then s else String.sub s 0 5

let create_aux ic ec hr =
  let dt1 = Time.add hr.HST.hr_timestamp !cookie_dt1 in
  let dt2 = Time.add hr.HST.hr_timestamp !cookie_dt2 in
  Hashtbl.add to_longcook (ec,ic) (dt1,dt2);
  #<If$minlevel 10>Logger.debug "create: add(to_longcook[%d])=%s.%s -> (%7.0f,%7.0f)"
                           (Hashtbl.length to_longcook) (abbrev ic) (abbrev ec)
                           (Time.in_seconds dt1) (Time.in_seconds dt2)#<End>;
  { hr with HST.hr_ec = ec; hr_ic = ic; hr_dt2 = dt2; }

let create hr =
  let ic = get_cookie_string () in
  let ec = get_cookie_string () in
  create_aux ic ec hr

let create_with_client_values hr =
  let ic = hr.HST.hr_ic in
  let ec = hr.HST.hr_ec in
  create_aux ic ec hr

let split_cookie str =
  List.map (fun x -> let a, b = String.split_char '=' x in ((String.trim a), b)) (String.slice ';' str)

let collect_cookies sched (_,expired) =
  List.iteri
    (fun (ec,ic) i ->
       if i mod !cookie_gc_blocksize = 0 then Scheduler.push sched (fun () -> ());
       (!cookie_expiration_callback) ec ic;
       #<If$minlevel 10>Logger.debug "GC(ec,ic): (%s,%s) deleted" ec ic#<End>;
       return_dead_cookie ec;
       return_dead_cookie ic;
       Hashtbl.remove to_longcook (ec,ic)) expired

let gc_cookies sched now =
  #<If$minlevel 20>Logger.debug "gc_cookies"#<End>;
  collect_cookies sched
    (Hashtbl.fold
       (fun ecic (dt1,dt2) (i,expired) ->
          if i mod !cookie_gc_blocksize = 0 then Scheduler.push sched (fun () -> ());
          if now > dt1 || now > dt2 then ((i+1),(ecic::expired)) else ((i+1),expired))
       to_longcook (0,[]))

(* Check the cookie given by the browser *)
(* Return the internal cookie *)
let get_internal hr =
  try
    if String.length hr.HST.hr_ec <> cookie_len || String.length hr.HST.hr_ic <> cookie_len then raise Not_found;
    let hr_ec_ic = hr.HST.hr_ec,hr.HST.hr_ic in
    if !cookie_accept_client_values && not (Hashtbl.mem to_longcook hr_ec_ic) then
      let hr = create_with_client_values hr in
      #<If$minlevel 10>Logger.debug "get_internal: not found but create_with_client_values={ec='%s' ic='%s'}"
                             (abbrev hr.HST.hr_ec) (abbrev hr.HST.hr_ic)#<End>;
      hr
    else
      let (dt1,dt2) = Hashtbl.find to_longcook hr_ec_ic in
      if hr.HST.hr_timestamp > dt1 || hr.HST.hr_timestamp > dt2
      then
        let hr = create hr in
        #<If$minlevel 10>Logger.debug "get_internal: expired new={ec='%s' ic='%s'}"
          (abbrev hr.HST.hr_ec) (abbrev hr.HST.hr_ic)#<End>;
        hr
      else
        (#<If$minlevel 10>Logger.debug "get_internal: found ec=%s ic=%s"
           (abbrev hr.HST.hr_ec) (abbrev hr.HST.hr_ic)#<End>;
         { hr with HST.hr_dt2 = dt2 })
  with Not_found ->
    let hr = create hr in
    #<If$minlevel 10>Logger.debug "get_internal: not found new={ec='%s' ic='%s'}"
                             (abbrev hr.HST.hr_ec) (abbrev hr.HST.hr_ic)#<End>;
    hr

(* Return the external cookie *)
let get_external hr =
  let id = (hr.HST.hr_ec,hr.HST.hr_ic) in
  try
    (* Note that if dt1 = dt2 = infinity then cookies will last forever. *)
    let max_age = Time.min !cookie_dt1 hr.HST.hr_dt2 in
    let dt1 = Time.add hr.HST.hr_timestamp max_age in
    Hashtbl.replace to_longcook id (dt1,hr.HST.hr_dt2);
    #<If$minlevel 10>Logger.debug "get_external(%s.%s) max_age=%f"
                             (abbrev hr.HST.hr_ec) (abbrev hr.HST.hr_ic) (Time.in_seconds max_age)#<End>;
    max_age, "ec="^hr.HST.hr_ec, "ic="^hr.HST.hr_ic
  with
  | Not_found -> Time.zero, "", ""
  | exn -> Logger.warning "CookieLong.get_external: Unknown exception %s" (Printexc.to_string exn); Time.zero, "", ""

let save_cookies () =
  if !cookies_txt_filename <> ""
  then begin
    Logger.info "Saving cookies ...";
    try
      let oc = open_out !cookies_txt_filename in
      Hashtbl.iter (fun (ec,ic) (dt1,dt2) ->
                      Printf.fprintf oc "%s %s (%f) (%f)\n"
                                        ec ic (Time.in_seconds dt1) (Time.in_seconds dt2)) to_longcook;
      close_out oc;
      Logger.info "... saved cookies."
    with exn ->
      Logger.error "save_cookies: exn=%s" (Printexc.to_string exn)
  end

let cookre = Str.regexp "[ \t]*\\([a-z0-9]+\\)[ \t]+\\([a-z0-9]+\\)[ \t]+(\\([0-9.eE+-]+\\))[ \t]+(\\([0-9.eE+-]+\\))"
let load_cookies () =
  if !cookies_txt_filename <> "" && File.exists !cookies_txt_filename
  then begin
    Logger.info "Loading cookies ...";
    let now = Time.now() in
    try
      let ic = open_in !cookies_txt_filename in
      let rec aux () =
        try
          (match input_line ic with
           | "" -> ()
           | str ->
               if String.length str < (cookie_len*2+3)
               then aux ()
               else
                 if Str.string_match cookre str 0
                 then
                   let ec = Str.matched_group 1 str in
                   let ic = Str.matched_group 2 str in
                   let dt1str = Str.matched_group 3 str in
                   let dt2str = Str.matched_group 4 str in
                   if String.length ec = cookie_len && String.length ic = cookie_len
                   then
                     (try
                        let dt1 = Time.of_unix_time_inf (float_of_string dt1str) in
                        let dt2 = Time.of_unix_time_inf (float_of_string dt2str) in
                        if now > dt1 || now > dt2
                        then (#<If$minlevel 10>Logger.debug "expired cookie: ec=%s ic=%s dt1=%7.0f dt2=%7.0f"
                                                       ec ic (Time.in_seconds dt1) (Time.in_seconds dt2)#<End>;
                              ())
                        else (#<If$minlevel 10>Logger.debug "loading cookie: ec=%s ic=%s dt1=%7.0f dt2=%7.0f"
                                                       ec ic (Time.in_seconds dt1) (Time.in_seconds dt2)#<End>;
                              Hashtbl.replace to_longcook (ec,ic) (dt1,dt2));
                        aux ()
                      with | Failure "float_of_string" -> aux ())
                   else aux ()
                 else aux ())
        with | End_of_file -> ()
      in
      aux ();
      close_in ic;
      Logger.info "Loaded cookies.txt file"
    with exn ->
      Logger.error "load_cookies: exn=%s" (Printexc.to_string exn)
  end

let init_cookies ~sched
                 ?(gc_period=100) ?(accept_client_values=false) ?(pool_min=100) ?(pool_max=10000) ?(timer_interval=1)
                 ?(rate_max=5.0) ?(period_max=5) ?(rate_ultimate=10.0) ?(period_ultimate=100)
                 ?(expires_short=Time.seconds 5) ?(expires_long=Time.seconds 50) ?(dt1=Time.days 10) ?(dt2=Time.infinity)
                 ?(max_external_cookies=25) ?(rotate_cookies=true) ?(cookies_filename="")
                 () =
  Random.self_init();
  let _, _, _, _, _, _, _ =
    period_max, rate_ultimate, period_ultimate, expires_short, expires_long, max_external_cookies, rotate_cookies in
  cookie_gc_period := gc_period;
  cookie_accept_client_values := accept_client_values;
  cookie_pool_size_min := pool_min;
  cookie_pool_size_max := pool_max;
  cookie_timer_interval := timer_interval;
  cookie_rotation_connection_rate_max := rate_max;
  (*cookie_rotation_period_max := period_max;
  cookie_rotation_connection_rate_ultimate := rate_ultimate;
  cookie_rotation_period_ultimate := period_ultimate;*)
  (*cookie_expires_short := expires_short;*)
  (*cookie_expires_long := expires_long;*)
  cookie_dt1 := dt1;
  cookie_dt2 := dt2;
  (*cookie_max_external_cookies := max_external_cookies;*)
  (*cookie_rotate_cookies := rotate_cookies;*)
  cookies_txt_filename := cookies_filename;
  last_cookie_check_time := Time.now();
  cookie_timer_stop := false;
  populate_cookie_dead pool_max;
  populate_cookie_pool pool_max;
  cookie_timer_functions := check_cookie_pool::!cookie_timer_functions;
  cookie_timer_functions := update_current_time::!cookie_timer_functions;
  cookie_timer_functions := (gc_cookies sched)::!cookie_timer_functions;
  Scheduler.timer sched (Time.seconds timer_interval) check_cookie_timer;
  if !cookies_txt_filename <> "" then (load_cookies(); at_exit save_cookies)
