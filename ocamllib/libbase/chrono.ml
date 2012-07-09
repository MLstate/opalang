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
(* CF mli *)

type t = { start : unit -> unit ; restart : unit -> unit ; stop : unit -> unit ; read : unit -> float ; reset : unit -> unit }
let make () = (* start , stop , read , reset *)
  let c = ref 0. in (* date of the last time you started it *)
  let t = ref 0. in (* cumulated time *)
  let stop = ref true in
  {start = (fun () -> if !stop then (stop := false ; c := Unix.gettimeofday()) else ());
   stop = (fun () -> if !stop then () else (stop := true ; t := !t +. ((Unix.gettimeofday()) -. !c)));
   read = (fun () -> if !stop then !t else !t +. (Unix.gettimeofday()) -. !c);
   reset = (fun () -> stop := true; t := 0.);
   restart = (fun () -> stop := false; t := 0. ; c := Unix.gettimeofday ())}
let start t = t.start ()
let restart t = t.restart ()
let stop t = t.stop ()
let read t = t.read ()
let reset t = t.reset ()
let print t msg =
  let f = t.read () in
  Printf.printf "%s: %.2fs\n%!" msg f;
  t.restart ()

let measure f g =
  let t0=Unix.gettimeofday() in
  let res= f() in
  let t1=Unix.gettimeofday() in
  g (t1-.t0);
  res

let measure_and_show prefix f =
  measure f (fun duration -> Printf.eprintf "%s: %fs\n%!" prefix duration)

let bound timeout f when_timeout =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"));
  ignore (Unix.alarm timeout);
  try
    let r = f() in
    ignore (Unix.alarm 0);
    r
  with
  | Failure "timeout" -> when_timeout()
  | Stack_overflow | Out_of_memory ->
      (* we must clear the still-pending alarm *)
      ignore (Unix.alarm 0);
      when_timeout()
  | e ->
      (* we must clear the still-pending alarm *)
      ignore (Unix.alarm 0);
      raise e
