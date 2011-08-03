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

let write_timeout () =
  let _ = Unix.putenv "MLSTATE_SHOW_LOGS" "1" in
  let scheduler = Scheduler.make () in
(*   let conn_out =  Scheduler.make_connection scheduler (Scheduler.File Unix.stdout) in *)
  let conn_in  =  Scheduler.make_connection scheduler (Scheduler.File Unix.stdin) in
  let block_size = 6 in
    (* WRITE TEST *)
  let count = ref 4000000 in
    Scheduler.write scheduler conn_in ~timeout:1. ~block_size "official message 1\n" ~err_cont:(fun e -> print_string "time's over! 1\n" ; flush stdout;) (fun i -> print_string "normal cont 1\n"; flush stdout; ) ;
    Scheduler.write scheduler conn_in ~timeout:0.00001 ~block_size "official message 2\n" ~err_cont:(fun e -> print_string "time's over! 2\n" ; flush stdout;) (fun i -> print_string "normal cont 2\n"; flush stdout; ) ;
    while (!count > 0) do
      Scheduler.wait scheduler ~block:false ;
      count := !count - 1;
      flush stdout;
    done
 

let _ =
  write_timeout ();

