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
  Testing module for SchedulerKer
  @author Cedric Soulas
*)
(*
   IMPORTANT:
   Because of side effects, each test is intended to be executed in a distinct processus
*)

module O = SchedulerKer.Operation
module P = SchedulerKer.Priority
module D = SchedulerKer.Descriptor
exception Ok_exception

let print = Logger.notice

module Operation =
struct
  let id1, id2 = Unix.stdin, Unix.stdout
  let operation = O.make ()

  let key n id d =
    let d = match d with
      | O.In -> 0
      | O.Out -> 1
    in
    n * 100 + (Obj.magic id) * 10 + d

  let add n id d =
    let s = Printf.sprintf "(%d, %s) (%d) #%d" (Obj.magic id) (O.direction_to_string d) n (key n id d) in
    print "Add %s" s;
    O.add operation id d (key n id d)(fun () -> print "Process add %s" s) (fun e -> print "Error raised: %s (%s)" (Printexc.to_string e) s)
  let remove n id d =
    print "Remove #%d" (key n id d);
    O.remove operation (key n id d)
  let remove_id id =
    print "Remove id %d" (Obj.magic id);
    O.remove_id operation id
  let process id d =
    print "Processing...";
    O.process operation id d
  let process_id_error id =
    print "Processing id...";
    O.process_id_error operation id
  let process_all l =
    let l = O.process_all operation l in
    assert (l = [])
  let wait () =
    O.wait operation 500

  let test_add () =
    assert ((O.is_empty operation) = true);
    add 1 id1 O.In;
    assert ((O.mem operation id1 O.In) = true);
    (* Simple test *)
    process_all [| (id1, [Epoll.In]) |];
    add 1 id1 O.Out;
    assert ((O.length operation) = 2);
    (* Processing once more is ok *)
    process_all [| (id1, [Epoll.In]) |];
    (* Processing two directions for the same id *)
    process_all [| (id1, [Epoll.In; Epoll.Out]) |];
    add 1 id2 O.In;
    process_all [| (id1, [Epoll.In; Epoll.Out]); (id2, [Epoll.In]) |];
    add 2 id2 O.In;
    (* The two (id2, In) count for 1 *)
    assert ((O.length operation) = 3);
    remove 1 id2 O.In;
    (* The second id2 operation have to be executed *)
    process_all [| (id2, [Epoll.In]) |];
    add 3 id2 O.In;
    (* Still the second id2 *)
    process_all [| (id2, [Epoll.In]) |];
    remove 2 id2 O.In;
    (* Now the third one *)
    process_all [| (id2, [Epoll.In]) |]

  let test_remove () =
    add 1 id1 O.In;
    add 2 id1 O.In;
    (* Process id1, just for In here *)
    print "One Scheduler.Connection_closed will be raised:";
    process_id_error id1 Scheduler.Connection_closed;
    (* Still the first id1 *)
    process_all [| (id1, [Epoll.In]) |];
    remove 1 id1 O.In;
    add 1 id1 O.Out;
    (* Process the second In and the first Out *)
    print "Two Scheduler.Connection_closed will be raised:";
    process_id_error id1 Scheduler.Connection_closed

  let test_exc () =
    (* Raise an exception *)
    begin
      try
        process id1 O.Out;
      with
      | O.Not_found (id, d) ->
          print "OK, exception raised";
          assert (id = id1);
          assert (d = O.Out)
      | e -> raise e
    end;
    add 1 id1 O.In;
    (* This one is ok *)
    process_all [| (id1, [Epoll.In]) |];
    remove 1 id1 O.In;
    (* Raise an exception *)
    begin
      try
        process_all [| (id1, [Epoll.In]) |];
      with
      | O.Not_found (id, d) ->
          print "OK, exception raised";
          assert (id = id1);
          assert (d = O.In)
      | e -> raise e
    end

  let test_wait () =
    (* Nothing to wait for *)
    let a = wait () in
    assert (Array.length a = 0);
    add 1 id2 O.Out;
    add 2 id2 O.Out;
    let a = wait () in
    assert (Array.length a = 1);
    process_all a;
    remove 1 id2 O.Out;
    (* Process the second Out *)
    process_all a;
    let a = wait () in
    assert (Array.length a = 1);
    (* Process the second Out *)
    process_all a;
    add 3 id2 O.Out;
    (* Process the second Out *)
    process_all a;
    remove_id id2;
    let a = wait () in
    assert (Array.length a = 0);
    (* Nothing to process *)
    print "Nothing to process...";
    process_all a;
    print "Nothing to process: OK";
    add 4 id2 O.Out;
    let a = wait () in
    assert (Array.length a = 1);
    (* Process the fourth Out *)
    process_all a;
    (* Remove the last candidate *)
    remove 4 id2 O.Out;
    let a = wait () in
    assert (Array.length a = 0);
    (* Nothing to process *)
    print "Nothing to process...";
    process_all a;
    print "Nothing to process: OK";

end
module Priority =
struct

  let priority = P.make ()
  let add key t =
    print "Add %d" t;
    P.add priority key (Time.milliseconds t) (fun () -> print "Process add %d" t)
  let remove s k =
    P.remove priority k;
    print "%s" s
  let process () =
    Unix.sleep 1;
    (* Little approximation for constant message *)
    let t = ((P.process priority) / 100) * 100 in
    print "Process return %d. Length: %d\n" t (P.length priority)

  let test_add () =
    (* Simple adding test *)
    add 1 500;
    add 2 3500;
    print "";

    process ();
    (* Adding but removing later *)
    add 3 1500;
    print "";

    process ();
    add 4 2500;
    print "";
    remove "Remove add 1500" 3;

    process ();
    (* Adding 0 that have to be called *before* the add 3500 *)
    (* Note: P.Const.priority_max_successive = 1 *)
    add 5 0;
    (* Adding 600 that have to be called *after* the add 3500 (which became urgent) *)
    add 6 600;
    process ();
    process ();
    process ();
    process ();
    process ();
    process ();
    print "Process is_empty: %b" (P.is_empty priority)

  let test_exc () =
    let _ = P.add priority 500 (Time.milliseconds 3) (fun () -> raise Ok_exception) in
    process ()

end

module Descriptor =
struct
  let d = D.make ()
  let id1, id2 = Unix.stdin, Unix.stdout

  let test () =
    assert ((D.length d) = 0);
    assert ((D.is_empty d) = true);
    let key1 = D.add d id1 in
    assert ((D.mem d id1 key1) = D.Alive);
    let key2 = D.add d id1 in
    assert (key1 != key2);
    assert ((D.length d) = 1);
    let _ = D.add d id2 in
    assert ((D.length d) = 2);
    assert ((D.mem d id1 key2) = D.Alive);
    assert ((D.mem d id1 key1) = D.Replaced);
    D.remove d id1;
    assert ((D.mem d id1 key1) = D.Closed);
    assert ((D.mem d id1 key2) = D.Closed);
    print "Test OK"
end
