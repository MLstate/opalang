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
let basic_std_test () =
  let _ = Unix.putenv "MLSTATE_SHOW_LOGS" "1" in
  let scheduler = Scheduler.make () in
  let conn_out =  Scheduler.make_connection scheduler (Scheduler.File Unix.stdout) in
  let conn_in  =  Scheduler.make_connection scheduler (Scheduler.File Unix.stdin) in
    (*   let scheduler = Scheduler.remove_connection scheduler conn_out in *)
    (*   let scheduler = Scheduler.remove_connection scheduler conn_in in *)
  let block_size = 6 in
    (* WRITE TEST *)
  let write_callback n pos =
    Printf.printf "Write %d ended at pos %d\n" n pos;
    flush stdout
  in
  let count = ref 0 in
  let get_count () = incr count; !count in
  let test_write s = 
    Scheduler.write scheduler conn_in ~block_size s (write_callback (get_count ())) 
  in
    (* READ TEST *)
  let rec test_read () = 
    Scheduler.read scheduler conn_out ~block_size (read_callback (get_count ()))
  and read_callback n (pos, buff) =
    let v = FBuffer.contents buff in
      Printf.printf "Read %d ended at pos %d with value ->%s<-\n" n pos v;
      flush stdout;
      test_read ();
  in
  let buf = " ** test stdout ** " in
  let _ =
    (try
       let n = Unix.write Unix.stdout buf 0 (String.length buf) in
         print_string "(length =="; print_int n; print_string ")"; print_newline ()
     with
       | Unix.Unix_error (e, _, _) -> print_string (Unix.error_message e)
       | _ -> print_string "Unix.write fatal error");
    flush stdout;
    (    
      test_write "first-write\n";
      test_read (); (* Read and write at the same time now works *)
      test_write "second-write-very-very-very-long\n"; (* several write at the same time now works *)
      test_write "third\n");
    (try
       test_read ();
       assert false; (* the second read registration HAS to raise the busy exception *)
     with
       | Scheduler.Busy_direction -> print_string "Ok, second read not permitted\n"; flush stdout
    );   
    Scheduler.sleep scheduler (-1.) (fun () -> print_string "Waked up after a sleep -1"; print_newline());
    Scheduler.sleep scheduler 0. (fun () -> print_string "Waked up after a sleep 0"; print_newline());
    Scheduler.sleep scheduler 1.42 (fun () -> print_string "Waked up after a sleep 1.42"; print_newline());
    Scheduler.sleep scheduler 2.84 (fun () -> print_string "Waked up after a sleep 2.84"; print_newline());
    Scheduler.timer scheduler (5.) (fun () -> print_string "timer..."; print_newline());
  in
    (* I bet that counting to 100,000 takes more than 5 sec. *)
  let count = ref 10000000 in
    while (!count > 0) do
      Scheduler.wait scheduler ~block:false ;
      count := !count - 1;
      flush stdout;
    done
      
    
let create_remove_epoll_fails () =
  let scheduler = Scheduler.make () in
  let my_file = 
    Unix.openfile "/tmp/my_file.test_scheduler.mlstate" [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o600 
  in
  let conn =  Scheduler.make_connection scheduler (Scheduler.File my_file) in
    Scheduler.remove_connection scheduler conn
      
let remove_unexisting () =
  let scheduler = Scheduler.make () in
  let my_file = 
    Unix.openfile "/tmp/my_file.test_scheduler.mlstate" [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o600 
  in
  let conn =  Scheduler.make_connection scheduler ~register:false (Scheduler.File my_file) in
    Scheduler.remove_connection scheduler conn



let through_files () =
  let scheduler = Scheduler.make () in
  let socket_in = 
    Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let socket_out = 
    Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn_out =  
    Scheduler.make_connection scheduler (Scheduler.Normal (socket_out, Unix.inet_addr_loopback)) 
  in
  let conn_in  =  
    Scheduler.make_connection scheduler (Scheduler.Normal (socket_in, Unix.inet_addr_loopback)) 
  in
    (*   let scheduler = Scheduler.remove_connection scheduler conn_out in *)
    (*   let scheduler = Scheduler.remove_connection scheduler conn_in in *)
  let block_size = 6 in
    (* WRITE TEST *)
  let write_callback n pos =
    Printf.printf "Write %d ended at pos %d\n" n pos;
    flush stdout
  in
  let count = ref 0 in
  let get_count () = incr count; !count in
  let test_write s = 
    Scheduler.write scheduler conn_in ~block_size s (write_callback (get_count ())) 
  in
    (* READ TEST *)
  let rec test_read () = 
    Scheduler.read scheduler conn_out ~block_size (read_callback (get_count ()))
  and read_callback n (pos, buff) =
    let v = FBuffer.contents buff in
      Printf.printf "Read %d ended at pos %d with value ->%s<-\n" n pos v;
      flush stdout;
      test_read ();
  in
  let _ =
    (    
      test_write "first-write\n";
      test_read (); (* Read and write at the same time now works *)
      test_write "second-write-very-very-very-long\n"; (* several write at the same time now works *)
      test_write "third\n");
    (try
       test_read ();
       assert false; (* the second read registration HAS to raise the busy exception *)
     with
       | Scheduler.Busy_direction -> print_string "Ok, second read not permitted\n"; flush stdout
    );   
    Scheduler.sleep scheduler (-1.) (fun () -> print_string "Waked up after a sleep"; print_newline());
    Scheduler.sleep scheduler 0. (fun () -> print_string "Waked up after a sleep"; print_newline());
    Scheduler.sleep scheduler 1.42 (fun () -> print_string "Waked up after a sleep"; print_newline());
    Scheduler.sleep scheduler 2.84 (fun () -> print_string "Waked up after a sleep"; print_newline());
  in
    Scheduler.remove_connection scheduler conn_out;
    Scheduler.remove_connection scheduler conn_in


let simple_socket () =
  let scheduler = Scheduler.make () in
  let socket_in = 
    Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let socket_out = 
    Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn_out =  
    Scheduler.make_connection scheduler (Scheduler.Normal (socket_out, Unix.inet_addr_loopback)) 
  in
  let conn_in  =  
    Scheduler.make_connection scheduler (Scheduler.Normal (socket_in, Unix.inet_addr_loopback)) 
  in
  let block_size = 6 in
    (* WRITE TEST *)
  let write_callback n pos =
    Printf.printf "Write %d ended at pos %d\n" n pos;
    flush stdout
  in
  let count = ref 0 in
  let get_count () = incr count; !count in
  let test_write s = 
    Scheduler.write scheduler conn_in ~block_size s (write_callback (get_count ())) 
  in
    (* READ TEST *)
  let rec test_read () = 
    Scheduler.read scheduler conn_out ~block_size (read_callback (get_count ()))
  and read_callback n (pos, buff) =
    let v = FBuffer.contents buff in
      Printf.printf "Read %d ended at pos %d with value ->%s<-\n" n pos v;
      flush stdout;
      test_read ();
  in
  let _ =
    (    
      test_write "first-write\n";
      test_read (); (* Read and write at the same time now works *)
      test_write "second-write-very-very-very-long\n"; (* several write at the same time now works *)
      test_write "third\n");
    (try
       test_read ();
       assert false; (* the second read registration HAS to raise the busy exception *)
     with
       | Scheduler.Busy_direction -> print_string "Ok, second read not permitted\n"; flush stdout
    );   
  in
    Scheduler.remove_connection scheduler conn_out;
    Scheduler.remove_connection scheduler conn_in

let with_read_all () =
  let scheduler = Scheduler.make () in
  let socket_in = 
    Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let socket_out = 
    Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn_out =  
    Scheduler.make_connection scheduler (Scheduler.Normal (socket_out, Unix.inet_addr_loopback)) 
  in
  let conn_in  =  
    Scheduler.make_connection scheduler (Scheduler.Normal (socket_in, Unix.inet_addr_loopback)) 
  in
  let block_size = 6 in
    (* WRITE TEST *)
  let write_callback n pos =
    Printf.printf "Write %d ended at pos %d\n" n pos;
    flush stdout
  in
  let count = ref 0 in
  let get_count () = incr count; !count in
  let test_write s = 
    Scheduler.write scheduler conn_in ~block_size s (write_callback (get_count ())) 
  in
    (* READ TEST *)
  let rec test_read () = 
    Scheduler.read_all scheduler conn_out ~block_size (read_callback (get_count ()))
  and read_callback n (pos, buff) =
    let v = FBuffer.contents buff in
      Printf.printf "Read %d ended at pos %d with value ->%s<-\n" n pos v;
      flush stdout;
      test_read ();
  in
  let _ =
    (    
      test_write "first-write\n";
      test_read (); (* Read and write at the same time now works *)
      test_write "second-write-very-very-very-long\n"; (* several write at the same time now works *)
      test_write "third\n");
    (try
       test_read ();
       assert false; (* the second read registration HAS to raise the busy exception *)
     with
       | Scheduler.Busy_direction -> print_string "Ok, second read not permitted\n"; flush stdout
    );   
  in
    Scheduler.remove_connection scheduler conn_out;
    Scheduler.remove_connection scheduler conn_in


 
let clean_print s =
    flush stdout;
    flush stderr;
    print_string s;
    flush stdout;
    flush stderr

let write_timeout () =
  let _ = Unix.putenv "MLSTATE_SHOW_LOGS" "1" in
  let scheduler = Scheduler.make () in
  let conn_out =  Scheduler.make_connection scheduler (Scheduler.File Unix.stdout) in
  let conn_in  =  Scheduler.make_connection scheduler (Scheduler.File Unix.stdin) in
  let block_size = 6 in
    (* WRITE TEST *)
    (* I bet that counting to 100,000 takes more than 5 sec. *)
  let count = ref 10000000 in
    Scheduler.write scheduler conn_in ~timeout:10. ~block_size "official message 1\n" ~err_cont:(fun e -> print_string "time's over! 1\n" ; flush stdout;) (fun i -> print_string "normal cont 1\n"; flush stdout; ) ;
    Scheduler.write scheduler conn_in ~timeout:0.00001 ~block_size "official message 2\n" ~err_cont:(fun e -> print_string "time's over! 2\n" ; flush stdout;) (fun i -> print_string "normal cont 2\n"; flush stdout; ) ;
    while (!count > 0) do
      Scheduler.wait scheduler ~block:false ;
      count := !count - 1;
      flush stdout;
    done



let _ =
    print_string " == Scheduler test == ";
    print_newline ();
    clean_print "--> \n\tbasic_std_test:\n" ;
    basic_std_test ();
    clean_print "--> \n\tcreate_remove_epoll_fails\n";
    create_remove_epoll_fails ();
    clean_print "--> \n\tremove_unexisting\n" ;
    remove_unexisting ();
    clean_print "--> \n\tthrough_files\n";
    through_files ();
    clean_print "--> \n\tsimple_socket\n";
    simple_socket ();
    clean_print "--> \n\twith_read_all\n";
    with_read_all ();
    clean_print "--> \n\twrite_timeout\n";
    write_timeout ();
    
