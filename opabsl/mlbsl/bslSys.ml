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
module String = Base.String


(**
   This is used by opa servers or qml applications to have access
   to the command-line options.

   This module provides access to argv-style arguments while being on top of the
   ServerArg compatibility layer, that makes any module able to filter its
   arguments out of the command-line.

   From ML code, don't use this, use ServerArg functions.
*)
##register get_argv: -> opa[list(string)]
let get_argv () =
  let caml_list =
    List.rev
      (fst
         (ServerArg.filter_functional (ServerArg.get_argv()) []
            (ServerArg.fold (fun acc -> ServerArg.wrap ServerArg.anystring (fun s -> s::acc)))))
  in
  BslNativeLib.caml_list_to_opa_list Base.identity caml_list

##register self_name : -> string
let self_name () = Sys.argv.(0)

(**
   The function to call when you want to quit your application.
   This will do a clean exit (closing the db, ...)
*)
##register exit \ do_exit : int -> 'a
let do_exit = ServerLib.do_exit

##module process
  (**
     [exec command input]
     is like :
     echo input | command > output

     Primitive for calling an external command, and returning the string
     built from the resulting stdout of the command, given an input to
     produce on the stdin of the process.

     In case of error, return the error message instead of the process output.
  *)
  ##register exec : string, string -> string
  let exec command input =
    try
      let ic, oc = Unix.open_process command in
      output_string oc input;
      output_string oc "\n";
      flush oc;
      close_out oc;
      let rec aux lines =
        try
          let line = input_line ic in
          aux (line::lines)
        with
        | End_of_file ->
            let _ = Unix.close_process (ic, oc) in
            String.rev_concat_map "\n" (fun s -> s) lines
      in
      aux []
    with
    | Unix.Unix_error (error, a, b) ->
        Printf.sprintf "Unix_error (%S, %S, %S)" (Unix.error_message error) a b
##endmodule


##opa-type ip


##register fork : -> int
let fork = Unix.fork

##register gethostname : -> string
let gethostname = Unix.gethostname

let split_dot = Str.split (Str.regexp_string ".")
let fields_ip = List.map ServerLib.static_field_of_name ["a"; "b"; "c"; "d"]
let opa_ip ip =
  let ip = Unix.string_of_inet_addr ip in (* no way to do better apparently *)
  let values = split_dot ip in
  let record =
    List.fold_right2 (* may raise Invalid_argument "List.fold_right2" *)
      (fun fn fv record ->
        ServerLib.add_field record fn (int_of_string fv) (* may raise Failure "int_of_string" *)
      ) fields_ip values ServerLib.empty_record_constructor
  in wrap_opa_ip (ServerLib.make_record record)

(** returns the first hosts entry found *)
##register gethostbyname : string -> option(opa[ip])
let gethostbyname host =
  try
    let ip = (Unix.gethostbyname host).Unix.h_addr_list.(0) in (* may raise Not_found *)
    Some (opa_ip ip)
  with
  | Not_found | Failure _ | Invalid_argument _ -> None

(** returns all hosts entry in hosts order*)
##register gethostsbyname : string -> opa[list(ip)]
let gethostsbyname host =
  BslNativeLib.caml_list_to_opa_list Base.identity (
  try
    let ips = (Unix.gethostbyname host).Unix.h_addr_list in (* may raise Not_found *)
    Base.List.init (Array.length ips) (fun i-> opa_ip ips.(i))
  with
  | Not_found | Failure _ | Invalid_argument _ -> [])


##register finalise : ('a -> void), 'a -> void
let finalise f v = Gc.finalise f v

(** Get the current process memory usage.
    @return the memory usage in bytes *)
##register get_memory_usage : -> int
let get_memory_usage = BaseSys.get_memory_usage

(** get access to environment variable if existing *)
##register get_env_var : string -> option(string)
let get_env_var var =
 try Some(Sys.getenv var) with Not_found -> None
