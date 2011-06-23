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
module List = BaseList
module A = ServerArg

module type Badop_wrapper = functor (Backend: Badop.S) -> Badop.S

let default_file ?(name="db") () =
  let (/) = Filename.concat in
  let mlstate_dir =
    try
      let id = Unix.geteuid() in
      (Unix.getpwuid id).Unix.pw_dir /
        (if id = 0 || id >= 500 then ".mlstate" else "mlstate")
    with Not_found ->
      Lazy.force File.mlstate_dir (* Useful on windows (getpwuid raises that) *)
  in
  mlstate_dir / (Filename.basename (Sys.argv.(0))) / name

let default_port = 4849
let default_local_options =
  { Badop.
    path = default_file();
    revision = None;
    restore = None;
    dot = false;
    readonly = false;
  }

let default =
  (module Badop_local : Badop.S), (Badop.Options_Local default_local_options)

let badop_wrapper functo modul =
  let module M = (val modul : Badop.S) in
  let module F = (val functo : Badop_wrapper) in
  (module F(M) : Badop.S)

let get_local_options = function
  | Badop.Options_Local o -> o
  | _ -> raise Not_found


let consume_option name =
  let rec aux acc lst =
    match lst with
    | [] -> false, acc
    | x::y ->
        if x = name then
          true, y @ acc
        else
          aux (x::acc) y
  in
  aux []


let options_parser_with_default ?name (_default_m, default_o) =
  let spec_msg = match name with None -> "" | Some n -> Printf.sprintf " for database \"%s\"" n in
  [
    ["--db-remote"],
    A.func A.parse_addr
      (fun (_,o) (addr,portopt) ->
         if o <> default_o
         then prerr_endline ("Warning: database options before --db-remote will be ignored"^spec_msg);
         (module Badop_client : Badop.S),
         Badop.Options_Client (Scheduler.default, (addr, Option.default default_port portopt))),
      "<host>[:<port>]",
      (let default_str = match default_o with
         | Badop.Options_Client(_,(host,port)) ->
             Printf.sprintf " (default: %s:%d)" (Unix.string_of_inet_addr host) port
         | _ -> ""
       in
       Printf.sprintf "Use a remote database on given server%s" default_str)
    ;
    ["--db-local"],
    A.func (A.option A.string)
      (fun (_,o) str_opt ->
         if o <> default_o
         then prerr_endline ("Warning: database options before --db-local will be ignored"^spec_msg);
         let path,flags = match str_opt with
           | Some str -> Base.String.split_char_last ':' str
           | None -> "", ""
         in
         let path = match path with
           | "" ->
               (match o with Badop.Options_Local({ Badop.path = path; _ }) -> path | _ -> default_file ?name ())
           | p -> (match name with None -> p | Some n -> Filename.concat p n)
         in

         let lflags = BaseString.slice ',' flags in
         let restore,lflags =
           let found, lflags = consume_option "restore" lflags in
           let r =
             if found then Some true
             else
               (match o with Badop.Options_Local({ Badop.restore = r; _ }) -> r | _ -> None)
           in r,lflags in

         let dot,lflags =
           let found, lflags = consume_option "dot" lflags in
           let r =
             if found then true
             else
               (match o with Badop.Options_Local({ Badop.dot = d; _ }) -> d | _ -> false)
           in r,lflags in

         let readonly,lflags =
           let found, lflags = consume_option "readonly" lflags in
           let r =
             if found then true
             else
               (match o with Badop.Options_Local({ Badop.readonly = d; _ }) -> d | _ -> false)
           in r,lflags in

        if not (List.is_empty lflags) then
          (prerr_endline ("Error: unknown db flag "^(List.print (fun x -> x) lflags)^spec_msg); raise Exit);

         (module Badop_local : Badop.S),
         Badop.Options_Local { Badop. path; restore; dot; readonly; revision = None }),
      "[<path>][:<flags>]",
      (let default_str = match default_o with
         | Badop.Options_Local({ Badop.path = path; _ }) ->
             Printf.sprintf " (default: %s)" path
         | _ -> ""
       in
       Printf.sprintf
         "Use a local database at given path. Use additional flag 'restore' to try and recover a corrupted database, \
          or 'dot' to have a database dot output each commit%s. You can specify several flags, separated by ','." default_str)
  ]
  @
  #<If:BADOP_DEBUG> [
    ["--db-revision"],
    A.func A.int
       (fun (_,o) i ->
         let opt = get_local_options o in
         (module Badop_local : Badop.S), Badop.Options_Local { opt with Badop.revision = Some i }),
     "<int>", "Revert the database to the given revision. Be careful, all data after that revision will be cleared";
    ["--db-template"],
    A.func A.unit
      (fun (m,o) () ->
         badop_wrapper (module Badop_wrapper_template.F : Badop_wrapper) m, o),
    "", "Wrap the database defined in previous arguments with a template identity layer";
    ["--db-stash"],
    A.func A.unit
      (fun (m,o) () ->
         badop_wrapper (module Badop_stash.F : Badop_wrapper) m, o),
    "", "Wrap the database defined in previous arguments with a trivial caching layer";
    ["--db-debug"],
    A.func (A.option A.string)
      (fun (m,o) pfx ->
         badop_wrapper (module Badop_debug.F : Badop_wrapper) m,
         Badop.Options_Debug
           (Printf.sprintf
              "[45m[30m[Badop%s][0m [35m"
              (Option.default_map "" (Printf.sprintf "(%s[45m[30m)") pfx), o)),
    "[string]", "Wrap the database defined in previous arguments with a debugging layer (prints about all database operations on stderr, with an optional tag)";
    ["--db-stats"],
    A.func A.unit
      (fun (m,o) () ->
         badop_wrapper (module Badop_stats.F : Badop_wrapper) m, o),
    "", "Wrap the database defined in previous arguments with a layer that prints statistics at exit";
    ["--db-dispatch"],
    A.func A.int
      (fun (m,o) n ->
         let rec specialise_opts i = function
           | Badop.Options_Local opt -> Badop.Options_Local {opt with Badop.path =  (opt.Badop.path^"_"^string_of_int i)}
           | Badop.Options_Debug (s,o) ->
               Badop.Options_Debug (Printf.sprintf "%s[3%dm<%d> " s (1 + i mod 6) i, specialise_opts i o)
           | _ -> failwith ("dispatch to that backend unsupported yet"^spec_msg)
         in
         badop_wrapper (module Badop_dispatcher.F : Badop_wrapper) m,
         Badop.Options_Dispatcher
           (1, Base.List.init n (fun i -> specialise_opts i o))),
    "<int>", "Dispatch the database accesses to <n> database instances";
    ["--db-workaround"],
    A.func A.unit
      (fun (m,o) () ->
         badop_wrapper (module Badop_workaround.F : Badop_wrapper) m, o),
    "", "Wrap the database defined in previous arguments with a layer that starts all operations from the root. This is inefficient but makes the dispatcher work properly until we handle locality correctly in links and maps";
    ["--db-check"],
    A.func A.unit
     (fun (m,o) () ->
       badop_wrapper (module Badop_check.F : Badop_wrapper) m, o),
    "", "Wrap the database defined in previous arguments with a layer that check some operations";
  ]
  #<Else> [] #<End>

let options_parser = options_parser_with_default default
