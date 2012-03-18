(*
    Copyright © 2011, 2012 MLstate

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
        (if id = 0 || id >= 500 then ".opa" else "opa")
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
let default_light_options =
  { Badop.
    lpath = default_file ~name:"db_light" ();
    ondemand = Some true;
    direct = Some true;
    max_size = Some max_int;
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

let get_light_options = function
  | Badop.Options_Light o -> o
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
  let suffix = match name with None -> "" | Some n -> Printf.sprintf ":%s" n in
  [
    [Printf.sprintf "--db-remote%s" suffix],
    A.func A.parse_addr
      (fun (_,o) (addr,portopt) ->
         if o != default_o
         then Logger.warning "Warning: database options before --db-remote will be ignored%s" spec_msg;
         (module Badop_client : Badop.S),
         Badop.Options_Client (Scheduler.default, (addr, Option.default default_port portopt), fun () -> `abort)),
      "<host>[:<port>]",
      (let default_str = match default_o with
         | Badop.Options_Client(_,(host,port),_) ->
             Printf.sprintf " (default: %s:%d)" (Unix.string_of_inet_addr host) port
         | _ -> ""
       in
       Printf.sprintf "Use a remote database on given server%s" default_str)
    ;
    [Printf.sprintf "--db-local%s" suffix],
    A.func (A.option A.string)
      (fun (_,o) str_opt ->
         if o != default_o
         then Logger.warning "Warning: database options before --db-local will be ignored%s" spec_msg;
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
          (Logger.warning "Error: unknown db flag %s%s" (List.print (fun x -> x) lflags) spec_msg; raise Exit);

         (module Badop_local : Badop.S),
         Badop.Options_Local { Badop. path; restore; dot; readonly; revision = None }),
      "[<path>][:<flags>]",
      (let default_str = match default_o with
         | Badop.Options_Local({ Badop.path = path; _ }) ->
             Printf.sprintf " (default: %s)" path
         | _ -> ""
       in
       Printf.sprintf
         "Use a local database at given path%s. Use additional flag 'restore' to try and recover a corrupted database, \
          or 'dot' to have a database dot output each commit. You can specify several flags, separated by ','." default_str)
  ] @
#<Ifstatic:HAS_DBM 1>
  [
    ["--db-light"],
    A.func (A.option A.string)
      (fun (_,o) str_opt ->
         (*#<If:BADOP_DEBUG$minlevel 10>Logger.log ~color:`red "--db-light (str_opt:%s)"
                                                             (Option.to_string (fun s -> s) str_opt)#<End>;*)
         if o <> default_o
         then prerr_endline ("Warning: database options before --db-light will be ignored"^spec_msg);
         let path,flags = match str_opt with
           | Some str -> Base.String.split_char_last ':' str
           | None -> "", ""
         in
         let path = match path with
           | "" ->
               (match o with Badop.Options_Light({ Badop.lpath = path; _ }) -> path | _ -> default_file ?name ())
           | p -> (match name with None -> p | Some n -> Filename.concat p n)
         in
         let lflags = List.map (BaseString.slice '=') (BaseString.slice ',' flags) in
         let ondemand,direct,max_size,lflags =
           List.fold_left
             (fun (od,di,ms,lf) -> function
              | ["ondemand"] -> (Some true,di,ms,lf)
              | ["direct"] -> (od,Some true,ms,lf)
              | ["max_size";str] -> (od,di,(try Some (int_of_string str) with Failure "int_of_string" -> ms),lf)
              | opt -> (od,di,ms,opt@lf))
             ((match o with Badop.Options_Light({ Badop.ondemand = od; _ }) -> od | _ -> None),
              (match o with Badop.Options_Light({ Badop.direct = di; _ }) -> di | _ -> None),
              (match o with Badop.Options_Light({ Badop.max_size = ms; _ }) -> ms | _ -> None),
              []) lflags
         in
         if not (List.is_empty lflags) then
           (prerr_endline ("Error: unknown db flag "^(List.print (fun x -> x) lflags)^spec_msg); raise Exit);
         let lpath = path^"_light" in
         #<If:BADOP_DEBUG$minlevel 10>Logger.log ~color:`red "path: %s" path#<End>;
         #<If:BADOP_DEBUG$minlevel 10>Logger.log ~color:`red "ondemand: %s" (Option.to_string string_of_bool ondemand)#<End>;
         #<If:BADOP_DEBUG$minlevel 10>Logger.log ~color:`red "direct: %s" (Option.to_string string_of_bool direct)#<End>;
         #<If:BADOP_DEBUG$minlevel 10>Logger.log ~color:`red "max_size: %s" (Option.to_string string_of_int max_size)#<End>;
         let module Badop_light = Badop_light.F(DbmDB) in
         (module Badop_light : Badop.S),
         Badop.Options_Light { Badop.lpath; ondemand; direct; max_size; }),
      "[<path>][:<flags>]",
      (let default_str = match default_o with
         | Badop.Options_Light({ Badop.lpath = lpath; _ }) ->
             Printf.sprintf " (default: %s_light)" lpath
         | _ -> ""
       in
       Printf.sprintf
         "Same as --db-local, but using the lightweight, history-less backend%s."
         default_str)
  ]
#<Else>
  []
#<End>
  @
  #<If:BADOP_DEBUG> [
    ["--db-remote-replicated"],
    A.func (A.list ',' A.parse_addr)
      (fun (_,o) addrlist ->
         if o != default_o
         then Logger.warning "Warning: database options before --db-remote-replicated will be ignored%s "spec_msg;
         (module Badop_dispatcher.F(Badop_client) : Badop.S),
         Badop.Options_Dispatcher
           (List.length addrlist,
            List.map
              (fun (addr,portopt) ->
                 Badop.Options_Client (Scheduler.default, (addr, Option.default default_port portopt), fun () -> `abort))
              addrlist)),
    "<host>[:<port>],<host>[:<port>],...",
    "Use a remote database replicated on all the given servers"
    ;
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
    ["--db-cache"],
    A.func A.unit
      (fun (m,o) () ->
         badop_wrapper (module Badop_cache.F : Badop_wrapper) m, o),
    "", "Wrap the database defined in previous arguments with another trivial caching layer";
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
