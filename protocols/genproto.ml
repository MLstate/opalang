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

module List = Base.List
module O = Ocaml
module Cons = O.Cons
module G = Grammar

let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))

let msgtype_of_defs lst =
  let typeconstr_of_def = function
    | G.Define (G.Constr (name, []), _) -> name,  None
    | G.Define (G.Constr (name, lst), _) ->
        name, Some (O.TypeTuple (List.map (snd @* Tools.tuple_of_var) lst))
    | _ -> assert false
  in O.Type [[], "msg", O.TypeConstructor (List.map typeconstr_of_def lst)]

let gettype_of_defs lst =
  let gettype_of_def = function
    | G.Define (G.Constr (name, []), _) ->
        O.Val (Ident.source (Printf.sprintf "get_%s" name),
             O.TypeArrow (O.TypeName ([O.TypeVerbatim "msg"],["list"]), O.TypeVerbatim "msg option"))
    | G.Define (G.Constr (name, lst), _) ->
        O.Val (Ident.source (Printf.sprintf "get_%s" name),
             O.TypeArrow (O.TypeName ([O.TypeVerbatim "msg"],["list"]),
                        O.TypeName ([(O.TypeTuple (List.map (snd @* Tools.tuple_of_var) lst))],["option"])))
    | _ -> assert false
  in List.map gettype_of_def lst

let rec ol2l = function [] -> [] | (Some x)::rest -> x::ol2l rest | None::rest -> (O.TypeConst O.TypeString)::ol2l rest

let msgtype_of_raws lst =
  let typeconstr_of_raw = function
    | G.Raw (name, _, []) -> name,  None
    | G.Raw (name, _, lst) -> name, Some (O.TypeTuple (ol2l (List.map (fun (_,b,_,_) -> b) lst)))
    | _ -> assert false
  in O.Type [[], "rawmsg", O.TypeConstructor (List.map typeconstr_of_raw lst)]

let gettype_of_raws lst =
  let gettype_of_raw = function
    | G.Raw (name, _, []) -> O.Val (Ident.source (Printf.sprintf "get_%s" name),
                                O.TypeArrow (O.TypeName ([O.TypeVerbatim "rawmsg"],["list"]), O.TypeVerbatim "rawmsg option"))
    | G.Raw (name, _, lst) -> O.Val (Ident.source (Printf.sprintf "get_%s" name),
                                 O.TypeArrow (O.TypeName ([O.TypeVerbatim "rawmsg"],["list"]),
                                            O.TypeName ([(O.TypeTuple (ol2l (List.map (fun (_,b,_,_) -> b) lst)))],["option"])))
    | _ -> assert false
  in List.map gettype_of_raw lst

let str_of_type_expr te = let b = Buffer.create 1024 in (OcamlPrint.Buf.type_expr b te; Buffer.contents b)

let make_runtime_type = function
  | G.Startfun (name, lst, _) ->
      let param_types =
        str_of_type_expr (match lst with [] -> O.TypeConst O.TypeUnit | _ -> (O.TypeTuple (List.map (snd @* Tools.tuple_of_var) lst))) in
"\
type proto_exn = exn * string * (string option)\n\
\n\
type ec = proto_exn -> runtime -> Scheduler.t -> Scheduler.connection_info -> Buffer.t * int ref -> unit\n\
\n\
type ecsa = proto_exn -> runtime -> Scheduler.t -> Scheduler.connection_info -> Buffer.t * int ref -> ec -> unit\n\
\n\
type t =\n  \
  {\n    \
    runtime : runtime;\n    \
    err_cont : ecsa option;\n    \
    extra_params : "^param_types^";\n  \
  }\n\n"
  | _ -> assert false

let make_launch_server parserprefix = function
  | G.Startfun (name, lst, _) ->
      let params = String.concat " " <| List.map (fst @* Tools.tuple_of_var) lst in
      let paramst = "("^(String.concat "," <| List.map (fst @* Tools.tuple_of_var) lst)^")" in
      let modname = String.capitalize parserprefix in
"\n(* == Future design on Runtime layer == *)\n\
(* Two steps (privileged and unprivileged) *)\n\
exception PermissionDenied\n\
exception UnixError\n\
\n\
let proto_"^name^"_start "^params^" runtime sched conn (_err_cont:ec) =\n  \
  (* Per-connection initialisation here... *)\n  \
  incr number_of_connections;\n  \
  proto_"^name^" "^params^" runtime sched conn (new_mailbox runtime) _err_cont\n\
\n\
let get_ports (t:t) (sched:Scheduler.t) =\n  \
[ t.runtime.rt_proto.rt_name, `Connection\n      \
{ Network.\n          \
          conn_incoming =\n            \
            (fun _params conn ->\n               \
               let "^paramst^" = t.extra_params in\n               \
               let _err_cont = Option.default default_err_cont (Option.map ecsa2ec t.err_cont) in\n               \
               proto_"^name^"_start "^params^" t.runtime sched conn _err_cont\n            \
            );\n          \
          conn_terminating = (fun () -> Logger.log \""^modname^" terminated\");\n          \
          secure_mode = t.runtime.rt_proto.rt_secure_mode;\n          \
          port_spec = Network.make_port_spec ~protocol (Unix.inet_addr_of_string t.runtime.rt_proto.rt_addr) t.runtime.rt_proto.rt_port\n        \
      };\n  \
  ]\n\n"
  | _ -> assert false


let make_launch_client = function
  | G.Startfun (name, lst, _) ->
      let params = String.concat " " <| List.map (fst @* Tools.tuple_of_var) lst in
      let paramst = "("^(String.concat "," <| List.map (fst @* Tools.tuple_of_var) lst)^")" in
"\n\
let proto_"^name^"_start "^params^" (runtime:runtime) (sched:Scheduler.t) (conn:Scheduler.connection_info) ?_err_cont () =\n  \
  (* Per-connection initialisation here... *)\n  \
  incr number_of_connections;\n  \
  let _err_cont = Option.default default_err_cont (Option.map ecsa2ec _err_cont) in\n  \
  proto_"^name^" "^params^" runtime sched conn (new_mailbox runtime) _err_cont\n\
\n\
let connect (t:t) ?(secure_mode=Network.Unsecured) (sched:Scheduler.t) (addr:string) (port:int) =\n  \
  let port_spec = Network.make_port_spec ~protocol (Network.inet_addr_of_name addr) port in\n  \
  let "^paramst^" = t.extra_params in\n  \
  Network.connect sched port_spec secure_mode\n                  \
                  (fun conn -> proto_"^name^"_start "^params^" t.runtime sched conn ?_err_cont:t.err_cont ())\n\
\n\
let run_client (sched:Scheduler.t) =\n  \
    Scheduler.run sched\n\
\n\
let launch_client (t:t) (addr:string) (port:int) =\n  \
  let sched = Scheduler.default in\n  \
  connect t sched addr port;\n  \
  run_client sched\n\n"
  | _ -> assert false


let gen_functor_sign has_raw arg lst =
  let ilst, stuff = List.partition (function G.Import _ -> true | _ -> false) lst in
  let sign = List.map Tools.val_of_import ilst
  and startfun =
    try
      List.find (function G.Startfun _ -> true | _ -> false) lst
    with Not_found -> failwith "Fatal Error: No entry node defined"
  in
  let fsign =
    match startfun with
    | G.Startfun (_, lst, _) ->
        let funame = match arg with | "server" -> "launch_server" | _ -> "launch_client" in
          (match arg with
           | "client" ->
               [ O.Val (Ident.source "number_of_connections", O.TypeVerbatim "int ref");
                 O.Val (Ident.source "connect",
                        O.TypeVerbatim ("t -> ?secure_mode:Network.secure_mode -> Scheduler.t -> string -> int -> unit"));
                 O.Val (Ident.source "close_conn", O.TypeVerbatim ("Scheduler.t -> Scheduler.connection_info -> Buffer.t * int ref -> unit"));
                 O.Val (Ident.source "run_client", O.TypeVerbatim ("Scheduler.t -> unit"));
                 O.Val (Ident.source funame, O.TypeVerbatim ("t -> string -> int -> unit"));
                 O.Val (Ident.source "protocol", O.TypeVerbatim "NetAddr.protocol");
                 O.Val (Ident.source "string_of_msg", O.TypeVar("msg -> string")) ;
               ]
           | _ ->
               [ O.Exception ("PermissionDenied",None);
                 O.Exception ("UnixError",None);
                 O.Val (Ident.source "number_of_connections", O.TypeVerbatim "int ref");
                 O.Val (Ident.source "string_of_msg", O.TypeVar("msg -> string")) ;
                 if has_raw then O.Val (Ident.source "string_of_rawmsg", O.TypeVar("rawmsg -> string")) else O.Verbatim "";
                 O.Val (Ident.source "compare_msg", O.TypeVar("msg * msg -> bool")) ;
                 O.Val (Ident.source "get_msg_name", O.TypeVar("msg -> string")) ;
                 O.Val (Ident.source "get_ports",
                      O.TypeVerbatim("t -> Scheduler.t -> (string * [> `Connection of Network.port ]) list"));
                 O.Val (Ident.source "protocol", O.TypeVerbatim "NetAddr.protocol");
               ])
    | _ -> assert false
  in
  if List.length sign > 0 then
    [O.DeclareFunctor (
       "Make",
       [ ("Required", Some (O.Signature (O.Inlined sign))) ],
       Some (O.Signature (O.Inlined fsign)),
       O.Structure []
     )]
  else fsign

let read_fun pp pn = O.Verbatim ("\n\
(*let raw_oc_opt = Some (open_out (Sys.getenv(\"HOME\")^\"/output.txt\"))*)\n\
\n\
let write_errcont runtime sched conn ?block_size ?timeout buf ?err_cont finalize =\n  \
  let default_errcont = function\n    \
    | Scheduler.Timeout ->\n    \
      (Logger.error \"write_errcont: Timeout\";\n       \
       Scheduler.remove_connection sched conn)\n    \
    | Scheduler.Connection_closed ->\n      \
      (Logger.error \"write_errcont: Connection_closed\";\n       \
       Scheduler.remove_connection sched conn)\n    \
    | exn ->\n      \
      (Logger.error \"Caught write exception: %s\" (Printexc.to_string exn);\n       \
       if runtime.rt_proto.rt_backtrace then Logger.debug \"%s\" (Printexc.get_backtrace());\n       \
       Scheduler.remove_connection sched conn)\n  \
  in\n  \
  let err_cont = Option.default default_errcont err_cont in\n  \
  let timeout = Option.default (runtime.rt_proto.rt_server_write_timeout) timeout in \n  \
  (*#<If$minlevel 10>match raw_oc_opt with\n  \
  | Some oc -> (output_string oc buf; Pervasives.flush oc)\n  \
  | None -> ()#<End>;*)\n  \
  Scheduler.write sched conn ?block_size ~timeout buf ~err_cont finalize\n\
\n\
let new_mailbox runtime = (HttpTools.get_buf ~hint:runtime.rt_proto.rt_block_size (), ref 0)\n\
\n\
let number_of_connections = ref 0\n\
\n\
let close_conn sched conn mailbox =\n  \
  (* Connection close code here... *)\n  \
  HttpTools.free_buf (fst mailbox);\n  \
  HttpTools.collect_bufs 2 (*(!number_of_connections)*);\n  \
  decr number_of_connections;\n  \
  #<If$minlevel 2>Logger.debug \"close_conn: %d\" !number_of_connections#<End>;\n  \
  Scheduler.remove_connection sched conn\n\
\n\
let (default_err_cont:ec) = fun (exn,name,bt_opt) _runtime sched conn mailbox ->\n  \
  Logger.error \""^(String.lowercase pp)^"(%s): Uncaught exception %s%!\" name (Printexc.to_string exn);\n  \
  Option.iter (fun bt -> Logger.debug \"%s\" bt) bt_opt;\n  \
  close_conn sched conn mailbox\n\
\n\
let ec2ecsa (ec:ec) = ((fun exn_name _runtime sched conn mailbox _ec -> ec exn_name _runtime sched conn mailbox):ecsa)\n\
\n\
let ecsa2ec (ecsa:ecsa) = ((fun exn_name _runtime sched conn mailbox -> ecsa exn_name _runtime sched conn mailbox default_err_cont):ec)\n\
\n\
let receive _runtime sched conn mailbox ?err_cont ?timeout cont =\n  \
  let parse_msg (buf,start,len) =\n    \
    let str = Buffer.sub buf start len in\n    \
    #<If$minlevel 2>Logger.debug \"receive: str='%s'\" (String.escaped str)#<End>;\n    \
    let (_pos,msg) = parse_"^(String.lowercase pn)^"_msg1 str in\n    \
    #<If$minlevel 1>Logger.debug \"receive: msg1=%s\" (String.escaped (string_of_msg msg))#<End>;\n    \
    cont msg\n  \
  in\n  \
    try\n      \
      HttpTools.upto_mark_stream_cps2 ~inclusive:true sched conn mailbox \"\\r\\n\" _runtime.rt_proto.rt_payload ?err_cont ?timeout parse_msg\n    \
    with exn -> match err_cont with Some err_cont -> err_cont exn | None -> raise exn\n\
")

let gen_functor ~protocol parserprefix parsername arg lst types startfun =
  let dlst, temp = List.partition (function G.Define _ -> true | _ -> false) lst in
  let rlst, temp2 = List.partition (function G.Raw _ -> true |_ -> false) temp in
  let slst, rest = List.partition (function G.Set _ -> true | _ -> false) temp2 in
  (*let opens, stuff = List.partition (function G.MOpen _ -> true | _ -> false) rest in*)
  let ilst, stuff2 = List.partition (function G.Import _ -> true | _ -> false) rest in
  let ocam, stuff3 = List.partition (function G.OcamlTop _ -> true | G.Ocaml _ -> true | G.GVerbatim _ -> true | _ -> false) stuff2 in
  let dbgvars, stuff4 = List.partition (function G.Debugvar _ -> true | _ -> false) stuff3 in
  let funs, _ = List.partition (function G.Startfun _ -> true | G.Fun _ -> true | _ -> false) stuff4 in
  let fs = Rewrite_funs.gen_funs funs in
  let code = List.fold_left (fun el gl ->
                               match gl with
                               | G.OcamlTop sl -> sl@el
                               | G.Ocaml (dbg,l,ew,en,e) -> (Rewrite_funs.dbgexpr (dbg,l,ew,en,e))::el
                               | G.GVerbatim (dbg,l,ew,en,s) -> (O.Verbatim (Rewrite_funs.verbstr (dbg,l,ew,en,s)))::el
                               | _ -> assert false) [] ocam in
  let sign = List.map Tools.val_of_import ilst in
  let launch =
    match arg with
    | "server" -> make_launch_server parserprefix startfun
    | _ -> make_launch_client startfun
  in
  let stru =
    O.Verbatim (Printf.sprintf "let protocol = NetAddr.mk_protocol \"%s\"" protocol)
    :: (if List.length sign > 0 then O.Open [Ident.source "Required"] else O.Verbatim "")
    :: (if not (List.is_empty dlst) then Gen_printer.do_it dlst else O.Verbatim "")
    :: (if not (List.is_empty dlst) then Gen_compare.gen_get_compare_msg dlst else O.Verbatim "")
    :: (if not (List.is_empty dlst) then Gen_compare.gen_get_msg_name dlst else O.Verbatim "")
    :: (if not (List.is_empty dlst) then Gen_compare.gen_get_msg_value dlst else O.Verbatim "")
    :: (if not (List.is_empty rlst) then Gen_raw_printer.do_it rlst else O.Verbatim "")
    :: (if not (List.is_empty rlst) then Gen_compare.gen_get_compare_rawmsg rlst else O.Verbatim "")
    :: (if not (List.is_empty rlst) then Gen_compare.gen_get_rawmsg_name rlst else O.Verbatim "")
    :: (if not (List.is_empty rlst) then Gen_compare.gen_get_rawmsg_value rlst else O.Verbatim "")
    :: read_fun parserprefix parsername
    :: List.map Tools.let_of_set slst
    @ code
    @ fs
    @ [O.Verbatim launch]
  in
    O.Open [Ident.source "Base"]
    :: O.Open [Ident.source "Scheduler"]
    :: O.Open [Ident.source (String.capitalize parsername)]
    (*:: List.map (function MOpen s -> O.Open [Ident.source s] | _ -> assert false) opens*)
    :: List.map (function G.Debugvar s -> O.Verbatim ("#<Debugvar:"^s^">") | _ -> assert false) dbgvars
    @ types
    @ (if List.length sign > 0 then
        [O.DeclareFunctor (
          "Make",
          [ ("Required", Some (O.Signature (O.Inlined sign))) ],
          None,
          O.Structure stru
        )]
      else stru)

let rec resolve_includes lst =
  List.fold_right (fun expr lst ->
                     match expr with
                     | G.Include name ->
                         (*Printf.eprintf "resolve_includes: name=%s\n%!" name;*)
                         let str = File.content name in
                         let pos,partial = G.parse_grammar_prog str in
                         if pos < String.length str then
                           failwith <| Printf.sprintf "Parse error at char: %d in file %s" pos name
                         else
                           let lst2 = resolve_includes (partial) in
                           lst2@lst
                     | expr -> expr::lst) lst []

let () =
  if Array.length Sys.argv <> 5 then
    failwith "Not the right number of arguments."
  else
    let src = Sys.argv.(1) in
    let dst_dir = Sys.argv.(2) ^ "/" in
    let dst = dst_dir ^ Sys.argv.(3) ^ ".ml" in
    let mlidst = dst_dir ^ Sys.argv.(3) ^ ".mli" in
    let parserprefix = Sys.argv.(3) in
    let parsername = Sys.argv.(4) in
    let trx = dst_dir ^ parsername ^ ".trx" in
    let rp = dst_dir ^ parsername ^ "_rp.ml" in
    let str = File.content src in

    let pos, partial = G.parse_grammar_prog str in
    if pos < String.length str then
      failwith <| Printf.sprintf "Parse error at char: %d in file %s" pos src
    else
      let complete = resolve_includes partial in

      let tmpl = List.filter (function
                        | G.Generate _ -> false
                        | _ -> true) complete in
      let olst, lst3 = List.partition (function G.MOpen _ -> true | _ -> false) tmpl in
      let malst, lst4 = List.partition (function G.MAlias _ -> true | _ -> false) lst3 in
      let tlst, lst2 = List.partition (function G.MType _ -> true | _ -> false) lst4 in
      let vlst, lst = List.partition (function G.MVal _ -> true | _ -> false) lst2 in
      let defs = List.filter (function G.Define _ -> true |_ -> false) lst in
      let raws = List.filter (function G.Raw _ -> true |_ -> false) lst in
      let has_raw = List.length raws > 0 in
      let mtype = msgtype_of_defs defs::[] in
      let mvals = gettype_of_defs defs in
      let rtype = if has_raw then msgtype_of_raws raws::[] else [] in
      let rvals = if has_raw then gettype_of_raws raws else [] in
      let opens = List.map (function G.MOpen s -> O.Open [Ident.source s] | _ -> assert false) olst in
      let modaliases = List.map (function G.MAlias (a,m) -> O.Module (a,Some (O.Verbatim m),[],None) | _ -> assert false) malst in
      let startfun =
        try List.find (function G.Startfun _ -> true | _ -> false) lst
        with Not_found -> failwith "Fatal Error: No entry node defined" in
      let protocol =
        match List.find_map (function G.Protocol ct -> Some ct | _ -> None) lst with
        | Some ct -> ct
        | None -> failwith "Fatal error: No protocol type specification; please add '-protocol XXX' in the protocol file"
      in
      let runtime_type = make_runtime_type startfun in
      let types = (Tools.types_of_tdefs parsername (G.MType ("msg", O.TypeVar "msg") :: tlst))@[O.Verbatim runtime_type] in
      let vals = Tools.types_of_tdefs parsername vlst in
      if not (List.is_empty raws)
      then
        Gen_rp.do_it rtype parsername raws rp;
        if not (List.is_empty defs) then let output_gra = open_out trx in (
          try
            Gen_trx.do_it mtype defs |> OcamlPrint.Output.code output_gra;
            let output_mod = open_out dst in (
                try
                  let serv_or_client =
                    (try
                       (function G.Generate s -> s |_ -> assert false)
                       <| List.find (function G.Generate _ -> true | _ -> false) complete
                     with
                       | Not_found -> failwith "Fatal error: No generate specification;\
 please add -generate client (or server) in protocol file")
                  in opens @ modaliases @ rtype @ gen_functor ~protocol parserprefix parsername serv_or_client lst types startfun
                  |> OcamlPrint.Output.code output_mod
                  ; let output_mli = open_out mlidst in (
                    try
                      opens @ modaliases @ types @ mvals @ rtype @ rvals @ vals @ gen_functor_sign has_raw serv_or_client lst
                      |> OcamlPrint.Output.code output_mli
                    with
                    | Failure s -> prerr_endline s
                    | e -> prerr_endline
                      <| Printf.sprintf "Fatal error while generating the signature file:"
                      ^ Printexc.to_string e
                  ) ; close_out output_mli
                with
                | Failure s -> prerr_endline s
                | Not_found -> prerr_endline "Error, you didn't precise if you wanted a server or a client.\nRemember to add a « -generate » rule in your proto source code."
                | e -> prerr_endline
                  <| "Fatal error while generating the ocaml source code:\n"
                  ^ Printexc.to_string e
              ) ; close_out output_mod
          with
          | Failure s -> prerr_endline s
          | e -> prerr_endline
            <| "Fatal error while generating the trx grammar:\n"
            ^ Printexc.to_string e
        ) ; close_out output_gra
