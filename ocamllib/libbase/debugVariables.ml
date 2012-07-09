(*
    Copyright © 2011, 2012 MLstate

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

(* THIS FILE HAS A DOCUMENTED MLI *)

type debug_var = string option

let other_mlstate_variables = ["no_embedded_lib"; "noide"]
let table = Hashtbl.create 100

let var var =
  let var = "MLSTATE_" ^ (String.uppercase var) in
  let value =
    try
      Some (Sys.getenv var)
    with
    | Not_found -> None in
  assert (not (Hashtbl.mem table var));
  Hashtbl.add table var value ;
  value

let () = List.iter (fun s -> ignore (var s)) other_mlstate_variables
(* *PLEASE* keep in alphabetical order (here and in mli) *)
let add_stdlib = var "add_stdlib"
let badop_debug = var "badop_debug"
let badop_xml_import_commit_transaction_step = var "badop_xml_import_commit_transaction_step"
let bsl_loading = var "bsl_loading"
let bsl_no_restriction = var "bsl_no_restriction"
let bsl_projection = var "bsl_projection"
let bsl_register = var "bsl_register"
let bsl_sl = var "bsl_sl"
let buffer_pool_debug = var "buffer_pool_debug"
let buffer_pool_enable = var "buffer_pool_enable"
let bypass_hoisting = var "bypass_hoisting"
let check_vars = var "check_vars"
let closure_debug = var "closure_debug"
let closure_opt = var "closure_opt"
let closure_stat = var "closure_stat"
let const_sharing_client_float = var "const_sharing_client_float"
let const_sharing_client_record = var "const_sharing_client_record"
let const_sharing_client_remove_coerce = var "const_sharing_client_remove_coerce"
let const_sharing_client_string = var "const_sharing_client_string"
let const_sharing_server_float = var "const_sharing_server_float"
let const_sharing_server_record = var "const_sharing_server_record"
let const_sharing_server_remove_coerce = var "const_sharing_server_remove_coerce"
let const_sharing_server_string = var "const_sharing_server_string"
let cps_blocking_wait = var "cps_blocking_wait"
let cps_debug = var "cps_debug"
let cps_keep_letcont = var "cps_keep_letcont"
let cps_noskip = var "cps_noskip"
let cps_stack_trace = var "cps_stack_trace"
let cps_verbose = var "cps_verbose"
let database_reconnect = var "database_reconnect"
let db3_no_final_snapshot = var "db3_no_final_snapshot"
let db3_transaction_limit = var "db3_transaction_limit"
let dbgen_always_upgrade = var "dbgen_always_upgrade"
let dbgen_butcher = var "dbgen_butcher"
let dbgen_debug = var "dbgen_debug"
let dbgen_flags = var "dbgen_flags"
let debug_db = var "debug_db"
let debug_db_index = var "debug_db_index"
let debug_db_max_delta = var "debug_db_max_delta"
let debug_paxos = var "debug_paxos"
let debug_paxos_cluster = var "debug_paxos_cluster"
let debug_paxos_consensus = var "debug_paxos_consensus"
let debug_paxos_le = var "debug_paxos_le"
let debug_paxos_rbr = var "debug_paxos_rbr"
let debug_paxos_sched = var "debug_paxos_sched"
let debug_xml = var "debug_xml"
let diffing = var "diffing"
let effects_show = var "effects_show"
let expl_inst_debug = var "expl_inst_debug"
let expl_inst_no_memo = var "expl_inst_no_memo"
let expl_inst_normalize = var "expl_inst_normalize"
let expl_inst_opt_debug = var "expl_inst_opt_debug"
let expl_inst_typename = var "expl_inst_typename"
let libnet_cluster = var "libnet_cluster"
let hlnet_debug = var "hlnet_debug"
let hldir_debug = var "hldir_debug"
let http_debug = var "http_debug"
let http_no_cookie = var "http_no_cookie"
let http_client_debug = var "http_client_debug"
let js_imp = var "js_imp"
let js_match_compilation = var "js_match_compilation"
let js_no_split = var "js_no_split"
let js_no_tailcall = var "js_no_tailcall"
let js_renaming = var "js_renaming"
let js_serialize = var "js_serialize"
let lambda_coerce = var "lambda_coerce"
let lambda_correct = var "lambda_correct"
let lambda_debug = var "lambda_debug"
let low_level_db_log = var "low_level_db_log"
let mimetype_debug = var "mimetype_debug"
let mongo_debug = var "mongo_debug"
let mongo_buffer_pool = var "mongo_buffer_pool"
let no_access_log = var "no_access_log"
let no_database_upgrade = var "no_database_upgrade"
let no_flood_prevention = var "no_flood_prevention"
let no_server_info = var "no_server_info"
let no_async = var "no_async"
let nocache = var "nocache"
let object_debug = var "object_debug"
let ocamldep_show_logs = var "ocamldep_show_logs"
let omanager_debug = var "omanager_debug"
let opacapi_loose = var "opacapi_loose"
let opadoc = var "opadoc"
let opatop_annot = var "opatop_annot"
let opatop_expr = var "opatop_expr"
let opatop_hook = var "opatop_hook"
let opatop_unvalrec = var "opatop_unvalrec"
let parser_cache_debug = var "parser_cache_debug"
let patterns_normalize = var "patterns_normalize"
let patterns_real_patas = var "patterns_real_patas"
let ping_debug = var "ping_debug"
let buf_debug = var "buf_debug"
let ppdebug = var "ppdebug"
let protocol_debug = var "protocol_debug"
let qmlc_no_magic = var "qmlc_no_magic"
let qmltop_time = var "qmltop_time"
let redundancy = var "redundancy"
let reorder = var "reorder"
let resource_tracker_debug = var "resource_tracker_debug"
let rpc_alt_skeleton = var "rpc_alt_skeleton"
let rpc_debug = var "rpc_debug"
let sa_dependencies = var "sa_dependencies"
let sa_printer_annot = var "sa_printer_annot"
let sa_printer_ty = var "sa_printer_ty"
let sa_trx = var "sa_trx"
let sa_xml_pattern = var "sa_xml_pattern"
let scheduler_debug = var "scheduler_debug"
let server_serialize = var "server_serialize"
let server_stats = var "server_stats"
let session_debug = var "session_debug"
let show_logs = var "show_logs"
let simplifymagic_disable = var "simplifymagic_disable"
let simplifymagic_failures = var "simplifymagic_failures"
let slicer_cond = var "slicer_cond"
let slicer_debug = var "slicer_debug"
let slicer_time = var "slicer_time"
let ssl_debug = var "ssl_debug"
let testing = var "testing"
let typer = var "typer"
let weblib_debug = var "weblib_debug"
(* testers *)
type debug_tester = debug_var -> bool
let default = function Some x -> x <> "0" | None -> false
let null d = not (default d)
let defined s = s <> None
let undefined s = s = None
let equals s = function
  | None -> false
  | Some s' -> String.compare s s' = 0
let toggle = equals "1"
let level cond = function
  | None -> false
  | Some s ->
      begin
        match Base.int_of_string_opt s with
        | Some i -> cond i
        | _ -> false
      end
let islevel n = level (fun i -> i = n)
let maxlevel n = level (fun i -> i <= n)
let minlevel n = level (fun i -> i >= n)
let contains substr = function
  | None -> false
  | Some envvar -> Base.String.is_contained substr envvar
let cont cont var = cont var
let is_contained str = function
  | None -> false
  | Some envvar -> Base.String.is_contained envvar str

let flag str = function
  | None -> false
  | Some var -> List.mem str (Base.String.slice ',' var)

let _ =
  match check_vars with
  | Some "0"
  | None -> ()
  | Some s ->
      let strong = s = "s" in
      let error = ref false in
      let iter var_equal_value =
        let var = Base.String.left_at var_equal_value '=' in
        if (Base.String.is_prefix "MLSTATE_" var)
        && not (Hashtbl.mem table var)
        then
          begin
            Printf.eprintf "[!] DebugVariables(MLSTATE_CHECK_VARS) -- process environment contains an unknown var :\n\t%s\n%!" var ;
            if strong then error := true
          end in
      Array.iter iter (Unix.environment ());
      if !error then (Printf.eprintf "[!] MLSTATE_CHECK_VARS=s => exit 1\n%!"; exit 1)
