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

(**
   Debug Variables Management for the MLstate debug preprocessor (ppdebug)

   @see "opalang/utils/ppdebug.pl" the script which implement the preprocess
   @author Louis Gesbert
   @author Mathieu Barbin
*)

(** {6 Template} *)

(**
   Template for a module name [MyId]
   {[
   #<Debugvar:MY_VAR>
   let debug fmt =
     OManager.printf ("@{<cyan>[MyId]@}@ @[<2>"^^fmt^^"@]@.")

   (* utilisation *)
   ...
   let _ =
     #<If:MY_VAR $minlevel 4>
       debug "do some %s of level %d@\n" "debug" 4
     #<End>
   in
   ...
   ]}
*)

(** {6 Static Debug Preprocessor -- Syntax Documentation} *)

(**
   The preprocessor {b ppdebug} is used whenever you want to add logs or computation
   in your application (assert, etc...), but you do not want the code to be embedded in release mode.

   {[
   let g =
       #<Debug>
         print_endline "this is a log" ;
       #<End>
       5
   ]}

   will be either
   {[
   let g =
(*        #<Debug> *)
         print_endline "this is a log" ;
(*        #<End> *)
       5
   ]}
   or
   {[
   let g =
(*        #<Debug> *)(*
         print_endline "this is a log" ;
*)(*        #<End> *)
       5
   ]}
   depending of the env var {b MLSTATE_PPDEBUG}, or on the tag used with ocamlbuild.

   Note that you can also have a different code instead of nothing, using [#else]
   {[
   let g =
       #<Debug>
         print_endline "this is BIG BIG LOG" ;
         print_endline "with so many details that I'd like to keep it only in debug mode" ;
       #<Else>
         print_endline "short log" ;
       #<End>
       5
   ]}

   <!> The code should be totally recompiled with/without option PPDEBUG to make changes be taken in consideration.
   For dynamic toggle behavior, cf the next part.

   For other compile-time checks (eg. target-dependent code), you can use the directive [#<Ifstatic:VAR REGEXP>].
   A check will be done by the preprocessor on environment variable [VAR] with the regexp [REGEXP], and comment
   the unappropriate code:

   {[
   let build_arch =
       #<Ifstatic:ARCH Win.*> "Windows" #<Else> "Other" #<End>
   ]}

   will define [build_arch] to be ["Windows"] whenever a variable [ARCH] is defined with a value beginning with
   ["Win"]
*)

(** {6 Dynamic Debug Preprocessor -- Syntax Documentation} *)

(**
   This preprocessor allows you to do some extra logs and computation without recompiling
   everything with a different preprocess configuration, but assures you that your code will
   not be embeded in release mode.

   {[
   let g =
      #<If:MY_SPLENDID_VARIABLE>
         print_endline "this is a log"
      #<End> ;
      5
   ]}

   will be transformed in :
   {[
   let g =
(*       #<If> *) if (DebugVariables.default) DebugVariables.my_splendid_variable then begin
         print_endline "this is a log"
(*       #<End> *) end else begin () end ;
      5
   ]}
   or if release-mode is activated :
   {[
   let g =
(*       #<If> *)(* if (DebugVariables.default) DebugVariables.my_splendid_variable then begin
         print_endline "this is a log"
(*       #<End> *) end else *) begin () end ;
      5
   ]}

   Note that the [;] at the end is kept: the ppdebug-[#<If>] should be an ocaml expression.

   You can find in this file all documented variables available.
   You should add your variables there, and documenting it following the guidelines.

   Extension of the test condition :

   By default, we check that the env var is defined and doesn't have value ["0"] (test is [DebugVariables.default]).
   You can use a different test using the following syntax :

   {[
   let g =
      #<If:MY_SPLENDID_VARIABLE $equals "MY_SPLENDID_VALUE">
         print_endline "this is a log"
      #<End> ;
      5
   ]}

   processed as :

   {[
   let  =
(*       #<If> *) if (DebugVariables.equals "MY_SPLENDID_VALUE") DebugVariables.my_splendid_variable then begin
         print_endline "this is a log"
(*       #<End> *) end else begin () end ;
      5
   ]}

   Matching on debug variables :

   If you have a multi-state debug variable, you can add a runtime-match as long as it's enclosed
   in a [#<If$defined>] :

   {[
   let verb x =
     #<If:MY_VERBOSE$defined> match DebugVariables.my_verbose with
       | Some "quite verbose" -> print_debug x
       | Some "very verbose" -> print_debug_very_verbose x
       | _ -> failwith "Bad value for debug variable MY_VERBOSE"
     #<End>
   ]}

   Warnings :
   + <!> This preprocessor should not be used for top-level definitions.
   + <!> Such a preprocessed part is a Ocaml.expr, necessary of type [unit] if there is no [#<Else>]
   + <!> Remember that in release, you're always in the Else case. Keep this for debug !

   You can find some tester available in this module.
   You can add any tester you want editing this module, and following guidelines.
*)


(** {6 Variables} *)

(**
   When you do lots of tests on the same debug-variable, you can use the shortcut:

   {[
   #<Debugvar:VAR>
   ]}

   The [:VAR] in subsequent uses of [#<If>] can be omitted.

   Guidelines :
      + all variables are documented in alphabetic order !!
      + all variable begin with {e MLSTATE_} when you export it in shell
      + all variable are UPPERCASED
      + all corresponding variable name in this file are lowercased and does not contains MLSTATE_
*)

(**
   The type of all debug_var.
   [None] if the variable is not defined, [Some value] if there is a value.

   <!> Env variables are checked only once, when this module is loaded.
*)
type debug_var = string option

(**
   {b MLSTATE_ADD_STDLIB}
   Be verbose to explain what files are added, and what files are rejected
   during the pass [AddStdlibFiles], in presence and in absence of option
   [--no-embedded-stdlib]

   Level 1: print only rejected files
   Level 2: print only keeped files
   Level 3: print both
*)
val add_stdlib : debug_var

(**
   {b BADOP_DEBUG}
   Adds some experimental options to servers related to badop modes.
*)
val badop_debug : debug_var

(**
   {b MLSTATE_BSL_LOADING}
   Use with level.
   Be more verbose during loading.
   1: plugins
   *: undocumented
*)
val bsl_loading : debug_var

(**
   {b MLSTATE_BSL_NO_RESTRICTION}
   When set, disables restriction checks on bypasses.
   Useful when recompiling printed code containing
   such bypasses.
*)
val bsl_no_restriction : debug_var

(**
   {b MLSTATE_BSL_PROJECTION}
   Use with level, print some debug logs about external bypass
   projection. (libbsl, backend, qmltop, etc...)
*)
val bsl_projection : debug_var

(**
   {b MLSTATE_BSL_REGISTER}
   Use with level. Be more verbose during building maps, and dealing
   with keys. Used during bslregister.
*)
val bsl_register : debug_var

(**
   {b MLSTATE_BSL_SL}
   Use with level. Be more verbose during some operations implemented
   in the ServerLib.
   This happens at runtime, during projections.
*)
val bsl_sl : debug_var

(**
   {b MLSTATE_BYPASS_HOISTING}
   insert debug message during the bypass hoisting pass. Works with level.
   Level 1 :
   Level 2 :
*)
val bypass_hoisting : debug_var

(**
   One variable to check them all !

   If you dont spell correctly a debug variable by exporting it before running your application,
   you may be surprised that the execution has not the expected behavior, and you may loose time until
   you notice that it is because of a variable not correctly defined.

   If (and only if) ["MLSTATE_CHECK_VARS"] is set with a value different than ["0"],
   this module will do a check on the global process environment.
   For each env variable starting with ["MLSTATE_"] which are not defined (and documented :))
   in this module, a warning will be printed on the standard error.

   Strong mode : [MLSTATE_CHECK_VARS="s"] will make an error rather than a warning, stopping the execution.
*)
val check_vars : debug_var

(** {9 Closure} *)

(**
   {b MLSTATE_CLOSURE_DEBUG}
   Works only for caml bypasses (with qmlflat only, but this could be fixed)
   Level 0 : runtime assertions were typing is not possible
             - qml closures actually have a compatible structure
             - imported functions must be exported closures
   Level 1 : previous +
             tells when applying and receiving magic arguments (to check bsl projections)
   Level 2 : previous +
             tells when applying closures to arguments and show the resulting value (very verbose)
*)
val closure_debug : debug_var
val closure_opt : debug_var (** generate optimized applyX functions *)
val closure_stat : debug_var (** display a few statistics in the passes uncurry/closure *)

(** {9 Constant Sharing} *)

(**
   Debug vars used for activating/desactivating some sharing during the passes client/server
   constant sharing. Variable are read only if the pass is activated.
*)

val const_sharing_client_float : debug_var
val const_sharing_client_record : debug_var
val const_sharing_client_remove_coerce : debug_var
val const_sharing_client_string : debug_var

val const_sharing_server_float : debug_var
val const_sharing_server_record : debug_var
val const_sharing_server_remove_coerce : debug_var
val const_sharing_server_string : debug_var

(** {9 Cps} *)

(**
   {b MLSTATE_CPS_DEBUG} :
   insert debug introspection message to trace the execution flow of continuations.

   <!> Note that CPS_DEBUG env var should also be set at runtime of the compiled
   application for logs to be produced, with a given level.

   <!> At compile time, the level is not read, everything is generated.

   {[
   export MLSTATE_CPS_DEBUG=1
   // compile
   // run
   unset  MLSTATE_CPS_DEBUG
   // run without debug logs
   // compile
   // run without debug logs
   export MLSTATE_CPS_DEBUG=1
   // run without debug logs (need a recompilation)
   ]}

   This var is used with increasing levels from 1 to 100 :
   + 1  : print a few things
   + 10 : cont tracer (print every cont application)
   + 100: backtraces display function arguments (WIP)
*)
val cps_debug : debug_var

(**
   {b MLSTATE_CPS_KEEP_LETCONT} :
   do not remove continuation directly applied. Used just for benchmark purpose.
*)
val cps_keep_letcont : debug_var

(**
   {b MLSTATE_CPS_NOSKIP} :
   do not use the skip optimisation. Used just for benchmark and tracability purpose.
*)
val cps_noskip : debug_var

(**
   {b MLSTATE_STACK_TRACE}
   When set, activates the printing of backtraces
   If it contains "tr", then transaction contexts are printed
   If it contains "th", then thread contexts are printed
   If it contains "args", then fonction arguments are printed
*)
val cps_stack_trace : debug_var

(**
   {b MLSTATE_CPS_VERBOSE} :
   compile time only : let the cps rewriting pass be more verbose. Used with level.
   No changes to the generated code, affect only the transformation pass.
   + Level 1 :
               + print some timing
   + Level 2 :
               + previous
               + print optimisation logs
*)
val cps_verbose : debug_var

(**
   {b MLSTATE_CPS_BLOCKING_WAIT} :
   Print blocking stack
*)
val cps_blocking_wait : debug_var

(** {b MLSTATE_DATABASE_RECONNECT}
  Set up a delay in seconds for automatic reconnection when a database server is lost.
  (if unset, never reconnect automatically)
*)
val database_reconnect : debug_var

(**
   {b MLSTATE_DB3_NO_FINAL_SNAPSHOT}
   When activated, when the database is closed, we do not make a snapshot of the current revision,
   meaning that the transaction file will not be empty, and pending transaction reapplied at
   reopen time.
   This is used for debugging the binary format of transaction (or queries) because if we
   always use a snapshot when we close the database, the transaction binary file is always empty.
*)
val db3_no_final_snapshot : debug_var

(**
   {b MLSTATE_DB3_TRANSACTION_LIMIT}
   Defines a limit of operations within a transaction, after which it is automatically aborted.
   Default is 10000. This is a safeguard against exploding the memory by spamming a
   transaction with too many writes. 0 disables the limit.
*)
val db3_transaction_limit : debug_var

(**
   {b DbGen obscure toggles}
*)
(** run-time *)
val dbgen_always_upgrade : debug_var (** Force upgrade of incompatible databases when they are opened *)
val dbgen_butcher : debug_var (** When updating a field from an old db is not possible, remove and re-create *)

val dbgen_debug : debug_var (** Enables some compile-time debug messages, adds debug prints in the
                                generated code and, at run-time, dbGen-related debug messages *)
(** compile-time only: *)
val dbgen_flags : debug_var
  (** comma-separated list of flags for DbGen, compile-time:
      - 'fullcoerce': put coerces almost everywhere. Useful for checks
      - 'source': instead of normal fresh identifiers, insert "source" textual ones in
                  AST. Useful to manual checks from toplevel.
      - 'nodefault': don't complain about missing default values, use by-default defaults.
      - 'nolazy': disable lazy records
      - 'sharing': enable full-sharing in the DB (enabled in db tests, temporary
                   disabled for now to workaround a bug in the low-level implementation
                   of copy-on-write)
      - 'noflatten': don't flatten recursive types, store them as deep trees
      - 'copylink': when flattening recursive types, link them with internal copy-nodes instead of link-nodes
  *)


(**
   {b MLSTATE_DEBUG_DB}
   Enables debug messages from the low-level database
   You can specify :
     - a debug level (int): to display low level db debug
     - 'migration': to display only migration debug
     - 'io': to display only io debug

*)
val debug_db : debug_var

(**
   {b MLSTATE_DEBUG_DB_INDEX}
   Enables debug messages from the index handling in the database
*)
val debug_db_index : debug_var

(**
   {b MLSTATE_DEBUG_DB_MAX_DELTA}
   How many delta nodes are permitted between full nodes; 0 means all are full.
*)
val debug_db_max_delta : debug_var

(**
   {b MLSTATE_DEBUG_PAXOS}
   Enables debug messages for the Paxos algorithm (including leader election)
*)
val libnet_cluster : debug_var
val debug_paxos : debug_var
val debug_paxos_le : debug_var
val debug_paxos_rbr : debug_var
val debug_paxos_consensus : debug_var
val debug_paxos_sched : debug_var


(**
  {b MLSTATE_DEBUG_XML}
  Enables debug messages for Xml module (libbase)
*)
val debug_xml: debug_var

(**
   {b MLSTATE_DIFFING}
   Try to remove as much as possible any diff not due to the input of the compiler,
   such as date, git sha, etc... for comparing the output of the compiler using
   2 branches.

   <!> This variable is also used in [generated_buildinfos.sh]
*)
val diffing : debug_var

(**
   {b MLSTATE_EFFECTS_SHOW}
   Show the types and effects as they are being inferred by QmlEffects
*)
val effects_show : debug_var

(**
  {b Explicit Instantiation debug}
*)
val expl_inst_opt_debug : debug_var
 (** print debug info when inserting ExplInst directives *)
val expl_inst_debug : debug_var
 (** print debug info when eliminating ExplInst directives *)
val expl_inst_no_memo : debug_var
 (** deactive sharing of types and typeschemes generated by ty_to_opaty and tsc_to_opatsc *)
val expl_inst_normalize : debug_var
 (** 0: no normalization of type variables in types and typeschemes
     1: no normalization of unbound type variables
     other: full normalization
 *)

(**
   print debug info when generating and inserting typename (type_def_map registration).
*)
val expl_inst_typename : debug_var

(**
   {b MLSTATE_HLNET_DEBUG}
*)
val hlnet_debug : debug_var
val hldir_debug : debug_var

(**
   {b MLSTATE_HTTP_DEBUG}
*)
val http_debug : debug_var

(**
   {b MLSTATE_HTTP_NO_COOKIE}
*)
val http_no_cookie : debug_var

(**
   {b MLSTATE_HTTP_CLIENT_DEBUG}
*)
val http_client_debug : debug_var

(**
   {b Javascript Compilation}
*)

(**
   General debug information about the qmljsimp compiler
*)
val js_imp : debug_var

(**
   Works with function name, and contains checker.
*)
val js_match_compilation : debug_var

(**
   Do not apply the 'split' pass of JsPasses.
*)
val js_no_split : debug_var

(**
   Do not optimize the call with the tailcall manager.
   Used for debuging only, and getting a correct stack trace
   with firebug.
   The generated code will stack overflow in practice.
*)
val js_no_tailcall : debug_var

(**
   Debug for the local renaming of qmljsimp only
*)
val js_renaming : debug_var

(**
   Debuging the serialization of javascript from compiler to runtime.
*)
val js_serialize : debug_var

(**
   {b Lambda lifting toggles}
*)
val lambda_debug : debug_var (** random stuff toggle *)
val lambda_coerce : debug_var (** adds explicit coercions in the code to check
                                  the well-typedness of the code *)
val lambda_correct : debug_var (** check that the code is well lambda lifted
                                   this is not really a postcondition since nobody cares if
                                   [val f(x) = let g(y) = y in g] is transformed as
                                   [val g(x,y) = y val f(x) = g(x)] but the lambda lifting
                                   is not correct anyway
                               *)

(**
   {b MLSTATE_LOW_LEVEL_DB_LOG}
   When activated, the low level database will print disk writing related log in
   'low_level_db.log' file.
*)
val low_level_db_log : debug_var

(**
   {b MLSTATE_MIMETYPE_DEBUG}
   display the debug on mimetype resolution
*)
val mimetype_debug : debug_var

(**
   {b MLSTATE_MONGO_DEBUG}
   display the debug for MongoDB
*)
val mongo_debug : debug_var
val mongo_buffer_pool : debug_var

(**
   {b MLSTATE_NO_ACCESS_LOG}
*)
val no_access_log : debug_var

(**
   {b MLSTATE_NO_DATABASE_UPGRADE}
   disables the database migration engine altogether,
   _assuming the db has been generated with the exact same database definition_
*)
val no_database_upgrade : debug_var

(**
   {b MLSTATE_NO_FLOOD_PREVENTION}
*)
val no_flood_prevention : debug_var

(**
   {b MLSTATE_NO_SERVER_INFO}
   Disable "starting" and "terminating" message
   when using the scheduler.
*)
val no_server_info : debug_var

(**
   {b MLSTATE_NO_FLOOD_PREVENTION}
*)
val no_flood_prevention : debug_var

(**
   {b MLSTATE_NOCACHE}
   Changes the HTTP header of responses to clients to prevent caching. Useful
   when you debug your server and re-run it often
*)
val nocache : debug_var

(**
   {b MLSTATE_OBJECT_DEBUG}
   Various printing while loading/managing object files
*)
val object_debug : debug_var

(**
   {b MLSTATE_OCAMLDEP_SHOW_LOGS}
   Do not logs the ocamldep output in a file, but print it on
   the console instead
*)
val ocamldep_show_logs : debug_var

(**
   {b MLSTATE_OMANAGER_debug}
   Replaces ansi escape by xml-like tags
*)
val omanager_debug : debug_var

(**
   {b MLSTATE_OPACAPI_LOOSE}
   Do not fail if somewhere during the compilation a lookup is done via [OpaMapToIdent]
   on an ident which is not defined in opacapi.
*)
val opacapi_loose : debug_var

(**
   {b MLSTATE_OPADOC}
   Prints some information while generating opa documentation
*)
val opadoc : debug_var

(**
   {b MLSTATE_OPATOP_ANNOT}
   Combined with MLSTATE_OPATOP_EXPR:
   will use the annotation printer for
   printing expressions before evaluation.
   Usefull in case of annot not found.
*)
val opatop_annot : debug_var

(**
   {b MLSTATE_OPATOP_EXPR}
   Print expressions before evaluation.
   Usefull for using it in case of a segfault for example.
*)
val opatop_expr : debug_var

(**
   {b MLSTATE_OPATOP_HOOK}
   Print hook from the source code for tracing the flow of execution.
   This variable is meant to be used for a better comprehension of the interpreter
*)
val opatop_hook : debug_var

(**
   {b MLSTATE_OPATOP_UNVALREC}
   Print the unvalrec used for evaluation.
*)
val opatop_unvalrec : debug_var


(**
   {b MLSTATE_PARSER_CACHE_DEBUG}
   Outputs whether there are cache hits or cache misses in the opa parser
*)
val parser_cache_debug : debug_var

(**
   {b MLSTATE_PATTERNS_NORMALIZE}
   Activating the pattern normalization.
*)
val patterns_normalize : debug_var

(**
   {b MLSTATE_PATTERNS_REAL_PATAS}
   Traduces opa patas into qml patas instead
   of performing a duplication of code.

   This will of course be the final behavior of the compiler,
   but currently, the PatAs is still not implemented everywhere
   in qml, so we can play with this debug var for debuging,
   and completing the implementation of patas in qml.
*)
val patterns_real_patas : debug_var

(**
    {b MLSTATE_PING_DEBUG}
    Debug printing on ping loop system
*)
val ping_debug : debug_var

(**
    {b MLSTATE_BUF_DEBUG}
    Debug printing on buf
*)
val buf_debug : debug_var

(**
  The env var {b MLSTATE_PPDEBUG}
*)
val ppdebug : debug_var

(**
   {b MLSTATE_PROTOCOL_DEBUG} Debug statements from DSL-generated code.
*)
val protocol_debug : debug_var

(**
   {b MLSTATE_QMLC_NO_MAGIC} : during the cps-pass, do not insert magic functions.
   The code may then not type check during ocaml compilation.
   This env var is also used by the standard ML-back-ends.

   This var is used essentially for debugging, to help to track
   runtime seg-fault, because in some case it may come from a
   bug introduced by a pass, and can be detected by the ocaml type
   checker, but not in presence of too much magie.
*)
val qmlc_no_magic : debug_var

(**
   {b MLSTATE_QMLTOP_TIME} : in qmltop, print the timing for each top level value
   computation (typing and eval)
*)
val qmltop_time : debug_var

(**
   {b MLSTATE_REDUNDANCY}
   Sets the redundancy parameter of the distributed database
*)
val redundancy : debug_var

(**
   {b MLSTATE_REORDER}
   Debug QmlDependencies module (reorder passes)
*)
val reorder : debug_var

(**
    {b MLSTATE_RPC_DEBUG}
    [rpc_debug] Debug the Opa_InsertRemote module (ResolveRemoteCalls pass)
    [rpc_alt_skeleton] Alternative implementation of skeletons
                       Activating this flag factorises the code produced
                       and should improve compilation time with neglectable
                       runtime hit
*)
val rpc_debug : debug_var
val rpc_alt_skeleton : debug_var

(**
   {b Debug on SurfaceAst passes}
*)
val sa_dependencies : debug_var (** for the debug of the reordering algorithm *)
val sa_printer_annot : debug_var (** to print the annotations while printing asts *)
val sa_printer_ty : debug_var (** to print the directive types while printing asts *)
val sa_trx : debug_var (** print the code of the parsers after they were rewritten *)
val sa_xml_pattern : debug_var (** print the ast of the xml patterns that is being rewritten *)

(**
   {b MLSTATE_SESSION_DEBUG}
*)
val session_debug : debug_var

(**
   {b MLSTATE_SCHEDULER_DEBUG}
*)
val scheduler_debug : debug_var

(**
   {b MLSTATE_SERVER_SERIALIZE}
*)
val server_serialize : debug_var

(**
   {b MLSTATE_SERVER_STATS}
   Controls whether to (collect and) print server statistics upon
   server termination.
*)
val server_stats : debug_var

(**
   {b MLSTATE_SHOW_LOGS}
   If set, all Logger logs will be output on stderr
   Consequently, no logs will be output in files
*)
val show_logs : debug_var (** show all logs on stderr *)

(**
   {b Pass_SimplifyMagic}
*)
val simplifymagic_disable : debug_var (** don't specialize any identifier *)
val simplifymagic_failures : debug_var (** show specializable identifiers that weren't specialized *)

(**
  {b Slicer debug}
*)
val slicer_cond : debug_var (** checks slicer precondition and postcondition *)
val slicer_debug : debug_var (** ?? *)
val slicer_time : debug_var (** shows where the time of the slicing pass is spent *)

(**
   {b MLSTATE_SSL_DEBUG}
*)
val ssl_debug : debug_var

(**
   {b MLSTATE_TESTING}
   This is set by the reftester. Only use to disable the printing of some
   messages that are normally useful but very troublesome for the stability
   of refs (eg. database locations...).
*)
val testing : debug_var

(**
   {b MLSTATE_TYPER}
   Works with level.
   Debugging the type checker.
*)
val typer : debug_var

(**
   {b MLSTATE_RESOURCE_TRACKER_DEBUG}
*)
val resource_tracker_debug : debug_var


(**
   {b MLSTATE_WEBLIB_DEBUG}
*)
val weblib_debug : debug_var (** takes a level *)

(**
   {b BADOP_XML_IMPORT_COMMIT_TRANSACTION_STEP}
   Only used by db3/Xml_import, to decide the frequency of commits.
*)
val badop_xml_import_commit_transaction_step : debug_var


(** {6 Variables Testers} *)

(**
   Variables tester are used to extend the macro [#<If>] of the ppdebug.
   You can acces them by using the following syntax :
   [#<If:MYVAR $tester args>]
   where args is some optional arguments.

   The meaning of this is that the corresponding tester is applied to all the
   given arguments, and finally to the value of the debug_var,
   returning a bool indicating what part of the control structure should
   be executed.

   Note :
   You can also used inline tester in your code, but it is not recommanded (remember,
   that must fit on the same line)
*)

(**
   The type of all testers
*)
type debug_tester = debug_var -> bool

(** the default tester.
    [#<If:MYVAR>] : returns [true] if and only if the variable is defined and doesn't have value "0" *)
val default : debug_tester

(**
   the negation of the default tester.
   [#<If:MYVAR $null>] : returns [true] if and only if the variable is undefined or have value "0"
*)
val null : debug_tester

(** the defined tester.
    [#<If:MYVAR$defined>] : returns [true] if and only if the variable is defined *)
val defined : debug_tester

(**
   the undefined tester.
   [#<If:MYVAR $undefined>] : returns [true] if and only if the variable is undefined
*)
val undefined : debug_tester

(** the toggle tester.
    [#<If:MYVAR$toggle>] : returns [true] if and only if the variable is set to ["1"] *)
val toggle : debug_tester

(** the equals tester.
    [#<If:MYVAR $equals "VALUE">] : returns [true] if and only if the variable is set to ["VALUE"] *)
val equals : string -> debug_tester

(** the level tester :
    [#<If:MYVAR $level fct>] : returns [true] if and only if the variable is set to an int
    value [n], and [fct n] returns [true] *)
val level : (int -> bool) -> debug_tester

(**
   A specific level.
   [#<If:MYVAR $islevel 42>] : returns [true] if and only if the variable is set to "42"
*)
val islevel : int -> debug_tester

(** the maxlevel tester :
    [#<If:MYVAR $maxlevel n>] : like [#<If: MYVAR $level (fun i -> i <= n)>] *)
val maxlevel : int -> debug_tester

(** the minlevel tester :
    [#<If:MYVAR $minlevel n>] : like [#<If: MYVAR $level (fun i -> i >= n)>] *)
val minlevel : int -> debug_tester

(** the contains tester :
    checks that the given string is included in the environment variable *)
val contains : string -> debug_tester

(**
   free debug_tester, for complex conditions
*)
val cont : debug_tester -> debug_tester

(** checks that the environment variable is included in the given string *)
val is_contained : string -> debug_tester

(** contents of the variable is taken as a comma-separated list; checks if
    the given flag exists in this list *)
val flag : string -> debug_tester
