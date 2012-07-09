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
(**
   OpaTop main starter.
   @author Mathieu Barbin.
*)

(**
   This file is the common main for any opatop using external loaders.
   The code of an opatop should be:
   {[
   (* Generated opatop with opa-plugin-maker *)
   let _ =
      Loader1.Self.self_store ();
      Loader2.Self.self_store ();
      OpaTopMain.main ()
   ]}
   where [Loader_] are the loader built by [bslregister] from the
   external libraries (bsl plugins).
*)

module String = Base.String

(** {6 Options} *)
(**
   Since opatop is a runtime tool, linked with the application server suite,
   it shares the option system of servers.

   In this way, we may use the command line options to set options of weblib,
   the db, etc...

   But the interpreter is also a compiler like tool, because it contains some
   rewritting rules, and trate code. So, some options are also binded there
   (like OManager, etc..)
*)

(*
   Following the guidelines for Arguments.
   Cf also in :
  - libbsl/bslbrowser.ml
  - libbsl/bslregister.ml

  TODO: apply the same guidelines in every applications.
*)


(* d *)


let do_init = ref true
let do_input = ref false


let dump_files = ref false
let dump_stdlib = ref false

(* f *)


let fatal = ref false


(* g *)


let greedy = ref false


(* u *)


let user_files = MutableList.create ()
let user_files_add opafile = MutableList.add user_files opafile


(* parse *)

module P = OpaTopProperties
module Sa = ServerArg

let (!>) = Base.Format.sprintf

(**
   Spec list for parsing.
*)
let spec = [


  (* - *)


  ["--"],
  (fun () -> Sa.skip_all),
  "",
  !>
    "Pass any remaining options to the application" ;


  (* d *)


  (* FIXME: use a ppdebug variable instead of this hacky option *)
  ["--dddbgen"],
  Sa.func Sa.unit (fun () () -> P.dddbgen_set true),
  "",
  !>
    "Start with dbgen-dumper on" ;


  ["--dump-files"; "-e"],
  Sa.func Sa.unit (fun () () -> dump_files := true),
  "",
  !>
    "Dump (types + values) the loaded files" ;


  ["--dump-stdlib"],
  Sa.func Sa.unit (fun () () -> dump_stdlib := true),
  "",
  !>
    "Dump (types + values) the stdlib" ;


  (* f *)

  ["--fatal-mode"],
  Sa.func Sa.unit (fun () () -> fatal := true),
  "",
  !>
    "Loading stdin, stop the interpreter with the first error. This is the default behavior for the stdlib and files";


  (* g *)


  ["--greedy"],
  Sa.func Sa.unit (fun () () -> greedy := true),
  "",
  !>
    "Loading stdlib and files, evaluate as much as possible and do not stop at the first error. This is the default behavior for stdin";


  (* i *)


  ["--input"],
  Sa.func Sa.unit (fun () () -> do_input := true),
  "",
  !>
    "After loading given file(s), don't quit and continue with stdin" ;


  (* n *)


  ["--no-assert"],
  Sa.func Sa.unit (fun () () -> P.assert_set false),
  "",
  !>
    "Start with assert off" ;


  [ "--no-stdlib" ],
  Sa.func Sa.unit (fun () () -> do_init := false),
  "",
  !>
    "Do not load the opalight stdlib" ;


  (* o *)


  ["--opa"],
   Sa.func Sa.string (fun () -> user_files_add),
   "<file>",
   !>
     "Load a file as an opa source" ;

  (* v *)

  ["--value-restriction"],
  Sa.func_opt Sa.string (fun () s ->
                           match s with
                           | "disabled" -> P.value_restriction_set `disabled; Some ()
                           | "normal" -> P.value_restriction_set `normal; Some ()
                           | "strict" -> P.value_restriction_set `strict; Some ()
                           | _ -> None),
  "{disabled|normal|strict}",
  !>
    "Set the kind of value restriction" ;

]
  @ ( Sa.import_arg_options OManager.Arg.options )
  @ ( Sa.import_arg_options [OManager.Arg.version "opatop"] )
  @ ( Sa.import_arg_options WarningClass.Arg.options )
  @ ( Sa.import_arg_options OpaSyntax.Args.options )


(**
    Loading opatop warnings
*)
let _ =
  let warning_set =
    let s = WarningClass.Set.create () in
    (* let (!+) w = WarningClass.Set.add s w in *)
    let (!++) s' = WarningClass.Set.add_set s s' in
    !++ QmlTyperWarnings.warning_set;
    s
  in
  WarningClass.load_set warning_set

(**
   Anon function for non --option arguments
*)
let anon_fun opafile =
  if String.is_suffix ".opa" opafile then user_files_add opafile else (
    if String.is_prefix "-" opafile then (
      OManager.printf "Invalid option @{<bright>%S@}@\n" opafile;
      OManager.printf "@[<2>@{<bright>Hint@}:@\nTry @{<bright>--help@} for more details.@]@\n";
      ()
    ) else (
      OManager.printf "Argument @{<bright>%S@} does not have the @{<bright>.opa@} extension@\n" opafile;
      OManager.printf "@[<2>@{<bright>Hint@}:@\nIf this is really an opa file,@ you can use the option@ @{<bright>--opa %s@}@\nTry @{<bright>--help@} for more details.@]@\n" opafile;
      ()
    );
    OManager.error "command line error"
  )

(** {6 Main} *)


let with_classic_syntax f =
  let opa_parser = (!OpaSyntax.Args.r).OpaSyntax.Args.parser in
  (* the libs of opatop are still in classic syntax *)
  OpaSyntax.Args.r := {!OpaSyntax.Args.r with OpaSyntax.Args.parser=OpaSyntax.Classic};
  let v = f () in
  OpaSyntax.Args.r := {!OpaSyntax.Args.r with OpaSyntax.Args.parser=opa_parser};
  v

(**
   The main of the console tool.
*)
let main () =
  (try ServerArg.filter () (ServerArg.make_parser ~final:true "opatop options" spec)
   with Exit -> exit 1);
  ServerArg.filter () (ServerArg.fold (ServerArg.func ServerArg.anystring (fun () -> anon_fun)));
  OpaTopEnv.set_directive_handler OpaTopDirectives.handler;
  let env = OpaTopEnv.start () in
  (* for init and user files, be greedy only if the option as asked *)
  P.greedy_set !greedy;
  P.dump_set !dump_stdlib;
  (* init *)
  let env =
    (* the libs of opatop are still in classic syntax *)
    if !do_init then with_classic_syntax (fun () ->
      let loaders = Option.default [] (BslPluginTable.last_finalize ()) in
      let fold env loader =
        let fold env (filename, contents) =
          OManager.verbose "load file @{<bright>%S@} ..." filename;
          let env = OpaTopEnv.set_filename env filename in
          let env = OpaTopEnv.input_contents env contents in
          env
        in
        List.fold_left fold env loader.BslPluginInterface.opa_code
      in
      let env = List.fold_left fold env loaders in
      env
    ) else env
  in
  (* user files *)
  P.dump_set !dump_files;
  let env =
    MutableList.fold_left OpaTopEnv.input_file env user_files
  in
  (* for input, be greedy, unless the option fatal was asked *)
  P.greedy_set (not !fatal);
  (* input *)
  if MutableList.length user_files = 0 || !do_input then (
    OManager.oformatter := Format.std_formatter;
    OManager.this_is_tool ~force:true "opatop";
    OManager.printf "This is an experimental interpretation loop for opa. Type '#help;;' to know more.@\n";
    OManager.oformatter := Format.err_formatter;
    P.dump_set true;
    let env = OpaTopEnv.set_filename env "stdin" in
    let _ = OpaTopEnv.input_loop env stdin in
    print_newline ()
  ) else (
    ()
  )

(** Output a manpage file *)
let write_manpage file =
  ServerArg.write_simple_manpage
    ~cmdname:"opatop"
    ~summary:"The Opa top-level"
    ~section:1
    ~centerheader:"Opa Manual"
    ~synopsis:"opatop [options]"
    ~description:"Opatop is an experimental interpretation loop for opa. Type '#help;;' to know more."
    ~options:spec
    ~other:["NOTE","Opatop also accept some options inherited from the Opa platform. Run 'opatop --help' for details."]
    file

