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

(* depends *)
module Arg = Base.Arg
module String = Base.String

type makefile_rule =
  | Bytecode
  | Native
  | Bytecode_and_native
  | Bytecode_or_native

(**
    Command-line options for the Qml to OCaml compiler.


    @author Mathieu Barbin
    @author David Rajchenbach-Teller (Additional options)
*)

(** STATICS PARAMETERS *)

module StaticParameters =
struct
  (** the other values are not mutable -- cf opa options -I -x to add some more libraries *)
  let options_compiler =
    ["-w a"]

  let options_linker =
    ["-w a"]
    @ (if Base.is_windows then
         ["-cclib"; "Dnsapi.lib"; "-cclib"; "libeay32.lib"; "-cclib"; "ssleay32.lib" (*; "ssl_stubs.obj" *)]
       else [])

  (**
     Absolute path for include directory, will be passed with the option -I to the ocaml compiler.
  *)
  let server_include_dir = [
    "+zip" ; "+site-lib/zip" ; "+site-lib/camlzip" ;
    "+ssl" ; "+site-lib/ssl" ;
    "+cryptokit"; "+site-lib/cryptokit" ;
    "+ulex" ; "+site-lib/ulex" ;
  ]

  (**
     Like [server_include_dir] but the path is not absolute, but relative to MLSTATELIBS directory.
     The following directories will be prefixed by MLSTATELIBS before passing to ocaml compiler with -I
  *)
  let server_include_mlstate_dir =
    InstallDir.lib_opa ::
    (
      if Base.is_windows
      then
        (*
          FIXME
          What is the purpose of this ?
          This will be concatenated with a Filename.concat anyway
        *)
        if BuildInfos.is_release
        then ["windows_libs/openssl/lib"]
        else ["/windows_libs/openssl/lib"]
      else []
    )

  (**
     The list of ocaml libraries. Will be suffixed by ".cma" or ".cmxa" depending of the option --bytecode.
     Theses libraries should be installed in one of the directory reachable by option -I (server_include_dir & server_include_mlstate_dir)
  *)
  let common_server_libraries = [
    "str" ;
    "unix" ;
    "bigarray";
    "ssl" ;
    "zip" ;
    "nums" ;
    "ulexing" ;
    "buildinfos" ;
    "libbase" ;
    "libtrx" ;
    "libruntime" ;
    "appruntime" ;
    "libtools" ;
    "libsecurity" ;
    "libnet" ;
    "libirc" ;
    "database" ;
    "oparuntime";
    "libsession" ;
  ]

  let qml_server_libraries = [
    "qmlflat", common_server_libraries @ [ "qmlcpsserver" ; "qmlflatserver"; "opabsl_for_server" ];
    "qmlfake", common_server_libraries @ [ "qmlcpsserver" ; "qmlfakeserver"; "opabsl_for_server" ];
  ]

  let opa_server_libraries =
    [ "cryptokit" ; "opabsl_for_server" ]

  (** the 2 following options will be used only in the case of resp bytecode / native *)
  let extra_bytecode_options = [ "-g" ]
  let extra_native_options = [ "-g" (* "-compact" *) ]
end

type input_file = QmlFile of string | OpaFile of string
let input_filename = function
  | QmlFile f
  | OpaFile f -> f

(*
  TODO
  for historic reasons, this used to be the options for the OPA compiler.
  now, this correspond only to the options used by the ocaml back-end.
  The current status is messy:
  -lots of these options are unused (never set, never read).
  -lots of these options are override by opa/passes.ml/pass_OpaOptionsToQmlOptions
  a big clean-up is expected
*)
type argv_options = {
  (*
    1) Options used by passes after qmlcompilation
  *)
  camlp4o : bool ;
  check_lib : bool ;
  compilation_directory : string ;
  compile_via_makefile : bool ;
  exe_run : bool ;
  exe_argv : string list ;
  extra_lib : string list ;
  extra_path : string list ;
  hacker_mode : bool ;
  makefile_max_jobs : int ;
  makefile_rule : makefile_rule;
  mlcopt : string list ;
  mllopt : string list ;
  no_move : bool ;
  ocamlc : string ;
  ocamlopt : string ;
  profile : bool ;
  show_compilation : bool ;
  target : string ;

  (*
    2) Options used by the pass qmlcompilation
  *)
  cps : bool ;
  no_assert: bool;
  qml_closure: bool;
  top_magic : bool ;

  (*
    3) Used by the command line tool qmljs
    This should be refactored, somehow
  *)
  bypass_plugin : string list ;
  cps_toplevel_concurrency : bool ;
  constant_sharing : bool ;
  input_files : input_file list ;
  lambda_lifting : bool ;
  no_stdlib : bool ;

  (*
    4) trash
  *)
  display_schema : string option ;
  server_side : bool ;
  split_ocaml_value : int ;
}

module ArgvOptions :
sig
  val default : string -> argv_options
end
  =
struct

  let default backend = {
    bypass_plugin = [] ;
    camlp4o = false ;
    check_lib = false ;
    compilation_directory = "" ; (* set by opa *)
    compile_via_makefile = false ;
    constant_sharing = false ;
    cps = false ;
    cps_toplevel_concurrency = false ;
    server_side = true ;
    display_schema = None ;
    exe_argv = [] ;
    exe_run = false ;
    extra_lib = (
      let qml = List.assoc backend StaticParameters.qml_server_libraries in
      let opa = StaticParameters.opa_server_libraries in
      qml@opa
    ) ;
    extra_path = [] ;
    hacker_mode = false ;
    input_files = [] ;
    lambda_lifting = false ;
    makefile_max_jobs = 7 ;
    makefile_rule = Native ;
    mlcopt = StaticParameters.options_compiler ;
    mllopt = StaticParameters.options_linker ;
    no_assert = false ;
    no_stdlib = false ;
    no_move = false ;
    ocamlc = "" ; (* set by opa *)
    ocamlopt = "" ; (* set by opa *)
    profile = false ;
    qml_closure = false ;
    show_compilation = false ; (* set by opa *)
    split_ocaml_value = 100 ; (* set by opa *)
    target = "" ; (* set by opa *)
    top_magic = true ; (* set by opa *)
  }
end
