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

(**
   The Core of the Bypass Standard Library.

   See the manual for more documentation.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(**
   TODO: defunc.
   Although it brings type safety about its manipulation,
   it is too complex.

   The table should be shared between different instance of back-end,
   since we have a lot of backends with which we may choose to togle
   dynamically.
*)

(* Implementation Note :
   the modules in the implementation can share the abstract types because
   there is no coercion of module there,
   until the final compilation of the module BslLib *)

open BslInterface

(** {6 Parser} *)

(** This module offers a High-level API for calling the [BslRegisterParser] module (trx) *)
module HLParser :
sig

  (**
     The list of all include formats used by default by the parser of {b bslregister}
     for processing {b opa} files.
  *)
  val default_opa_iformats : (string * string) list

  (**
     Add some more format to the table.
  *)
  val add_iformat : ('a * string) list -> unit (** suggar : call format *)

  (**
     Print all currently loaded iformats.
  *)
  val show_iformats : Format.formatter -> unit -> unit

  (** {6 Exported rules} *)

  (**
     Parse a directive available in an Opa file.
  *)
  val opalang_directive : string -> (BslTags.parsed_t, BslDirectives.opalang_directive) BslDirectives.decorated_source_elt

  (**
     Parse a directive available in a bypass file. (Ocaml, Javascript)
  *)
  val bypasslang_directive : string -> (BslTags.parsed_t, BslDirectives.bypasslang_directive) BslDirectives.decorated_source_elt

end

module Arg :
sig

  (**
     Defines some common options.
     + [--show-iformats] for printing the default iformats available in opa.
     {[
     ##include <format> path
     ]}
  *)
  val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list
end

(** {6 Generation} *)

(**
   Get the map of types, usefull to generate projection code.
   If runtime is set to true, the module will appear in the Runtime.Module.Path
*)
val record_path_map_of_typesmap : ?complete:bool -> ?runtime:bool -> typesmap -> string StringMap.t

(** ----- *)
(** Extra tools on type generation with ocaml *)
val map_type_from_type_map_and_path : typesmap -> string list -> ?definition:bool -> BslTypes.t -> BslTypes.t

(** A sugar to get the string boxed in a meta_code *)
val meta_code : meta_code -> string

(** {6 Bypass scopes} *)

(**
   This function is used for warning the user when some import-plugin are missing
   in the code, and detected thanks to the autobuild mode
*)
val declare_visibility : ObjectFiles.package_name -> BslPluginInterface.plugin_basename -> unit

(** {6 Constructors of custom BSL} *)

(** Creating a new BSL from all I,CTrans. This is the constructor for advanced utilisation *)
module MakeLibBSL :
  functor (ML_ITrans : ML_ITRANS) ->
  functor (ML_CTrans : ML_CTRANS) ->
  functor (JS_CTrans : JS_CTRANS) ->
  BSLINTROSPECTION with type ml_ctrans_env = ML_CTrans.env
                   and  type js_ctrans_env = JS_CTrans.env

(** Dummy modules - for simple users, like bslbrowser - or some verificator *)
module Dummy_ML_ITrans : ML_ITRANS
module Dummy_ML_CTrans : ML_CTRANS
module Dummy_JS_CTrans : JS_CTRANS

(** If you write a qml compiler, you don't need the tools on transtyping on dynamic loading
    but you do not take care on JS trans.
    This is built with MakeLibBSL with all Dummy_* but ML_CTRANS *)
module LibBSLForQml2Ocaml :
  functor (ML_CTrans : ML_CTRANS) ->
  BSLINTROSPECTION with type ml_ctrans_env = ML_CTrans.env

(** If you write a js compiler, you don't need the tools on transtyping on dynamic loading
    but you do not take care on ML trans.
    This is built with MakeLibBSL with all Dummy_* but JS_CTRANS *)
module LibBSLForQml2Js :
  functor (JS_CTrans : JS_CTRANS) ->
  BSLINTROSPECTION with type js_ctrans_env = JS_CTrans.env

(** If you write a interpreter, you don't need the tools on auto code generation.
    Currently, qmltop does not use ML_ITRANS, and does not use this module *)
module LibBSLForQmlTopLevel :
  functor (ITrans : ML_ITRANS) ->
  BSLINTROSPECTION

(** A BSL for all use but the transtyping tools *)
module BSL : BSLINTROSPECTION

(** A type used several time in the framework, mostly by backends *)
type env_bsl =
    {
      bymap : BSL.ByPassMap.t ;
      plugins : BslPluginInterface.plugin list
    }

(** {6 Extra common tools for projection} *)

(** A common tools for the ML_CTrans
    This function is shared by the two translation functions
    _qml_of_ocaml and _ocaml_of_qml
    It is used to project functions.
    Sample of generated code :
    [  let f' x1 x2 x3 =
       let r = f x1 (px2 x2) x3 in
       pr r in f')
    ]
 *)
val ml_function_projection :
  inputs:(BslTypes.t -> meta_ident -> meta_code option) ->
  outputs:(BslTypes.t -> meta_ident -> meta_code option) ->
  BslTypes.t list -> BslTypes.t -> meta_ident -> meta_code option


(** A common tools for the JS_CTrans *)
val js_function_projection :
  inputs:(BslTypes.t -> meta_ident -> meta_code option) ->
  outputs:(BslTypes.t -> meta_ident -> meta_code option) ->
  BslTypes.t list -> BslTypes.t -> meta_ident -> meta_code option
