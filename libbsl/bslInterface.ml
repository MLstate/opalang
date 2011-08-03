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
   The main interfaces of the Bypass Standard Library.

   See the manual for more documentation.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(* This file has no mli to avoid the duplication of sig and types definitions *)

(** {6 Type alias} *)
type deprecated = string
type skey = string

(** {6 Public Types Definitions} *)

(** The hierarchy in the bsl library : like a files system,
    modules are directories and functions are files *)
type ('a, 'b) kind = Function of 'a | Module of 'b

(** A type used to export bypass for a hierarchical browsing structure *)
type ('a, 'b) hierarchy = HFunction of 'a | HModule of ( 'b * ('a, 'b) hierarchy list )

(**
   A map from type (name) to path and type representation.

   Note that the path is represented as lowercase. If you wish to use this in OCaml,
   you will need to (re)capitalize things.

   The first string is the name of the MLRuntime
*)
type typesmap_elt = deprecated * skey list * BslTypes.t
type typesmap = typesmap_elt BslKeyMap.t

type bypass_tags = BslTags.parsed_t


(** {6 Transtyping Modules} *)

(**
    The bsl module is a functor which take a module in arg called the {b CTrans}.

    The system of bypass-projections can be resume so :
    + tell the way you represent the opa-values in your compiler by implementing a {b CTrans} module
    + use this CTrans module as an argument of the corresponding functor to build a custom BSLINTROSPECTION module
    + register all primitives you need using this custom BSLINTROSPECTION module
    + use functions of the API to get the generated projected code corresponding to fresh bypasses,
    with the property that they could be applied directly on values of your runtime type algebra.

    For more flexibility, during the projection, you may propagate an environment. (see interface of [CTrans] modules)

    Finally, note that if you don't need to generate code, and you're interressed only by some informations about bypass,
    there is a bsl built by defaut, which is not specialized for any code generator : see module [BSL]
    (the functor is applied with dummy arg-modules).
*)

(** an uniq identifiant for every record defined in bypass implementation *)
type record_t = string

(** indexation in record local index in the record and name of field *)
type field_t = int * string

(** a ident used in projection code *)
type meta_ident = MetaIdent of string

(** a string in the syntax of the target language *)
type meta_code_expr = MetaCodeExpr of string

(** the type returned by the CTrans about projection call *)
type meta_code =
  | MetaCode of string    (** a meta code expressed in the target language *)
  | MetaComment of string (** just a comment to help the anderstanding of the projection code *)

type ('env,'code) bsl_projection = BslTypes.t -> env:'env -> meta_ident -> 'env * 'code option

(* TODO: simplify ITrans ? *)

(** Version of CTrans for qml interpreters *)
module type ML_ITRANS =
sig
  (**
     This functionnality will probably be removed.
     @deprecated too complex, and probably not really needed
  *)

  (* Parametrisation of bsl : do you want that the bsl use your module ? *)
  val auto_transtype : bool
  val record_clash : record_t -> record_t -> 'a
  val unbound_record : record_t -> 'a (** you could for example raise an exception *)

  (* standard conversion *)
  type qml           (* typing of record if unable return None *)
  val qml_of_ocaml : BslTypes.t -> ('a -> qml) option
  val ocaml_of_qml : BslTypes.t -> (qml -> 'a) option

  (* extra dynamic introspection on record *)
  val type_of_record : qml -> record_t (* or raise exception *)
  val get_field : qml -> field_t -> qml
  val get_field_opt : qml -> field_t -> qml option
  val build_record : record_t -> (field_t * qml) list -> qml
end

(** Version of CTrans for qml2ocaml compilers *)
module type ML_CTRANS =
sig
  type env
  val empty : unit -> env
  (** standard conversion   same as in the interpreted  *)
  (** at projection time, we give to the CTrans the bslkey where the projection is needed and we give also the identifier of what is going to be projected (meta_ident) *)
  val qml_of_ocaml : bslkey:BslKey.t -> bsltags:BslTags.t -> (env,meta_code) bsl_projection (* $('a -> qml)$ : None for identity *)
  val ocaml_of_qml : bslkey:BslKey.t -> bsltags:BslTags.t -> (env,meta_code) bsl_projection (* $(qml -> 'a)$ : None for identity *)

  val runtime_ocaml_coercion : bslkey:BslKey.t -> bsltags:BslTags.t -> BslTypes.t -> env:env -> string -> env * string
    (* the coercion with your runtime representation of qml-types
       if you don't you want fun _ s -> s , else fun t s -> "(s : t)" *)

  (** accumulator of conversion code ( you can initialize it with fun env -> env, "" *)
  (** this function will be called at end of all generation, to be able to produce good links *)
  val conversion_code : env -> env * string

  (**
     Additional arguments to add on projection.
     This is used for second-order bypass in Ocaml, for adding an extra continuation
     argument, used in projection of functionnal arguments, to be passed to the functions
     uncps.

     This code is inserted there :
     {[
     let bslp36 x0 x1 x2 <THERE> =
     ]}
     which is the place for adding a 'k' argument.
  *)
  val more_args : BslKey.t -> BslTags.t -> env -> string option

  (**
     Additional code add on projection.
     This is used for second-order bypass in Ocaml, for adding a let binding introducing the
     bslkey of the bypass we are projecting, essentially for keeping trace of the stack of blocking_wait,
     to be passed to the projection functions uncps.

     This code is inserted there :
     {[
     let bslp36 x0 x1 x2 =
       <THERE>
       ...
     ]}
     which is the place for adding :
     {[
     "let bslkey = \"mybslkey\" in"
     ]}
  *)
  val more_code : BslKey.t -> BslTags.t -> env -> string option

  (**
     Combined with more_args, the [return] function is used to apply a function to
     the result of bypass. Typically, this is a [Continuation.return]

     This code is inserted there :
     {[
     let bslp36 x0 x1 x2 =
       let r = ... in
       <THERE>(r)
     ]}
     which is the place for adding :
     {[
     "QmlCpsServerLib.return k r"
     ]}
  *)
  val return : BslKey.t -> BslTags.t -> env -> meta_ident -> string option
end

(** Version of CTrans for qml2js compilers *)
module type JS_CTRANS =
sig
  type env
  val empty : unit -> env
  val qml_of_js : bslkey:BslKey.t -> bsltags:BslTags.t -> (env,(JsAst.ident list * JsAst.expr)) bsl_projection
  val js_of_qml : bslkey:BslKey.t -> bsltags:BslTags.t -> (env,(JsAst.ident list * JsAst.expr)) bsl_projection
  val conversion_code : env -> env * (string(*unicity_index*) * JsAst.statement) list
end

(** {6 Bsl Main Modules Interfaces} *)

(**
   During the jscleaning (runtime-init)
   some js-elt are duplicated.
   We use this unicity_index for keeping only
   one version of each key.

   Property:
   even if the index is generated in two different
   packages compilation, the index should be the same
   in case of duplicated elts.
*)
type unicity_index = string

(** The interface of BSL like modules. This is the core of {b libbsl} *)
module type BSLINTROSPECTION =
sig
  (** {6 Functionnal API} *)

  (** the type of the env of the ml_ctrans used to build this module *)
  type ml_ctrans_env

  (** the type of the env of the js_ctrans used to build this module *)
  type js_ctrans_env

  (** Manipulation of bypasses implementations *)
  module Implementation :
  sig
    (**
       Implementation expressed as concrete syntaxe.
       e.g.:
       {[
       "my_bsl_function"
       ]}
       {[
       "Pervasives.(+)"
       ]}
    *)
    type function_name = string

    type compiler_repr =
      | Ident of Ident.t
      | String of string

    (** The abstract type of a compiled implementation *)
    type compiled

    (** The abstract type of a interpreted implementation. This means that we have an ocaml implementation available,
        and we could get dynamically a pointer of the function *)
    type interpreted

    (** The abstract type of a implementation *)
    type t = private Compiled of compiled | Interpreted of interpreted

    (** Get the language of an implementation *)
    val lang : t -> BslLanguage.t

    (**
       Get the type of an implementation.
       Several implementations of the same bypass should have the same type.
       However, these types could be protected by the [opa[]] constructor to tell
       if the bypass works either on standard values, or projected values
       (depending on the runtime algebra of the compiler)
    *)
    val trans_type : t -> BslTypes.t

    (**
       Get the tags defined in the implementation of the bypass.
       [##register [...mytags...] etc...]
       Since the tags can be different in each language of implementation,
       this is not a function of the module [ByPass]. *)
    val bsltags : t -> BslTags.t

    (** Manipulation of [Compiled] implementations *)
    module CompiledFunction :
    sig
      (**
         This is the core of the {b bsl keys resolution}.
         When a compiler based on libbsl meet a bypass, after finding it from
         its key in the bymap, the key should be replaced by the effective code
         of the implementation of the bypass, in each target language (Ocaml, Js).
         This function returns the code which must be inserted in the
         generated code, expressed as concrete syntax.
         This can be either :
         + the name of the implementation, as it was defined in the bsl
         + a row string, if the bypass was defined with the injected syntax, like in
         {[
         ##register add_int \ `Pervasives.(+)` : int, int -> int
         ]}
         + the name of a fresh function which has been regenerated because of
         bsl-projection system (CTrans).
         In this case, the code of the function can be found in the
         [language_init] string (bsl_ocaml_init.ml, bsl_js_init.js, ...)
      *)
      val compiler_repr   : compiled -> function_name

      val compiler_detailed_repr : compiled -> compiler_repr

      (**
         Is stident is case of an Ident.
         Ocaml:
         For the Ocaml, Ident.stident is the function used in fine
         anyway, and actually this is never called since we do not
         use anything but String case for the Ocaml.
         Js:
         This function is not called, the jsfun uses directly the detailed
         representation for inserting projected bypass as ExprIdent nodes,
         so that the runtime renaming is consistent.
      *)
      val string_of_repr : compiler_repr -> string

      (** Get the filename where the implementation was declared. *)
      val origin_file     : compiled -> string

      (** Given a compiled function, you may want to know if it needed a {b bsl projection}.
          It depends directly on what returned the {b CTrans} module where it was called with
          the original type of the bypass.*)
      val is_transtype    : compiled -> bool

      (** Get the name of the module where the implementation has been register.
          Used for ocaml only. *)
      val origin_module   : compiled -> string
    end

    (** From a ocaml interpreted implementation, get its function pointer.
        This functionnality is available only if a [*Loader] module has
        been generated by {b bslregister}
        and statically linked with the executable calling this function.

        This is how {b opatop} find its primitives.

        Note : with [Interpreted] implementations, there is just this
        function to export, that's why there is no module [InterpretedFunction] *)
    val dynamic_repr : interpreted -> Obj.t

    (** From an implementation, get a printable information for debug (used e.g. by {b bslbrowser})*)
    val pp : t LangPrint.pprinter
  end

  (** Manipulation of bypasses *)
  module ByPass :
  sig
    (** The abstract type of the bypass *)
    type t

    (** Get the key of a bypass *)
    val key : t -> BslKey.t

    (** shorthand for [BslKey.to_string (ByPass.key t)] *)
    val skey : t -> string

    (** Get the just the name of the bypass, without its module hierarchy, e.g :
        ["string.init" --> "init"] *)
    val name : t -> string

    (** [format t fmt] print a bypass using a bsl-format.
        Documentation of format is not yet available.
        @param t a bypass
        @param fmt a format *)
    val format : t -> string -> string

    (** Get the implementation of a desired language, if available (otherwise, you'll get [None]). *)
    val implementation : t -> lang:BslLanguage.t -> Implementation.t option

    (** Get all the implementations of a bypass *)
    val all_implementations : t -> Implementation.t list

    (** Get all the compiled implementations of a bypass *)
    val compiled_implementation : t -> lang:BslLanguage.t -> Implementation.compiled option

    (** Get all the interpreted implementations of a bypass *)
    val interpreted_implementation : t -> lang:BslLanguage.t -> Implementation.interpreted option

    (** Get the list of all languages implementing this bypass *)
    val langs : t -> BslLanguage.t list

    (** Check if a bypass is implemented in the asked language *)
    val implemented_in : t -> lang:BslLanguage.t -> bool

    (** Check if a bypass is implemented in {b at least one} asked language *)
    val implemented_in_any : t -> lang:BslLanguage.t list -> bool

    (** Check if a bypass is implemented in {b every} asked language *)
    val implemented_in_all : t -> lang:BslLanguage.t list -> bool

    (** Get the original type of the bypass, as it was defined in the implementation
        and registred by [##register].
        All implementation have the same registred type, that is checked by {b bslregister}.
        <!> Beware, note that if the bypass needs a bsl-projection is one of
        the implementation language (CTrans),
        the type of the projected bypass may change
        @see "Implementation.trans_type" to get the type of the projected bypass *)
    val definition_type : t -> BslTypes.t
  end

  (** Functionnal structure to manipulate a collection of [ByPass.t] *)
  module ByPassMap :
  sig

    (** The abstract type of a bypass map *)
    type t

    (** The empty val is used just for typing purposes,
        if you need to defined a default value (e.g. in a record) before building any map *)
    val empty : unit -> t

    (** At end of generation, the final ctrans_env are contained in the bymap. *)
    (** the resulting ml_ctrans env *)
    val ml_ctrans_env : t -> ml_ctrans_env
    (** the resulting js_ctrans env *)
    val js_ctrans_env : t -> js_ctrans_env

    (** {6 Type definitions} *)
    (** The types of the bymap are every types used in all interfaces of loaded bypass.
        + record types
        + abstract types

        For using bypasses having these types in their interface, you need to define
        these types in opa. These functions give facilities to generate these
        definitions, e.g. the directive [##include-type ...] handled by
        {b bslregister} uses these functions. *)

    val types : t -> typesmap_elt list
    val typesmap : t -> typesmap

    (** fold on every types *)
    val fold_types : t -> ('a -> BslTypes.t -> 'a) -> 'a -> 'a

    (** export all needed types definitions in opa syntax *)
    val opa_types : t -> string

    (** {6 Bsl-Projection} *)
    (** Using the {b CTrans}, some bypass have been regenerated. The code of this
        generation can be found in
        a string available for each language, called the ["bsl-init"] string.
        + The compilers 2ocaml produce this string in a file called ["bsl_ocaml_init.ml"].
        + The compilers 2js produce this string in a file called ["bsl_js_init.js"],
        or produces this string at the end
        of the uniq generated file if the generated js is not splitted into small parts.
    *)

    (** Get the bsl-init for ocaml. *)
    val ocaml_init : t -> string

    (**
       Get the bsl-init for js in ast format, and each
       code elt is associated to the key of the bypass
       it is redefining. (used by js serializer).

       For the serializer to work properly, the ident of the elt
       should be a ExprIdent. (cf JsAst)
    *)
    val js_init : t -> (unicity_index * JsAst.code_elt) list

    (** {6 Introspection & iterators} *)

    (** find_opt *)
    val find_opt : t -> ?lang:BslLanguage.t -> BslKey.t -> ByPass.t option
    (** suggar of interface : *)
    val find_opt_implementation : t -> lang:BslLanguage.t -> BslKey.t -> Implementation.compiled option
    val find_opt_interpreted_implementation : t -> lang:BslLanguage.t -> BslKey.t -> Implementation.interpreted option
    val iter : (BslKey.t -> ByPass.t -> unit) -> t -> unit
    val fold : (BslKey.t -> ByPass.t -> 'a -> 'a) -> t -> 'a -> 'a

    (** more introspection, get the map part of t structure : *)
    val get_map : t -> ByPass.t BslKeyMap.t

    (** {6 Link with the qml typer} *)

    (** you can get the bypass_typer function with a partial call here *)
    val bypass_typer : ?typeident:(?check:bool -> string -> QmlAst.TypeIdent.t) -> t -> BslKey.t -> QmlAst.ty option

    (** in some cases, you can prefer dealing with BslTypes.t instead of QmlAst.ty *)
    val bsl_bypass_typer : t -> BslKey.t -> BslTypes.t option

    (**
       Get tags from a bypass map.
       If the bypass is not found, or if it is not implemented in this language,
       the function returns [None]
    *)
    val bsl_bypass_tags : t -> lang:BslLanguage.t -> BslKey.t -> BslTags.t option

    (** Get the corresponding cps bypass of a bypass if exists. This
        cps bypass must be named like the original bypass and suffixed
        by "_cps". It must tagged by [cps-bypass] and the type must
        corresponding. *)
    val bsl_bypass_cps : t -> lang:BslLanguage.t -> BslKey.t -> BslKey.t option

    (** {6 Facilities for browsing the bypass library} *)
    (** This module offers a cyclic functionnal data structure : starting from each
        elt you can browse all the structure
        by accessing children and parents of every elt. *)
    module Browser :
    sig
      (** If you want to work with the library, you can export the bsl representation
          to this type *)
      type bypass_library = (ByPass.t, string) hierarchy

      (** uniq identifiant for every module or function :e.g.  /module1/module2/function
          or module3 *)
      type path

      (** an elt is a module, or a bypass *)
      type elt
      type module_elt
      type public_elt = (ByPass.t, module_elt) kind

      (** Build the root node constructor (bypervasives) of elt from a ByPassMap.t *)
      val init : t -> elt

      (** Deconstructor : from a Browsing structure get back the ByPassMap *)
      val bymap : elt -> t

      (** Go to root from anywhere. This is like [cd ~] in a file system *)
      val root : elt -> elt

      (** short name only, else cf string_of_path *)
      val elt_name : elt -> string

      (** Get the list of all children of a node : (name * elt). A function has no child,
          only [module_elt] have children. *)
      val children : module_elt -> (string * elt) list

      (** Say if an elt is the root of the structure. This is like the test [$PWD=~]*)
      val is_root : elt -> bool

      (** Get the parent of an elt. Parent of the root node, is the root node *)
      val parent : elt -> elt

      (** Unwrap an elt to its public representation. *)
      val public_elt : elt -> public_elt

      (** Get Initial Code : this does not include the types definition *)
      val export_bypass : elt -> bypass_library

      (** export children : give the [function] or children of module *)
      val export_children : elt -> bypass_library list

      (**
         produce the inclusion from parser fmt : can raise RegisterParserError exception.
         Until we remove the qml-syntax, there is an optionnal flag to tell if we are
         in opasyntax mode for inclusion. (default is qml)
      *)
      val include_format : elt -> BslIncludeFormats.fmt -> string

      val preprocess_line : t -> filename:string -> line:int -> string -> string

      (** A module for manipulating path *)
      module Path :
      sig
        type step
        val backward : path -> path
        val forward : path -> step -> path
        val step : string -> step
        val root : path
        val is_root : path -> bool
        val of_string : string -> path option
          (** for example : "/toto/bibi" or "toto/../../string/../" return None if syntax error *)
        val build : string list -> path option
        val to_string : path -> string (* module hierarchie *)
        val to_list : path -> string list
        val cd : elt -> path -> elt option
        val pwd : elt -> path
      end
    end
  end

  (** {6 Imperative interfaces for registering bypasses and building bymaps} *)

  (**
     The 2 following modules work together. They share common imperatives tables.

     Using them can be resumed so :

      + use the [RegisterInterface] to register some primitives and types,
     leading to a side effect on private tables
      + use the [RegisterTable] when you're finished to build a fonctionnal
     structure [ByPassMap.t]
     from the current state of the private imperative tables

     <!> Note that casual users may not use directly the [RegisterInterface].
     It is provided to talk with generated code only, generated by {b bslregister}
     ([*Loader], [*Plugin] like modules)
  *)

  module RegisterTable :
  sig
    (** We build a functionnal structure with all loaded bypass *)
    (** If you want to repeat it, you can load other libs, and recompute *)
    (** optimisation : you can build a restricted map directly here
        (better than restrict it after) *)
    val build_bypass_map :
      (* trans *)
      ?ml_ctrans:ml_ctrans_env ->
      ?js_ctrans:js_ctrans_env ->

      (* filtering *)
      ?filter:(ByPass.t -> bool) ->

      unit ->
      ByPassMap.t

    val build_restrict_map_any :
      (* trans *)
      ?ml_ctrans:ml_ctrans_env ->
      ?js_ctrans:js_ctrans_env ->

      (* filtering *)
      ?filter:(BslKey.t -> bool) ->
      lang:BslLanguage.t list ->

      unit ->
      ByPassMap.t

    val build_restrict_map_all :
      (* trans *)
      ?ml_ctrans:ml_ctrans_env ->
      ?js_ctrans:js_ctrans_env ->

      (* filtering *)
      ?filter:(BslKey.t -> bool) ->
      lang:BslLanguage.t list ->

      unit ->
      ByPassMap.t
  end

  module RegisterInterface :
  sig
    type error
    exception RegisterError of error
    val pp_error : error LangPrint.pprinter

    (** Gestion of MultiLoading :
        In Each Execution of bsl, every one-day generated loader could be runned just once *)
    module MultiLoading :
    sig
      val is_loaded : string -> bool
      val first_load : string -> unit
    end

    (** cf type register_primitive in BslPluginInterface *)
    val unsafe_register_primitive :  BslPluginInterface.register_primitive

    (** the bslregister call this function to declare every (records, extern)
        in the implementations files *)
    val unsafe_register_type : BslPluginInterface.register_type

    (** Safe register *)
    val register : BslPluginInterface.multi_loading_safe_get_dynloader_interface

    (** take a Loader.dynload function, and execute it *)
    val dynload : BslPluginInterface.dynloader -> unit

    (** the same without registering the function pointers (the argument [obj:Obj.t]) *)
    val dynload_no_obj : BslPluginInterface.dynloader -> unit

  end
end


(** {6 Bsl Register} *)

type filename             = string
type executable           = filename
type shell_options        = string list

(**
   Options for bslregister.

   The type of options you should pass to build a new bslregister session

   the mlruntime_filename is given because this name appears in several places
   in other generated files. (like the plugin). It is necessary to know it
   a priori, before any generation.
*)
type options = {

  basename : BslPluginInterface.plugin_basename ;
  (**
     The name of the plugin being build
  *)

  bypass_plugins : BslDynlink.bypass_plugin_file list ;
  (**
     The list of plugin which this lib depends on
  *)

  check_style : bool ;
  (**
     This option is not meant to be used all the time.
     It performs some more check about some guidelines,
     or some higienic practice.
     It is meant to be checked e.g. by a distant robot,
     or sometimes, just to see what happens.

     <!> The exact things checked are not documented,
     it can freely evoluate, not any compatibility with
     previous versions are guarante.
  *)

  js_files : filename list ;
  (**
     this is needed, because for parsing jsconf files, we need to
     know the name of all considered js files for beeing able to
     filter them reading regexps.
     The declaration of js files should be done before any preprocessing
     of jsconf files.
  *)

  js_syntax_checker : ( (executable * shell_options) * filename list ) option ;
  js_validator      : ( (executable * shell_options) * filename list ) option ;
  (**
     The executable for performing syntax check.
     TODO(maxime) merge them both. Not in one year please.
  *)

  ml_plugin_filename  : filename ;
  (**
     The asked filename for the plugin.
     e.g. ["opabslPlugin.ml"]
  *)

  ml_runtime_filename : filename ;
  (**
     The asked filename for the ml_runtime.
     e.g. ["opabslMLRuntime.ml"]
  *)

  unsafe_js : bool ;
  (**
     Continue even if the js validation fails.
  *)

  unsafe_opa : bool ;
  (**
     Continue, even if the opa validation fails.
  *)
}
