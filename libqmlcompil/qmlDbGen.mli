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

(** {6 Command line arguments } *)

(** Describes different backend that dbgen handle. *)
type engine = [`db3 | `mongo]

(** Command line specification provides necessary options for switch
    database backend. *)
module Args : sig

  (** The command line specifications. *)
  val options : (string * Base.Arg.spec * string) list

  (** Get selected backend.*)
  val get_engine : unit -> engine

end

(** {6 Initialization } **)

(** Set the translation function beetween string and renamed type
    ident.*)
val settyp : (string -> QmlAst.typeident) -> unit

(* =============== *)
(** DbGen Main API *)

module Schema: sig

  (** The type of the database schema. Purely functional structure *)
  type t = Schema_private.meta_schema

  type database = {
    name : string;
    ident : Ident.t;
    dbty : QmlAst.ty;
    options : QmlAst.Db.options list;
    package : ObjectFiles.package_name;
  }

  type query = QmlAst.expr QmlAst.Db.query * QmlAst.expr QmlAst.Db.query_options

  type set_kind =
    | Map of QmlAst.ty * QmlAst.ty
    | DbSet of QmlAst.ty

  type node_kind =
    | Compose of (string * string list) list
    | Plain
    | Partial of bool * string list * string list
    | SetAccess of set_kind * string list * (bool * query) option (*bool == unique*)

  type node = {
    ty : QmlAst.ty;
    kind : node_kind;
    database : database;
    default : QmlAst.annotmap -> (QmlAst.annotmap * QmlAst.expr);
  }


  (** Maps the idents of the different database schemas and their respective
      options in a multi-schema *)
  val mapi:
    (string list -> QmlAst.ident * QmlAst.Db.options list -> QmlAst.ident * QmlAst.Db.options list)
    -> t -> t

  (** Initial empty schema *)
  val initial: t

  (** Returns true if no definitions have been recorded into the schema *)
  val is_empty: t -> bool

  (**
     Registers a path (given as string list) with a given type and default
     value in a schema. The Env.t is only used for looking up typenames
     Hack: until the refactoring of positions is done, we must pass to this
     function an error context.
  *)
  val register_path:
    context:QmlError.context ->
    t -> QmlTypes.Env.t -> QmlAst.Db.path_decl -> QmlAst.ty -> t

  val register_path:
    context:QmlError.context ->
    t -> QmlTypes.Env.t -> QmlAst.Db.path_decl -> QmlAst.ty -> t

  (**
     Registers the default value for a given path; puts the coerced default
     expression into the schema to type-check later.
     Hack: until the refactoring of positions is done, we must pass to this
     function an error context.
     If name_default_values is true, then this function returns a binding
     (the name and definition of the default value) and the new expression
     inside the default value (which is just the name=
     if name_default_values is false, then the second return value is always None
  *)
  val register_default:
    name_default_values:bool ->
    context:QmlError.context -> t -> QmlAst.Db.path_decl -> QmlAst.expr -> t * ((Ident.t * QmlAst.expr) * QmlAst.expr) option

  (** Registers database declarations *)
  val register_db_declaration:
    t -> Annot.label * Ident.t * QmlAst.Db.path_decl * QmlAst.Db.options list
    -> t

  (** Registers db-related declarations (paths & default & constraints)
      See register_default for the meaning of the name_default_values parameter
      and the second return value
  *)
  val register_new_db_value:
    name_default_values:bool ->
    t -> QmlTypes.Env.t
    -> Annot.label * (QmlAst.expr, QmlAst.ty) QmlAst.Db.db_def
    -> t * ((Ident.t * QmlAst.expr) * (QmlAst.expr, QmlAst.ty) QmlAst.Db.db_def) option

  (** Map any prepath to its coerced expression equivalent within the
      expressions. Additionally, returns a assoc list of old annots to new
      generated annots that can be used eg. to keep track of positions *)
  val preprocess_paths_expr: ?val_:(string -> QmlAst.ident) -> t -> QmlTypes.gamma -> QmlAst.expr -> (Annot.t * Annot.t) list * QmlAst.expr
  val preprocess_paths_code_elt: ?val_:(string -> QmlAst.ident) -> t -> QmlTypes.gamma ->QmlAst.code_elt -> (Annot.t * Annot.t) list * QmlAst.code_elt
  val preprocess_paths_ast: ?val_:(string -> QmlAst.ident) -> t -> QmlTypes.gamma -> QmlAst.code_elt list -> (Annot.t * Annot.t) list * QmlAst.code_elt list

  (** Finalization of the schema, to use before initialisation below, and before
      code generation. Returns None if no database content is actually defined.
      Only touches the part of the schema belonging to the current package *)
  val finalize: t -> t option

  val of_package : t -> ObjectFiles.package_name -> t

  (** Merge two schemas.*)
  (* Temporary assumption: schemas should be compatible *)
  (* When each compilation unit will stored ONLY its own schema part (and not the merged one as now),
     merge will be done only with disjoint schemas (except for the root). *)
  val merge : t -> t -> t

  (** Folds the given function on all types contained in the schema *)
  val map_types : (QmlAst.ty -> QmlAst.ty) -> t -> t

  (** Folds the given function on all expressions contained in the schema *)
  val map_expr : (QmlAst.expr -> QmlAst.expr) -> t -> t

  (** Folds the given function on all expressions contained in the schema
      (e.g. default values). Use this for typing (before or after finalize *)
  val fold_expr: ('a -> QmlAst.expr -> 'a) -> 'a -> t -> 'a

  (** Same but also rewrites these definitions *)
  val foldmap_expr: ('a -> QmlAst.expr -> 'a * QmlAst.expr) -> 'a -> t -> 'a * t

  (** Exports the database schema as dot data, useful for debugging. Display for
      example with [dot -Tps |display] *)
  val to_dot: t -> out_channel -> unit

  (** Exports the database schema for the given database as dot data, useful for
      debugging. Display for example with [dot -Tps |display]. Raises Not_found
      if the database does not exist, or you didn't specify one and there are
      several *)
  val db_to_dot: t -> string option -> out_channel -> unit

  (** As [db_to_dot], but exports as a gml file, that can be used to manipulate
      live databases with [opa-db-tool] *)
  val db_to_gml: t -> string option -> out_channel -> unit

  (** Parses a schema saved in the GML format (like in the run-time db) *)
  val from_gml: string -> t

  val get_db_declaration: t -> database list

  val get_node: t -> QmlAst.path -> node

  val pp_node: node BaseFormat.pprinter

  (**
     Hackish module, should be removed after the refactoring of positions in the AST.
  *)
  module HacksForPositions :
  sig
    (**
       Special Hackish annotmap, used for finding positions for error messages.
       Used for having an annotmap when the module fails, without changing every
       interfaces by adding an annotmap in argument.
    *)
    val set_annotmap : QmlAst.annotmap -> unit
    val free_annotmap : unit -> unit
  end
end

module type S = sig include DbGenByPass.S end

(** Internal type used to handle bindings in code generation *)
type dbinfo
val merge_dbinfo : dbinfo -> dbinfo -> dbinfo

module DbGen: functor  ( Arg: S ) -> sig
  (** [initialize schema] initialises database code on a given schema. It
      returns the dbinfo needed for further access to this database, the gamma
      corresponding to defined idents and the initial code that opens the
      database and defines the specialised access functions. The returned annotmap
      contains only added annots.

      The Arg module must give access to identifiers linked to "some", "none",
      "map_empty", "map_add"...

      The returned code should be put _after_ declarations of these
      identifiers and _before_ any access to the DB *)
  val initialize: ?annotmap:(QmlAst.annotmap option) -> ?valinitial_env:(Arg.ValInitial.env) -> QmlTypes.gamma -> Schema.t -> dbinfo StringListMap.t * QmlTypes.Env.t * (QmlAst.annotmap option) * QmlAst.code * QmlAst.code

  (** Replaces all path accesses in an expression by calls to Db3. The resulting
      expression is guaranteed not to contain any Path or Transaction.

      The annotmap must be provided for the resulting code to be typed. Returns the
      annotmap of created annots or IntMap.empty. *)
  val replace_path_exprs: Schema.t -> dbinfo StringListMap.t -> QmlTypes.gamma -> ?annotmap:(QmlAst.annotmap option) -> ?valinitial_env:(Arg.ValInitial.env) -> QmlAst.expr -> QmlAst.annotmap option * QmlAst.expr

  (** Same as [replace_path_exprs] but maps on a code_elt *)
  val replace_path_code_elt: Schema.t -> dbinfo StringListMap.t -> QmlTypes.gamma -> ?annotmap:(QmlAst.annotmap option) -> ?valinitial_env:(Arg.ValInitial.env) -> QmlAst.code_elt -> QmlAst.annotmap option * QmlAst.code_elt * QmlTypes.gamma

  (** Same as [replace_path_exprs] but maps on a full code *)
  val replace_path_ast: Schema.t -> dbinfo StringListMap.t -> QmlTypes.gamma -> ?annotmap:(QmlAst.annotmap option) -> ?valinitial_env:(Arg.ValInitial.env) -> QmlAst.code -> QmlAst.annotmap option * QmlAst.code * QmlTypes.gamma
end

module DbGenByPass : sig
  module MakeS ( N : DbGenByPass.SourceInterface ) (I : DbGenByPass.I) : S with type ValInitial.env = I.env
  module I_Unit : DbGenByPass.I
  module I_Alpha : DbGenByPass.I
  module BSLDbGen : S
  module BSLDbGenAlpha : S
  module BSLDbGenAlphaOpa : S with type ValInitial.env = QmlAlphaConv.t option
end

(** Warnings that can be triggered by DbGen and schema processing *)
val warning_set: WarningClass.Set.t
