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
(*
  Authors: 2006 - 2009, Henri Binsztok    <Henri.Binsztok@mlstate.com>
           2009, Vincent Benayoun         <Vincent.Benayoun@mlstate.com>
           2009, Mikolaj Konarski         <Mikolaj.Konarski@mlstate.com>
           2009, Mathieu Barbin           <Mathieu.Barbin@mlstate.com>
           2009, Louis Gesbert            <Louis.Gesbert@mlstate.com>
           2009, Mehdi Bouaziz            <Mehdi.Bouaziz@mlstate.com>
           2009, David Rajchenbach-Teller <David.Teller@mlstate.com>
*)

(**
   The complete AST for QML.
*)

(*
    The purpose is that the module who have to deal with AST should only do a 'open QmlAst'

    - interfaces bring lot of work on maintainability, so let's try to reduce the number of it

    - but then, the construction of type (actually, this is like an implementation) is hidden
    by having the type in the first level of this 100% shared file

    This file has no mli, because it mostly contains type declaration,
    and few implementation, then the mli would be the same (duplication)

*)

(* depends *)
module String = BaseString
module List = BaseList

(* aliases *)
module QTV = QmlTypeVars

(* shorthands *)
module TypeVar = QmlTypeVars.TypeVar
module RowVar = QmlTypeVars.RowVar
module ColVar = QmlTypeVars.ColVar

(**
   A literal constant
*)

type const_expr =
  | Int of int (** simplify the life in opa / ml code generator *)
  | Float of float
  | String of string

(**
   The type of a simple value.
   Please keep them sorted alphabetically, for tests and display.
*)

type const_ty =
  | TyFloat
  | TyInt
  | TyNull (** empty type: nothing should really have this type *)
  | TyString

(** build_meta will also be moved *)
module Const =
struct
  (** Return the type of a simple value.*)
  let type_of = function
    | Int _    -> TyInt
    | Float _  -> TyFloat
    | String _ -> TyString

  (** Return a human-readable version of a simple value.*)
  let string_of_expr = function
    | Int i    -> string_of_int i
    | Float f  -> string_of_float f
    | String s -> Printf.sprintf "%S" s

  let pp_expr fmt expr = Format.pp_print_string fmt (string_of_expr expr)

  (** Return a human-readable version of a simple type.*)
  let string_of_ty = function
    | TyInt    -> "int"
        (** if we reswitch to Int64.t, to preserve confusion in declaration of type (e.g. by using register directives in libbsl) it is preferable to print int64 instead of int *)
    | TyFloat  -> "float"
    | TyString -> "string"
    | TyNull   -> "null"

  let pp_ty fmt ty = Format.pp_print_string fmt (string_of_ty ty)

  (** Return the OCaml type corresponding to this simple type.
      Used to generate OCaml source code.*)
  let ocamlbsl_string_of_ty = function
(*     | TyInt -> "Int64.t" *)
    | t     -> string_of_ty t

  (** Return the C type corresponding to this simple type.
      Used to generate C source code. *)
  let cbsl_string_of_ty = function
    | TyInt -> "long long int"
    | TyFloat -> "double"
    | TyString -> "char *"
    | TyNull -> assert false (* "void *" ?? *)

  (** meta : keep it not too far from the definition *)
  let meta = function
    | TyInt    -> "TyInt"
    | TyFloat  -> "TyFloat"
    | TyString -> "TyString"
    | TyNull   -> "TyNull"
end

(** This module contains the structures for DB definitions,
    and unpreprocessed paths *)
module Db =
struct

  (** The kind of a path, ie the type of access it is *)
  type kind =
    | Option
        (** Check if the path exists in the database, and return the lazy-read as
            option if it does. Errors while reading are still filled with
            default values *)
    | Default
        (** Normal read (any error/missing data returns a default value) *)
    | Valpath
        (** Firstclass value-path, does not read right away, but bound to the
            current state of the db *)
    | Ref
        (** No operation is done, just build a pointer to the database.  used for
            operations on paths (like writes) *)

  (** The paths as specified in definitions (eg. with possible definition of keys) *)
  type path_decl_key =
    | Decl_fld of string
        (** A field name, for standard /db/xxx paths *)
    | Decl_int
        (** Declares an intmap *)
    | Decl_string
        (** Declares a string map *)
    | Decl_set of string list list
        (** Declares a set with unicity determined by list of keys
            (empty means whole element) *)
  type path_decl = path_decl_key list

  (** The extensible type for DB definitions (the "database xxx" level 0
      directive). For now, only used to specify the data storage location (any
      path, absolute or relative, ending with the filename prefix). DbGen sets
      this to [~/.mlstate/<progname>/default] by default (based on argv.(0)) *)
  type engine =
      [
      |`db3 of string option (* path *)
      |`db3light of string option (* path *)
      |`meta
      |`client of string option * int option (* server:port *)
      ]
  type options =
      [
      |`engine of engine
      |`mountpoint of string list
      ]

  type 'expr db_constraint =  (** The type of DB constraints as specified by the user. To be extended *)
      (* /!\ this is WIP, and most of it is dummy for now *)
    | C_Ordering of 'expr
        (** For maps: give an ordering relationship for returned results
            (expr is of type [tkeys -> tkeys -> int] if the type stored at the path is [map(tkeys,'a)]) *)
    | C_Inclusion of path_decl
        (** Db_Constraint(p1, Db_Inclusion p2) means that any path satisfying p1 is an element of
            the set pointed to by path p2 *)
    | C_Validation of 'expr
        (** Provide a validation function to run on the data before writing it at the given path
            (expr is of type [typath -> bool], where typath is the type stored at the path) *)
    | C_Inverse of path_decl
        (** cross-reference between the elements of two paths and their
            respective parents, for example [/authors[]/biblio] and
            [/books[]/authors], meaning that for any element of [/books] [B] and any
            element of [/authors] [A], [B] \in [A/biblio] is equivalent to [A] \in
            [B/authors] *)
    | C_Private
        (** marks a path as private: accesses will only be allowed on the full path,
            not directly on children. Implies that the sub-tree can't contain partial
            records. This constraint is propagated to all sub-nodes *)

  type ('expr,'ty) db_def =
    | Db_TypeDecl of path_decl * 'ty
    | Db_Default of path_decl * 'expr
    | Db_Alias of path_decl * path_decl
    | Db_Constraint of path_decl * 'expr db_constraint
    | Db_Virtual of path_decl * 'expr

  let path_kind_to_string = function
    | Default -> "" | Option -> "?" | Valpath -> "!" | Ref -> "@"
  let engine_to_string opt =
    match opt with
    | `db3 s -> "@local(" ^ Option.default "" s ^ ")"
    | `db3light s -> "@light(" ^ Option.default "" s ^ ")"
    | `meta -> "@meta"
    | `client (h, p) ->
        let h = Option.default "" h in
        let p = match p with None -> "" | Some p -> ":" ^ string_of_int p in
        "@shared(" ^ h ^ p ^ ")"
  let options_to_string opts =
    String.concat_map " "
      (function
       | `engine s -> engine_to_string s
       | `mountpoint _sl -> ""
      ) opts
  let path_decl_key_to_string = function
    | Decl_fld s -> "/"^s
    | Decl_int -> "[_]" (* "[]" in qml. This is only valid for default defs. Should be "[int]" once available in OPA *)
    | Decl_string -> "[_]" (* same remark, should be "[string]" *)
    | Decl_set [] -> "[_]"
    | Decl_set l ->
        String.concat_map "; " (fun l -> "[{"^ String.concat ", " l ^"}]") l
        (* not yet usable *)
  let path_decl_to_string = String.concat_map "" path_decl_key_to_string

  let constraint_to_string _e_t_s = function
    | C_Private -> "full"
    | _ -> "[[database constraint]]" (* todo (but doesn't have a syntax yet) *)

  let def_to_string e_t_s ty_t_s = function
    | Db_TypeDecl (pd,ty) -> Printf.sprintf "db %s : %s" (path_decl_to_string pd) (ty_t_s ty)
    | Db_Alias (pd,pd') -> Printf.sprintf "db %s alias %s" (path_decl_to_string pd) (path_decl_to_string pd')
    | Db_Default (pd,e) -> Printf.sprintf "db %s = %s" (path_decl_to_string pd) (e_t_s e)
    | Db_Constraint (pd,cstr) -> Printf.sprintf "db (%s %s)" (path_decl_to_string pd) (constraint_to_string e_t_s cstr)
    | Db_Virtual (pd,e) -> Printf.sprintf "db %s := %s" (path_decl_to_string pd) (e_t_s e)

  let print_constraint _p_e out = function
    | C_Private -> Format.fprintf out "full"
    | _ -> Format.fprintf out "<FIXME database constraint>"

  let print_def p_e p_ty out = function
    | Db_TypeDecl (pd,ty) ->
        Format.fprintf out "@[<hv 2>db %s :@ %a@]" (path_decl_to_string pd) p_ty ty
    | Db_Alias (pd,pd') ->
        Format.fprintf out "@[<hv 2>db %s :@ %s@]" (path_decl_to_string pd) (path_decl_to_string pd')
    | Db_Default (pd,e) ->
        Format.fprintf out "@[<hv 2>db %s =@ %a@]" (path_decl_to_string pd) p_e e
    | Db_Constraint (pd,cstr) ->
        Format.fprintf out "@[db (%s %a)@]" (path_decl_to_string pd) (print_constraint p_e) cstr
    | Db_Virtual (pd, e) ->
        Format.fprintf out "@[db %s : = %a@]" (path_decl_to_string pd) p_e e


  module TU =  Traverse.Utils
  let sub_db_constraint sub_e _sub_ty = function
    | C_Ordering e ->
        TU.wrap (fun e -> C_Ordering e) (sub_e e)
    | C_Inclusion p ->
        TU.wrap (fun e -> C_Inclusion e) (TU.sub_ignore p)
    | C_Validation e ->
        TU.wrap (fun e -> C_Validation e) (sub_e e)
    | C_Inverse p ->
        TU.wrap (fun e -> C_Inverse e) (TU.sub_ignore p)
    | C_Private as x ->
        TU.sub_ignore x

  let sub_db_def sub_e sub_ty = function
    | Db_TypeDecl (p,ty) ->
        TU.wrap (fun (p,ty) -> Db_TypeDecl (p,ty)) (TU.sub_2 TU.sub_ignore sub_ty (p,ty))
    | Db_Alias (p,p') ->
        TU.wrap (fun (p,p') -> Db_Alias (p,p')) (TU.sub_2 TU.sub_ignore TU.sub_ignore (p,p'))
    | Db_Default (p,e) ->
        TU.wrap (fun (p,e) -> Db_Default (p,e)) (TU.sub_2 TU.sub_ignore sub_e (p,e))
    | Db_Constraint (p,c) ->
        TU.wrap (fun (p,c) -> Db_Constraint (p,c)) (TU.sub_2 TU.sub_ignore (sub_db_constraint sub_e sub_ty) (p,c))
    | Db_Virtual (p,e) ->
        TU.wrap (fun (p,e) -> Db_Virtual (p,e)) (TU.sub_2 TU.sub_ignore sub_e (p,e))

  let foldmap_expr f acc dbdef =
    let cons, subs = sub_db_def TU.sub_current TU.sub_ignore dbdef in
    let acc, subs = List.fold_left_map f acc subs in
    acc, cons subs

  let foldmap_ty f acc dbdef =
    let cons, subs = sub_db_def TU.sub_ignore TU.sub_current dbdef in
    let acc, subs = List.fold_left_map f acc subs in
    acc, cons subs
end

(**
   Sometimes, modules redefines locally a module Ident.
   This type is for referencing the ident used in QML lang,
   which are standard [Ident.t]
*)
type ident = Ident.t

(**
   This module defines a mechanism for type variables. This mechanism
   is used for actual type variables (e.g. ['a], ['b], etc.) but also
   for row variables and for column variables.

   These are just synonyms for the fresh generation module (defined in
   "fresh.ml").
*)

module TypeIdent :
sig
  type t = Ident.t

  (** to be used in printers *)
  val to_printable_string : t -> string
  (** gives back the original name of the type *)
  val to_string : t -> string
  val to_debug_string : t -> string

  val of_string : ?check:bool -> string -> t (* should be used only by the parser *)
  val of_ident : Ident.t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end =
struct
  type t = Ident.t

  let to_debug_string id = Printf.sprintf "``_ty_%s" (Ident.to_string id)

  (*
    We test strictly than the of_string function is called only on
    identifiers registred in opacapi, using the opacapi interface.
    The physical test asserts than the string corresponding to the
    identifier is not duplicated in the code containing the insertion.
  *)
  let opacapi_check s =
    let is_in_opacapi =
      try
        let ss = Hashtbl.find Opacapi.table s in
        s == ss
      with
      | Not_found -> false
    in
    if not is_in_opacapi
    then (
      OManager.printf "OPACAPI violation, on ident %S@\n" s;
      OManager.printf "You should use opacapi for inserting types from the stdlib@.";
      assert false
    )

  let to_string x = Ident.original_name x

  let to_printable_string = to_string

  let of_string ?(check = true) name =
    if check then opacapi_check name ;
    Ident.source name

  let of_ident ident = ident

  let compare x y = Ident.compare x y

  let equal x y = compare x y = 0


  (*
    <!> keep the following property :
    equal(t1, t2) => hash(t1) = hash(t2)
  *)
  let hash i = Ident.hash i
end

(**
   Options for types definitions, from toplevel directives.
   Extensible, using {[{ QmlAst.ty_def_options with }]} syntax.
   The warn_x_field is used until we have at least 2 infos.
*)
type ty_def_options = {
  opacapi : bool ;
  warn_x_field : unit ;
}

(**
   The default value for type definitions options.
   Used for default value for easyer extensibility of
   the options in [ty_def_options]
*)
let ty_def_options = {
  opacapi = false ;
  warn_x_field = () ;
}

let pp_ty_def_options fmt opt =
  let () =
    if opt.opacapi
    then Format.pp_print_string fmt "@opacapi "
  in
  ()

type typeident = TypeIdent.t

type ty =
  | TypeConst    of const_ty
  | TypeVar      of typevar
  | TypeArrow    of ty list * ty (**The type of a function.*)
  | TypeRecord   of ty_row
  | TypeSum      of ty_col
  | TypeSumSugar of ty list
      (**Inelegant duplication; used to allow type names in sums; transformed later by the typer to regular TypeSum.
         Possible fix: differently typed ASTs at different stages to handle raw & preprocessed types*)
  | TypeName     of ty list * typeident
  | TypeAbstract
      (**represents *only* the keyword "abstract" or "extern" in the AST *)
  | TypeForall   of typevar list * rowvar list * colvar list * ty

and fields = (string * ty) list
and ty_row =
  | TyRow of fields * rowvar option
and ty_col =
  | TyCol of fields list * colvar option

and typevar = TypeVar.t
and rowvar = RowVar.t
and colvar = ColVar.t

let typeNull : ty = TypeConst TyNull
let column_to_records : ty_col -> ty list =
  function (TyCol (l, _)) ->
    let constant_row_to_record l = TypeRecord (TyRow (l, None)) in
    List.map constant_row_to_record l

module EqualsTy : sig
  type t = ty
  (** comparisons without environment, so without unfolding of named types *)
  val equal : ty -> ty -> bool
  val compare : ty -> ty -> int
  (** comparison modulo alpha-renaming of typevars, rowvars, colvars
      * if the [absorb] flag is on, it means that [Some row or col var] is equal
        to [None] (but if row or col vars are present on both sides, they must
        be consistent)
      * if the [collapse] flag is on, then distinct variables from the first
        type are allowed to be collapsed in the second one (warning this option
        is not symmetric)
  *)
  val equal_alpha : ?collapse: bool -> ?absorb: bool -> ty -> ty -> bool
  val compare_alpha : ?collapse: bool -> ?absorb: bool -> ty -> ty -> int
end =
struct
  type t = ty
  let compare_gen cmp_ti cmp_tv cmp_rv cmp_cv =
    let rec compare t1 t2 =
      if t1 == t2 then 0 (* speedup *) else
        match t1, t2 with
        | TypeConst c1, TypeConst c2 -> Pervasives.compare c1 c2
        | TypeVar v1, TypeVar v2  -> cmp_tv v1 v2
        | TypeVar _, _ -> 1
        | _, TypeVar _ -> -1
        | TypeArrow (lt11,t12), TypeArrow (lt21,t22) ->
            let c = List.make_compare compare lt11 lt21 in
            if c <> 0 then c else
            compare t12 t22
        | TypeRecord (TyRow (tfs,tv)), TypeRecord (TyRow (ufs, uv)) ->
            let c = cmp_rv tv uv in
            if c <> 0 then c else compare_fields tfs ufs
        | TypeName (tel1, n1), TypeName (tel2, n2) ->
            let c = cmp_ti n1 n2 in
            if c <> 0 then c else
              List.make_compare compare tel1 tel2
        | TypeSum (TyCol (ts1, cv1)), TypeSum (TyCol (ts2, cv2)) ->
            let c = cmp_cv cv1 cv2 in
            if c <> 0 then c else List.make_compare compare_fields ts1 ts2
        | TypeSumSugar ts1, TypeSumSugar ts2 ->
            List.make_compare compare ts1 ts2
              (* some unpleasant overlap between TypeRecord and TypeSum *)

         | TypeRecord (TyRow (tfs, None)), TypeSum (TyCol ([ufs], None)) ->
             compare_fields tfs ufs

        | TypeSum (TyCol ([ufs], None)), TypeRecord (TyRow (tfs, None)) ->
            compare_fields ufs tfs
              (* different root constructors, so unequal by Pervasives.compare *)

        (* This fix may sux if explicitely quantified typevar are not unique *)
        (*| TypeForall(_,_,_,t1), TypeForall( _, _, _,t2) -> compare t1 t2*)
        | _ -> Pervasives.compare t1 t2
    and compare_fields f1 f2 =
      let cmp_field (tfn, tft) (ufn, uft) =
        let c = Pervasives.compare tfn ufn in
        if c <> 0 then c
        else compare tft uft in
      List.make_compare cmp_field f1 f2 in
    compare

  let compare =
    compare_gen
      TypeIdent.compare TypeVar.compare (Option.make_compare RowVar.compare)
      (Option.make_compare ColVar.compare)

  let equal t1 t2 = (compare t1 t2 = 0)

  let compare_alpha ?(collapse=false) ?(absorb=false) t1 t2 =
    let hashtbl_size = 8 in
    let option_make =
      if absorb then fun cmp v1 v2 -> Option.default 0 (Option.map2 cmp v1 v2)
      else Option.make_compare in
    let cmp_factory =
      if collapse
      then fun cmp ->
        let h = Hashtbl.create hashtbl_size in
        fun v1 v2 ->
          try let w = Hashtbl.find h v1 in cmp w v2
          with Not_found -> Hashtbl.add h v1 v2; 0
      else fun cmp ->
        let h1 = Hashtbl.create hashtbl_size in
        let h2 = Hashtbl.create hashtbl_size in
        fun v1 v2 ->
          try let w = Hashtbl.find h1 v1 in cmp w v2
          with Not_found -> Hashtbl.add h1 v1 v2;
            if Hashtbl.mem h2 v2 then cmp v1 v2 else (Hashtbl.add h2 v2 v1; 0)
    in
    let cmp_tv = cmp_factory TypeVar.compare
    and cmp_rv = cmp_factory (option_make RowVar.compare)
    and cmp_cv = cmp_factory (option_make ColVar.compare) in
    compare_gen TypeIdent.compare cmp_tv cmp_rv cmp_cv t1 t2

  let equal_alpha ?(collapse=false) ?(absorb=false) t1 t2 =
    compare_alpha ~collapse ~absorb t1 t2 = 0

end

type annotmap = ty QmlAnnotMap.gen_annotmap

type pat_rowvar = [
  | `closed
      (**
         represent the absence of dots in the pattern, as in:
         {[
         | { foo = bar }
         ]}
      *)

  | `open_
      (**
         represent the dots in the pattern, as in:
         {[
         | { foo = bar ; ... }
         ]}
      *)
(*
  | `autocomplete
      (**
         represent the syntax '_' for asking the
         typer to complete the pattern.
      *)
*)
]

type pat =
  | PatRecord        of Annot.label * (string * pat) list * pat_rowvar
  | PatConst         of Annot.label * const_expr
  | PatVar           of Annot.label * Ident.t
  | PatAny           of Annot.label
  | PatCoerce        of Annot.label * pat * ty
  | PatAs            of Annot.label * pat * Ident.t

(*
?) PatOr

let x = ... in match x with
| truc
| machin
| bidule
| a_lot_of_patterns
 -> do a_big_bloc_of_code()

it will be represented into a lot of sames big blocs of codes... (it sux)

?) guard

match x with
| 0 -> 4
| x when mod(x, 2) == 0 -> 5
| 3 -> 42
| _ -> 5 / 0

we cant do this, because "when" is not possible with this kind of pattern.
*)

(** {6 Directives} *)
type tagged_string_kind =
  | Rpc_use | Rpc_def
  | Type_def | Type_use
  | Client_closure_use

(** Types directive *)
type type_directive = [
  | `coerce
      (**
         Coercion. Currently unused. Should replace Coerce from the AST
      *)

  | `opensums
      (**
         Adds deeply column variables to allow subtyping of sum types
      *)

  | `openrecord
      (**
         Adds a row variable to allow subtyping of record types
      *)

  | `module_
      (**
         Indicate that the underlying record is a module, for type generalization purpose
      *)

  | `module_field_lifting
      (** Indicates that the embedded expression was initially a module field
          that has been extracted from the module and transformed into an
          expression bound at the same level than the module to be used later
          to really create the expression of module.
          [SurfaceAstDependencies.rewrite_module] takes each field of a module
          and transform it into a let-definition with the same name than the
          field. Then, once done for all the fields, the module expression is
          rebuilt by setting each field to its corresponding artificial
          let-definition.
          This transformation pass must signal the let-definitions it creates
          this way by embedding it into the present directive. Hence, normally
          this directive should only host [QmlAst.LetIn] expressions kind.
          Being able to track these let-definitions is required to remind that
          type variables introduced by explicit type annotations in a module
          field must be generalizable at the exit of the definition of this
          field. Because the transformation transforms fields into
          let-definitions, without the directive, it is impossible to know that
          type variables of a let-definitions can be generalized at the exit of
          the definition and not at the exit of the current toplevel phrase. *)
  | `unsafe_cast
      (**
         As [Obj.magic]
      *)

  | `nonexpansive
      (**
         Tell the value restriction pass to generalize.
      *)

  | `warncoerce
      (**
         Used to check after typing that some expressions have an expected types.
         In case the expression has not the given type, the pass_WarnCoerce raise
         a warning of the class typer.warncoerce. This class can be turned on --warn-error.
         Essentially used for checking that an expression after a 'do' should have type void.
      *)

]

(**
   Directives inserted by the new slicer
   (it doesn't use any directives)
*)
type simple_slicer_directive =
    [ `ajax_call    of [`sync | `async]
        (** remote call client -> server *)
    | `ajax_publish of [`sync | `async]
        (** only appear at the top of an expression in a newval
            indicate that a server-side stub should be produced for the binding
        *)

    | `comet_call   (** symmetric directive to `ajax_call *)
    | `comet_publish(** symmetric directive to `ajax_publish *)
    | `insert_server_value of Ident.t
      (** only appears at the top of an expression in a newval in the client code
          the ident in the variant is a server-side identifier
          indicate that the expression on the client should be replaced at server
          startup by the value of the server identifier
      *)
    | `sliced_expr ] (** see the description in surfaceAst *)

type userland_public_visibility_directive = [`sync | `async ]
type compiler_public_visibility_directive = [ `funaction ]
type public_visibility_directive = [ userland_public_visibility_directive | compiler_public_visibility_directive]

type 'a generic_visibility_directive = [ `visibility_annotation of [ `public of 'a | `private_ ] ]


type userland_visibility_directive = userland_public_visibility_directive generic_visibility_directive
type visibility_directive          = public_visibility_directive          generic_visibility_directive

type slicer_directive =
    [ `side_annotation of [ `server
                          | `client
                          | `both
                          | `prefer_server
                          | `prefer_client
                          | `prefer_both
                          | `both_implem ]
    | visibility_directive
    ]

(** Fun actions *)

type fun_action_content =
  | Deserialize
  | Client_id

type fun_action_directive = [
  | `fun_action of fun_action_content option
      (** put by the parser in the ast at, for instance: <div onclick={@fun_action(some_expr)}/>
          (except that actually the magicFunAction is under the @fun_action *)
  ]

(** Thread context *)
type thread_context_directive = [
  | `thread_context (** Returns the context stored in the current continuation *)
  | `with_thread_context (** Update the context stored in the current continuation *)
]

(**
   Coding directives
*)
type coding_directive = [
  | `deprecated
  | `todo
]

(** CPS and concurrency *)
type cps_directive = [
  | `spawn      (**Spawn a task for concurrent execution. Handled by CPS transform.*)
  | `wait       (**Wait until a task has completed. Handled by CPS transform.*)
  | `callcc     (** \@callcc( fun k -> ApplyCont (k, e) ) returns e. \@uncps is too restrictive, and is deprecated. *)
  | `atomic     (**Specify that an expression should be executed in one go, without interruptions. Handled by CPS transform.*)
  | `immovable  (**Specify that an expression should be executed only where it was spawned. Handled by CPS transform.*)

  | `cps_stack_lambda of (Obj.t as 'il_cident) option ref (** used to mark the continuation of a lambda
                                                              appears exclusively right under a lambda *)
  | `cps_stack_apply of ((Obj.t as 'il_cident) option ref option * string option * string option)
      (** used to mark applications where the stack needs to be updated
          the first element is the continuation of the closest enclosing lambda (marked with @cps_stack_lambda)
          (if any)
          the second element is the name of called function if any
          the third element is the position of the application, if any
      *)

  | `async
      (**
         When put on a lambda, means that the call to this function should be asynchronous
         (valid for local calls and remote calls)
         When put on something else, tag for using partially the toplevel-concurrency mode
         when the option is not activated.
         As we have no way to put directives at toplevel in the AST, we count on the luck and the
         great passes magical spirit for the directive to stay arround the expression associated
         to the binded identifier in a NewVal.
      *)

  | `may_cps
      (**
          For using the cps_bypass instead of a bypass
      *)

  | `apply_cont
      (**
         Indicate that the underlying apply was translated in qml from an ApplyConst in IL
         Used by the js backend to know when to treat tail calls
      *)
]

(** Exceptions. Unsafely typed for now, only for experimentation purposes *)
type exception_directive = [
  | `throw (** Takes an expression, evaluates it and raises it as an exception *)
  | `catch (** Takes a function of exceptions, and an expression to evaluate. Any
               exception thrown during this evaluation and not already caught is
               passed directly to the function given, and the result of this
               application is the return value of the directive *)
]

(** Used for extend magic function. *)
type opavalue_directive = [
| `stringifier (** Extend OpaValue.to_string *)
| `comparator  (** Extend OpaValue.compare *)
| `serializer  (** Extend serialization system *)
| `xmlizer     (** Extend Xml.of_alpha (ie magic_xml)*)
]

type doctype_access_directive =
    [ `private_ (* visible only in the current module *)
    | `public (* visible to everyone *)
    | `package (* visible only in the current package *)
    ]

(**
  add information around an apply or a lifted lambda
*)
type lambda_lifting_directive = [
  | `partial_apply of int option * bool (* original arity of the function, guaranteed to be filled by lambda lifting,
                                           None means 'undisclosed information' :)
                                           the boolean indicates that this is a creation of serializable closure
                                           (so the partial apply may have extra type arguments) *)
  | `full_apply of int (* size of the env *)
  | `lifted_lambda of int * Ident.t option (* size of the env and the toplevel name of the declaration from where it was lifted
                                            * (meaningful between lambda lifting and explicit instantiation, because
                                            * ei adds @lifted_lambda on declarations that are not really lifted, so
                                            * what would the value be?) *)
]

(*
  If you want to add a new directive, it should appear:
  - in qmlPrinters line ~650 if the variant holds some data line
  - in qmlPrinters line ~400 in any case
  - in qmlAstCons.Directive
  - in qmlflat/qmlFlatCompiler
  - qmltop/qmlTopEval
  Expression in directive are no more allowed.
  You will use the Nary support for such directives.
*)
type qml_directive = [
  | cps_directive
  | coding_directive
  | exception_directive
  | fun_action_directive
  | simple_slicer_directive
  | slicer_directive
  | thread_context_directive
  | type_directive

  | `closure_create of Ident.t * int * ((ty, unit) QmlGenericScheme.tsc) option
      (** definition of a closure with its implementation, arity, and type scheme *)
  | `closure_apply
      (** application of a closure to a list of arguments *)
  | `closure_create_no_function of Ident.t * int * ((ty, unit) QmlGenericScheme.tsc) option
      (** definition of a closure without its implementation (the ident is unbound at this point) *)
  | `closure_define_function of Ident.t * Ident.t * ((ty, unit) QmlGenericScheme.tsc) option
      (** definition of the implementation field of the closure *)

  | `assert_  (**As [assert]. : if --no-assert is enabled, all this directive without exception are ignored ('assert false' too) *)
  | `fail (**As [assert false], with a message. : always fails, no matter if --no-assert is enabled or not. type : 'a *)
  | `typeof     (** -> WIP, don't use (yet) *)

  | `expand  of Big_int.big_int option     (**Marker for macro (function) that are macro-expanded, the integer represents the number of unrolling the compiler is authorised to do, it must do at least one *)
  | `restricted_bypass of string
      (** this directive should be produced by any pass inserting some restricted bypass,
          with a static string identifier to identify the pass.
          If a bypass is tagged using [bsltags] to be restricted, then checks are done to be sure that the call is authorized *)
  | `create_lazy_record
      (** For use by the database.
          [`create_lazy_record] creates a lazy record,
          with an optional information [info] (ie the expr list is either [e] or [e;info])
          Specifies that an expression evaluating to a record
          should be executed only when the record is effectively accessed.*)

  (** Explicit instantiation directives *)
  | `apply_ty_arg of ty list * ty_row list * ty_col list
  | `abstract_ty_arg of TypeVar.t list * RowVar.t list * ColVar.t list

  | `doctype of (string list * doctype_access_directive)
  | `hybrid_value
      (** Directive for qmljs. First expression is a client
          function whose type must be (string -> 'a).
          This expression is optional (the default value being \@unsafe_cast).
          Second expression is a string, that expression must be computed
          on the server. *)
  | `backend_ident of string
      (** A hack for passes that need to introduce unbound identifiers
          when they know the backend is going to generates the definitions
          Should be used only at the very end of the compilation passes
          since many passes don't want unbound identifiers
      *)
  | `tracker of PassTracker.t
  | `js_ident
      (** Get the js name of an ident, used by serialize of fun
          action. *)

  | `llarray
      (**
         Syntax creation for a low level array, from expression list of a static length.
         Used for optimisation traduction from huge structure, e.g. in js serialization.
      *)

  | `specialize of [ `strict | `polymorphic ]
      (**
         Used to replace magic functions by standard functions at compile time
         when the time is known at the call site
         It takes a list of at least one expression, the first one being the general case
         and the other expressions being the specialized cases.

         In case of a strict specialization, during the pass_SimplifyMagic an error
         is raised if the type cannot be specialized.
      *)

  | lambda_lifting_directive

  | `at_init
      (**
         Used to specify that a value must be computed during initialization, not inside a lambda
      *)

  | opavalue_directive

  | `tagged_string of string * tagged_string_kind
      (**
         Instead of putting static strings in the ast, we put these directives in the ast
         It allows latter passes to pattern match the ast easily to find the use and definition
         of types
         for instance, [\@typeof(void)] will become [{TyName_ident = \@tagged_string ("void", `type_use); TyName_args = \[\]}]
      *)

  | `recval
      (** should appear only in newvalrec and letrecin
          indicate it was explicitely allowed by the programmer
          to have a recursive expression that is not a lambda
          (and the validity of the recursion is checked at runtime)
      *)
]

(* TODO: remove Coerce form AST and use this instead; same for Parser; maybe others ? *)

and expr =
  | Const          of Annot.label * const_expr
  | Ident          of Annot.label * Ident.t
  | LetIn          of Annot.label * (Ident.t * expr) list * expr
  | LetRecIn       of Annot.label * (Ident.t * expr) list * expr
  | Lambda         of Annot.label * Ident.t list * expr
  | Apply          of Annot.label * expr * expr list
  | Match          of Annot.label * expr * (pat * expr) list
  | Record         of Annot.label * (string * expr) list
  | Dot            of Annot.label * expr * string
  | ExtendRecord   of Annot.label * string * expr * expr
  | Bypass         of Annot.label * BslKey.t
  | Coerce         of Annot.label * expr * ty
  | Path           of Annot.label * dbpath_expr_elt list * Db.kind
  | Directive      of Annot.label * qml_directive * expr list * ty list

and dbpath_expr_elt =
  | FldKey of string
  | ExprKey of expr
  | NewKey
(*   | Unpreprocessed of Db.element *)

(** First level of qml *)

and type_def_visibility =
  | TDV_public   (** Type definition is public, visible from anywhere. *)
  | TDV_abstract of ObjectFiles.package_name (** Type definition is visible
              from anywhere but internal representation is only visible inside
              the hosting package. *)
  | TDV_private of ObjectFiles.package_name (** Type definition is not exported
              outside the hosting package, i.e. doesn't appear in the package's
              interface. *)

(* ************************************************************************** *)
(** {b Descr}: Definition of a new type constructor.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
and typedef = {
  ty_def_options : ty_def_options ;
  (**
     More options on type definitions, from toplevel directives.
  *)

  ty_def_visibility : type_def_visibility ;
  (**
     Nature of the definition,
     governing how the defined type is visible.
  *)

  ty_def_name : typeident ;
  (** Name of the defined type. *)

  ty_def_params : typevar list ;
  (** Parameters of the defined type. *)

  ty_def_body : ty ;
  (** Body, i.e. effective structure of the defined type. *)
}

let (!!) (p : qml_directive) x = (x = p)
let comb l x = List.fold_left (fun b f -> f x || b) false l

type code_elt =
  | Database of Annot.label * Ident.t * (** The name of the database*)
                Db.path_decl * Db.options list
  | NewDbValue  of Annot.label * (expr,ty) Db.db_def
  | NewType     of Annot.label * typedef list
  | NewVal      of Annot.label * (Ident.t * expr) list
  | NewValRec   of Annot.label * (Ident.t * expr) list

type code = code_elt list

module Label :
sig
  val code_elt : code_elt -> Annot.label
  val expr : expr -> Annot.label
  val pat : pat -> Annot.label

    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> Annot.label *)
  module New :
  sig
    val code_elt : code_elt -> Annot.label -> code_elt
    val expr : expr -> Annot.label -> expr
    val pat : pat -> Annot.label -> pat


    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> Annot.t *)

  end
end =
struct
  let code_elt = Annot.Magic.label
  let expr = Annot.Magic.label
  let pat = Annot.Magic.label
  let ty = Annot.Magic.label
  module New =
  struct
    let code_elt = Annot.Magic.new_label
    let expr = Annot.Magic.new_label
    let pat = Annot.Magic.new_label
    let ty = Annot.Magic.new_label
  end
end

module Pos :
sig
  val code_elt : code_elt -> FilePos.pos
  val expr : expr -> FilePos.pos
  val pat : pat -> FilePos.pos


    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> FilePos.pos *)

  module New :
  sig
    val code_elt : code_elt -> Annot.pos -> code_elt
    val expr : expr -> Annot.pos -> expr
    val pat : pat -> Annot.pos -> pat


    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> Annot.t *)

  end

end =
struct
  let code_elt = Annot.Magic.pos
  let expr = Annot.Magic.pos
  let pat = Annot.Magic.pos
  let ty = Annot.Magic.pos
  module New =
  struct
    let code_elt = Annot.Magic.new_pos
    let expr = Annot.Magic.new_pos
    let pat = Annot.Magic.new_pos
    let ty = Annot.Magic.new_pos
  end
end

module QAnnot :
sig
  val code_elt : code_elt -> Annot.t
  val expr : expr -> Annot.t
  val pat : pat -> Annot.t

    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> Annot.t *)

  module New :
  sig
    val code_elt : code_elt -> Annot.t -> code_elt
    val expr : expr -> Annot.t -> expr
    val pat : pat -> Annot.t -> pat


    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> Annot.t *)

  end

  module Refresh :
  sig
    val code_elt : code_elt -> code_elt
    val expr : expr -> expr
    val pat : pat -> pat


    (* DO NOT EXPORT THEM NOW, IT WOULD SEG-FAULT *)

    (* 3thd part of the refactoring *)
    (* val ty : ty -> Annot.t *)

  end

end =
struct
  let code_elt = Annot.Magic.annot
  let expr = Annot.Magic.annot
  let pat = Annot.Magic.annot
  let ty = Annot.Magic.annot

  module New =
  struct
    let code_elt = Annot.Magic.new_annot
    let expr = Annot.Magic.new_annot
    let pat = Annot.Magic.new_annot
    let ty = Annot.Magic.new_annot
  end

  module Refresh =
  struct
    let fresh ast = Annot.Magic.new_annot ast (Annot.next ())
    let code_elt = fresh
    let expr = fresh
    let pat = fresh
    let ty = fresh
  end
end

(** Some code use a code maping : changing type code ==> change this function *)
let map_code = List.map

(** For the map or fold_map functionnality on code_elt,
    we need a representation to do a possible echo of the inferred type, or inferred value (qmltop.native)

    This is done to produce an qmli for example :

    +for Database, it is a simple echo
    +for NewType, it can changed when we add some verification of type definition (like type 'a bibi = 'b bobo)
    +for NewVal/Rec, it produces a unified M_NewVal
    +There in a other repr M_TopExpr for any expression in the first level of a source.

    Finaly, the type is parametric to be shared with the toplevel
    'a = typescheme         : for a normal typer only (see module QmlMakeTyper.Make, ex MakeAstTyper)
    'a = typescheme * value : for the top-level

    for M_NewType, it can diverge, and there is no value anyway : use 'b possibly different as 'a
*)
type ('a, 'b) maped_code_elt =
  | M_Failure of code_elt * (exn * exn list) (** exn list : for NewVal for example *)
  | M_Database of Ident.t * Db.path_decl * (Db.options list)
  | M_NewDbValue of Db.path_decl * 'a
  | M_DbAlias of Db.path_decl * Db.path_decl
  | M_DbDefault of Db.path_decl
  | M_DbConstraint of Db.path_decl * expr Db.db_constraint
  | M_DbVirtual of Db.path_decl * expr
  | M_NewType of (TypeIdent.t * 'b) list (* (QmlAst.typevar list * QmlAst.ty) : possible got from Scheme API *)
  | M_NewVal of (Ident.t * 'a) list
      (** Don't add M_NewValRec : it is included in M_NewVal !! *)
  | M_TopExpr of 'a

(** Mathieu : samedi 20 juin 2009, 12:08:19 (UTC+0100)

    The definitive answer to this existential question : << What is the [unvalrec] ? >>

    The [unvalrec] is introduced because at some point, there is no easy way to deal with recursive values at the first level of the language.
    so, the [unvalrec] uses a trivial transformation of first level recursive values to non-recursive first level values.

    see this example :

    ======================
    val rec a $x = $u
        and b $y = $v
    ======================

    where $x, $y denote the params of functions a, b and $u, $v the respective body of functions a and b

    In the ast, the representation of theses values are :

    NewValRec ( [ ( a, fun $x -> $u ) ; ( b, fun $y -> $v ) ]

    then it will be transformed :

    =================================
[
    val _v57_unvalrec =
       let rec a $x = $u
           and b $y = $v in
       {
          _v55_unvalrec_fun_a = a ;
          _v56_unvalrec_fun_b = b
       }

    val a = _v57_unvalrec._v55_unvalrec_fun_a ;
    and b = _v57_unvalrec._v56_unvalrec_fun_b ;
]
    =================================

    Remark :

    the syntax for val ... and is implemented in the parser,
    in the ast, this is the sense of the construction :  NewVal list

    This means that values in the same NewVal are independents and their computation could be for example parallelized.
    They must not be computed in a simple fold_left, because they share the same previous env, before the
    fist val of the list.

    The _57_unvalrec cannot be in the same NewVal because val a and val b depends on it of course.

    ==============================================
    What are the fields in the record [unvalrec] ??

    valrec : this is the original valrec : ( ident * expr ) list
    -------

        [ ( a , fun $x -> $u ) ;  ( b, fun $y -> $v ) ]

    letrec : this is the [let rec in] obtained from val rec -- it is a QmlAst.expr
    -------
[
       let rec a $x = $u
           and b $y = $v in
       {
          _v55_unvalrec_fun_a = a ;
          _v56_unvalrec_fun_b = b
       }
]
    freshval : this is the fresh generated value  --  ( ident * expr )
    ---------

    NewVal [ _v57_unvalrec , valet ] :

    corresponding to the following code in the example :
[
      val _v57_unvalrec =
        let rec a $x = $u
            and b $y = $v in
       {
          _v55_unvalrec_fun_a = a ;
          _v56_unvalrec_fun_b = b
       }
]
    newval : this is the function in the unvalrecursives form : ( ident * expr ) list
    -------
[
    NewVal [ ( a , _v57_unvalrec._v55_unvalrec_fun_a ) ; ( b ,  _v57_unvalrec._v56_unvalrec_fun_b ) ]
]
    corresponding to the following code in the example :
[
      val a = _v57_unvalrec._v55_unvalrec_fun_a ;
      and b = _v57_unvalrec._v56_unvalrec_fun_b ;
]

    BEWARE !!
    ---------

    The newval are using directly the [unvalrec] --
    so, if you need to evaluate it, of course you need to introduce the [freshval] value !

    ANNOTATIONS !!
    --------------

    In order that we can use the typer ( only defined on expr, not on first level recursive value) ,
    we ask the typer to type the [valet]

    So, all annotation of existing code are reproduced and not refreshed !!

    In the example :

    valrec [ ( a, AN[i0] ( fun $x -> $u ) ) ; a, AN[i1] ( fun $y -> $v ) ]

    valet : refreshing  annotations for the let, but for i0, i1 keep the same
    annotations

    EXTRA : QmlAst.code transformation in a passe of full [unvalrec]
    ------------------------------------------------------

    There is an extra function called [unvalrec_val] :

    [unvalrec_val]  : ( ident * expr ) list -> code

    ++returning a list of 2 NewVal : the freshval and the newval from the
    [unvalrec], simply concatenated

    [unvalrec_code] : code -> code

    for every NewValRec in the code, will replace it by the 2 new val produced
    by [unvalrec_val]
*)
type unvalrec =
    {
      valrec   : (Ident.t * expr) list ;
      letrec   : expr ;
      freshval : Ident.t * expr ;
      newval   : (Ident.t * expr) list ;
    }

(** ALL THE FUNCTIONS TO GENERATE UNVALREC ARE IN QMLASTCONS in the module
    UnValRec *)

(** Common modules *)
module TypeIdentMap : (BaseMapSig.S with type key = TypeIdent.t) = BaseMap.Make (
    struct
    type t = TypeIdent.t
    (* not [compare], because it needs to work for user-written names of types,
       so that we can find a type with a given name without knowing
       if it's abstract and if so, what the abstract stamp is *)
    let compare = TypeIdent.compare
    end )

module TypeIdentSet : (BaseSetSig.S with type elt = TypeIdent.t) = BaseSet.Make ( TypeIdent )

module TypeIdentTable = Base.Hashtbl.Make ( TypeIdent )


(* DEPRECATED :TODO ==> OManager *)

(**
   Debugging : common interface for all modules of libqmlcompil
   It is defined here because it is used by modules containing a open QmlAst
   With the design which try to control strictly the depandancies to typers in
   qml2llvm, the cmi of typers are not copied to the install directory, but
   qml2llvm needs to have access to the API of DebugInterfaces. So, it is no
   more defined in the typer-files.
*)

let quiet, set_quiet =
  let __quiet_ref = ref false in
  let quiet = fun () -> !__quiet_ref in
  let set_quiet b = __quiet_ref := b in
  quiet, set_quiet


module QmlDebugInterface =
  DebugTracer.MakeDebugInterface
    (struct
       let libname = "libqmlcompil"
       let version = "summer09"
       let quiet = quiet
       module DefaultColor =
       struct
         let error = `red
         let warning = `yellow
         let verbose = `cyan
         let withcolor = true
       end
     end)
#<<     (DebugTracer.DebugTracer)  >>#;

#<< let _ = if Base.debug_getenv_toggle "MLSTATE_HTML_DEBUG" then QmlDebugInterface.active() >>#;

module QmlConsole =
struct
  let is_typer_debug_on () = Base.debug_getenv_toggle "TYPER_DEBUG"
end
