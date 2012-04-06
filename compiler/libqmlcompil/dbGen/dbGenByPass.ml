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
(*
    @author1 Louis Gesbert
    @author2 Mathieu Barbin

    This module handles the dependence of DbGen on the outside world: it
    provided parameters to the DbGen functor to give it the identifiers it
    requires.
**)


(* shorthand *)
module Q = QmlAst

(* alias *)
module ExprIdent = Ident
module TypeIdent = Q.TypeIdent

module type SourceInterface =
sig
  val type_option : string
  val none        : string
  val some        : string

  val write : string

  val type_map    : string
  val intmap_empty   : string
  val intmap_add     : string
  val intmap_fold    : string
  val stringmap_empty   : string
  val stringmap_add     : string
  val stringmap_fold    : string
  val dbset_empty   : string

  val make_virtual_val : string
  val make_virtual_ref : string

  val val_to_val : string
  val ref_to_ref : string

  val dbset_genbuild : string
  val db3set_iterator : string
end

module QmlInterface : SourceInterface =
struct
  let type_option = Opacapi.Types.option
  let none        = "none"
  let some        = "some"

  let write = Opacapi.Db.write

  let type_map    = Opacapi.Types.map
  let intmap_empty   = "intmap_empty"
  let intmap_add     = "intmap_add"
  let intmap_fold    = "intmap_fold"
  let stringmap_empty   = "stringmap_empty"
  let stringmap_add     = "stringmap_add"
  let stringmap_fold    = "stringmap_fold"
  let dbset_empty   = Opacapi.DbMongoSet.empty
  let ref_to_ref = Opacapi.Db3.ref_to_ref

  let make_virtual_val = "make_virtual_val"
  let make_virtual_ref = "make_virtual_ref"

  let val_to_val = Opacapi.Db3.val_to_val
  let ref_to_ref = Opacapi.Db3.ref_to_ref
  let dbset_genbuild = Opacapi.DbMongoSet.genbuild
  let db3set_iterator = Opacapi.Db3Set.iterator
end

(* ======================================================================================================== *)
(** The signature for the module that DbGen takes as argument *)
module type S =
sig

  (** This module enables DbGen to find the idents it requires to interact with
      standard types ; at run-time, these idents need to be defined and to have
      the given types. The env can typically be used to hold an alpha-conv *)
  module ValInitial :
  sig
    type env
    val empty : env
    val none : env -> ExprIdent.t
    val some : env -> ExprIdent.t (** 'a -> 'a option *)

    val write : env -> ExprIdent.t

    val intmap_empty : env -> ExprIdent.t
    val intmap_add : env -> ExprIdent.t (** ('a,'b) map -> 'a -> 'b -> ('a,'b) map *)
    val intmap_fold : env -> ExprIdent.t (** ('acc -> 'a -> 'b -> 'acc) -> ('a,'b) map -> 'acc -> 'acc  *)
    val stringmap_empty : env -> ExprIdent.t
    val stringmap_add : env -> ExprIdent.t (** ('a,'b) map -> 'a -> 'b -> ('a,'b) map *)
    val stringmap_fold : env -> ExprIdent.t (** ('acc -> 'a -> 'b -> 'acc) -> ('a,'b) map -> 'acc -> 'acc  *)

    val dbset_empty : env -> ExprIdent.t

    val make_virtual_val : env -> ExprIdent.t (** val_path('e, 'a) -> ('a -> 'b) -> virtual_val_path('e, 'b) *)
    val make_virtual_ref : env -> ExprIdent.t (** ref_path('e, 'a) -> ('a -> 'r) -> ('w -> 'a) -> virtual_ref_path('e, 'r, 'w) *)

    (** Lists of all the mandatory idents used by DbGen. Should include all the
        above (used by the code splitter in the blender *)
    val list_of_idents : env -> ExprIdent.t list
    val list_of_typeidents : env -> TypeIdent.t list

    val val_to_val : env -> ExprIdent.t
    val ref_to_ref : env -> ExprIdent.t
    val dbset_genbuild : env -> ExprIdent.t
    val db3set_iterator : env -> ExprIdent.t
  end

end
  (* ======================================================================================================== *)

(* ======================================================================================================== *)
(** The signature to build a module S from a module which manage the initial value needed by dbgen *)
module type I =
sig
  type env
  val empty : env
  val conv : env -> ExprIdent.t -> ExprIdent.t
  val distributed : bool
end

module MakeS (N : SourceInterface) ( I : I ) : S with type ValInitial.env = I.env =
struct
  module ValInitial =
  struct
    type env = I.env
    let empty = I.empty
    (* Should correspond to stg defined in initial *)
    (* These are the only code called from qml within ocaml *)
    let none env = I.conv env (ExprIdent.source N.none)
    let some env = I.conv env (ExprIdent.source N.some) (* 'a -> 'a option *)

    let write env = I.conv env (ExprIdent.source N.write)

    let intmap_empty env = I.conv env (ExprIdent.source N.intmap_empty)
    let intmap_add env = I.conv env (ExprIdent.source N.intmap_add)
      (* 'a -> 'b -> ('a,'b) map -> ('a,'b) map *)
    let intmap_fold env = I.conv env (ExprIdent.source N.intmap_fold)
      (* ('a -> 'b -> 'acc -> 'acc) -> ('a,'b) map -> 'acc -> 'acc  *)
    let stringmap_empty env = I.conv env (ExprIdent.source N.stringmap_empty)
    let stringmap_add env = I.conv env (ExprIdent.source N.stringmap_add)
      (* 'a -> 'b -> ('a,'b) map -> ('a,'b) map *)
    let stringmap_fold env = I.conv env (ExprIdent.source N.stringmap_fold)
      (* ('a -> 'b -> 'acc -> 'acc) -> ('a,'b) map -> 'acc -> 'acc  *)

    let dbset_empty env = I.conv env (ExprIdent.source N.dbset_empty)

    let make_virtual_val env = I.conv env (ExprIdent.source N.make_virtual_val)
    let make_virtual_ref env = I.conv env (ExprIdent.source N.make_virtual_ref)

    let val_to_val env = I.conv env (ExprIdent.source N.val_to_val)
    let ref_to_ref env = I.conv env (ExprIdent.source N.ref_to_ref)

    let dbset_genbuild env = I.conv env (ExprIdent.source N.dbset_genbuild)
    let db3set_iterator env = I.conv env (ExprIdent.source N.db3set_iterator)

    let list_of_idents env = [ none env; some env; intmap_empty env; intmap_add env; intmap_fold env; stringmap_empty env; stringmap_add env; stringmap_fold env]
    let list_of_typeidents _ = [TypeIdent.of_string N.type_option; TypeIdent.of_string N.type_map;]
  end

end

let use_distributed_db = true

(** A first dummy version *)
module I_Unit : I with type env = unit =
struct
  type env = unit
  let empty = ()
  let conv _ id = id
  let distributed = use_distributed_db
end

(** The most common use of the env, is an alpha-conv option *)
(** The following module is used in QmlBlender, and in qmltop *)

module I_Alpha : I with type env = QmlAlphaConv.t option =
struct
  type env = QmlAlphaConv.t option
  let empty = None
  let conv env id =
    match env with
    | None -> id
    | Some alpha -> Option.default id (QmlAlphaConv.ident alpha id)
  let distributed = use_distributed_db
end

module BSLDbGen = MakeS ( QmlInterface ) ( I_Unit )
module BSLDbGenAlpha = MakeS ( QmlInterface ) ( I_Alpha )

module DbOpaInterface =
struct
  let type_option = "option"
  let none        = Opacapi.none
  let some        = Opacapi.some

  let write = Opacapi.Db.write

  let type_map    = "map"
  let intmap_empty   = Opacapi.IntMap.empty
  let intmap_add     = Opacapi.IntMap.add
  let intmap_fold    = Opacapi.IntMap.fold
  let stringmap_empty   = Opacapi.StringMap.empty
  let stringmap_add     = Opacapi.StringMap.add
  let stringmap_fold    = Opacapi.StringMap.fold
  let dbset_empty    = Opacapi.DbMongoSet.empty

  let make_virtual_val = Opacapi.DbVirtual.make_val
  let make_virtual_ref = Opacapi.DbVirtual.make_ref

  let val_to_val = Opacapi.Db3.val_to_val
  let ref_to_ref = Opacapi.Db3.ref_to_ref

  let dbset_genbuild = Opacapi.DbMongoSet.genbuild
  let db3set_iterator = Opacapi.Db3Set.iterator
end

(* Add dropbox opa interface here *)
module BSLDbGenAlphaOpa = MakeS ( DbOpaInterface )  ( I_Alpha )
