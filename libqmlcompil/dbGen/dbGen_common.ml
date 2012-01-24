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
let version = 9 (* Should be the same as in db3/Dbgraph *)

type path = string list

(** The typing of edges out of sets (aka multi nodes) *)
type multi_key =
  | Kint (** for intmaps *)
  | Kstring (** for stringmaps *)
  | Kfields of string list list (** for sets indexed by key (possibly multiple) *)

(** Edges of the schema *)
type edge_label =
  | Multi_edge of multi_key (** indexing keys for sets (aka primary keys) *)
  | Hidden_edge (** for hidden maps *)
  | SumCase of int (** cases in a sum type ; as for fields, the int should be a unique id *)
  | Field of string * int (** records, tuples, ... ; the int is the index in the implementation *)

type leaf =
  | Leaf_int
  | Leaf_float
  | Leaf_text
  | Leaf_binary

(** Nodes of the schema *)
type node_label =
  | Multi (** A set of data of the same type *)
  | Hidden (** A multi node that is implicit wrt to paths (for flattened rec types...) *)
  | Sum (** A data belonging to one of several record types *)
  | Product (** A record *)
  | Leaf of leaf (** A simply typed data *)

type schema_edge = { label: edge_label; is_main: bool }
type schema_node = {
  from_package : ObjectFiles.package_name;
  nodeid : string; (* = string_of_int(n)^package_name (except "root" and "dead") *)
  nlabel : node_label;
  ty : QmlAst.ty;
  default : QmlAst.expr option;
  constraints : QmlAst.expr QmlAst.Db.db_constraint list;
  context : QmlError.context;
  plain : bool;
}

let settyp, typ =
  let typ = ref (function _ ->
                   OManager.i_error "Function for name -> TypeIdent.t translation is not initialized") in
  (function f -> typ := f),
  (function s -> !typ s)

(* type of sets stored in the database *)
let tydbset ty = QmlAst.TypeName ([ty], typ Opacapi.Types.dbset)

(** Extract the type inside a dbset type [get_dbset_ty(dbset(t)) = t]. *)
let get_dbset_ty = function
  | QmlAst.TypeName ([x], id) ->
      assert(QmlAst.TypeIdent.to_string id = "dbset"); x
  | ty -> OManager.i_error "Wait a dbset type receive : %a" QmlPrint.pp#ty ty

let firstclass_path_tyid () =
  typ Opacapi.Types.path_t
let val_p_tyid () =
  typ Opacapi.Types.path_val_p
let ref_p_tyid () =
  typ Opacapi.Types.path_ref_p
let val_path_ty ty =
  QmlAst.TypeName ([QmlAst.TypeName ([],val_p_tyid ()); ty],
                   firstclass_path_tyid ())

let get_val_path_ty = function
  | QmlAst.TypeName ([_; rty], _) -> rty
  | ty -> OManager.error "Type of val_path seems malformed : %a"
      QmlPrint.pp#ty ty

let ref_path_ty ty =
  QmlAst.TypeName ([QmlAst.TypeName ([],ref_p_tyid ()); ty],
                   firstclass_path_tyid ())
let val_v_tyid () =
  typ Opacapi.Types.virtual_val_path
let ref_v_tyid () =
  typ Opacapi.Types.virtual_ref_path

(** Construct type [virtual_val_path('a, rty)]*)
let virtual_val_path_ty rty =
  QmlAst.TypeName ([rty], val_v_tyid ())

(** Construct type [virtual_ref_path('a, rty, wty)]*)
let virtual_ref_path_ty rty wty =
  QmlAst.TypeName ([rty; wty], ref_v_tyid ())

(* Warning: the names (including prefixes) of the types are hardcoded
   in the three definitions below. *)
let tydb () =
  QmlAst.TypeName ([], typ Opacapi.Types.badoplink_database)

let engine_opt opts =
  match Base.List.filter_map (function `engine e -> Some e | _ -> None) opts with
  | [engine] -> engine
  | [] -> `db3 None (* default engine, if any requested *)
  | _::_ -> failwith "Ambiguous database engine specification"
