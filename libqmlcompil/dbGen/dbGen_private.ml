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
   Private module for DB-Accessors generation.
   @author Louis Gesbert
   @author Mathieu Barbin (errors report)
*)

(* depends *)
module Format = Base.Format
module List = BaseList

(* shorthands *)
module Q = QmlAst
module DbAst = QmlAst.Db
module C = DbGen_common
module H = DbGenHelpers

(* alias *)
module AnnotTable = H.AnnotTable
module ExprIdent = Ident
module QC = QmlAstCons.UntypedExpr
module V = Schema_private.V
module E = Schema_private.E
module SchemaGraph = SchemaGraphLib.SchemaGraph
module SchemaGraph0 = SchemaGraphLib.SchemaGraph0


(** the module AnnotTable is in dbGenHelpers *)

#<Debugvar:DBGEN_DEBUG>

let error ?e fmt = AnnotTable.emergency_close();
  match e with
    | Some e ->
        let context = QmlError.Context.expr e in
        let context = Schema_private.HacksForPositions.map context in
        QmlError.error context fmt
    | None ->
        let context = QmlError.Context.shame_on_me_i_am_too_lazy "DbGen_private.error" in
        QmlError.error context fmt

let debug fmt = OManager.printf ( "@{<magenta>[dbgen]@} "^^fmt^^"@." )

type dbinfo = {
  access_fun: Schema_private.vertex -> Q.ident -> Q.expr -> Q.expr;
  (* node trans dbpath => expr *)

  writer: Schema_private.vertex -> Q.expr -> Q.expr -> Q.expr -> Q.expr;
  (* node -> tr -> path -> value -> wr expr *)

  default_expr: Schema_private.vertex -> Q.expr;
  edge_num_expr: Schema_private.edge -> Q.expr;
  (* given an edge in the current schema,
     returns the corresponding edge id in the current database *)

  db_ident: Q.ident;
  (* user-visible (unless no db declarations) ident for this db mount point *)
}

let merge_dbinfo dbinfo1 _dbinfo2 = dbinfo1


let data_root_keylist = [1]
let db_config_keylist = [2]
let db_internal_keylist = [3] (* for the licencing hacks; temporarily unused *)
let config_keys = [`schema, 0; `version, -1]
let (@:) a b = H.(@:) a b

module Default = struct

  let rec expr_aux sch n =
    H.copy_expr (H.newexpr_annot (expr_for_def sch n) (SchemaGraphLib.type_of_node n))

  and expr_for_def sch n =
    let ty = SchemaGraphLib.type_of_node n in
    let _ =
      let pos = QmlError.Context.get_pos ((V.label n).C.context) in
      H.start_built_pos pos in
    let r =
      match (V.label n).C.default with
      | Some e -> e
      | None ->
          if Schema_private.has_dflt_in_parents sch n then
            let e = SchemaGraphLib.get_parent_edge sch n in
            H.make_dot ~ty (expr_aux sch (E.src e)) (SchemaGraphLib.fieldname_of_edge e)
          else
            match (V.label n).C.nlabel with
            | C.Multi -> (match SchemaGraphLib.multi_key sch n with
                          | C.Kint -> H.make_record ["empty", H.make_record []]
                          | C.Kstring -> H.make_record ["empty", H.make_record []]
                          | C.Kfields _ -> H.make_record ["empty", H.make_record []]
                              (* TODO - ... *))
            | C.Hidden -> expr_aux sch (SchemaGraph.unique_next sch n)
            | C.Sum -> H.convert_case_to_sum ty (expr_aux sch (E.dst (Schema_private.find_nonrec_child_edge sch n)))
            | C.Product ->
                H.newexpr_annot
                  (QC.record
                     (List.map (fun e -> SchemaGraphLib.fieldname_of_edge e, expr_aux sch (E.dst e)) (SchemaGraph0.succ_e sch n)))
                  ty
            | C.Leaf C.Leaf_int -> H.const_int 0
            | C.Leaf C.Leaf_text -> H.const_string ""
            | C.Leaf C.Leaf_binary -> H.const_string ""
            | C.Leaf C.Leaf_float -> H.newexpr_annot (QC.const (Q.Float 0.)) ty
    in H.end_built_pos ();
    r

  let expr annotmap sch n =
    let () = H.AnnotTable.open_table ~annotmap:(Some annotmap) () in
    let e = expr_aux sch n in
    (match H.AnnotTable.close_table () with
     | None -> assert false
     | Some a -> a, e)

end

module CodeGenerator ( Arg : DbGenByPass.S ) = struct
  (* aliases *)
  module Helpers_gen = H.Helpers_gen (Arg)
  module Bypass = Helpers_gen.Bypass
  module Db = Schema_private.Db

  let typath = Helpers_gen.typath
  let tytrans = Helpers_gen.tytrans
  let tykey = Helpers_gen.tykey
  let tydiff = Helpers_gen.tydiff

  (* -------- DB parameters and access auxiliaries ---------- *)

  let make_some e =
    H.make_record ["some", e]
(*     let ty = AnnotTable.getTypeOfAnnot e.Q.annot in *)
(*       H.apply_lambda (expr_some ty) e *)

  let debug_in _msg e =
    #<If$minlevel 10> debug "Insert run-time debug: %s" _msg #<End>;
    #<If> H.make_letin (H.new_ident "_", H.apply_lambda (Bypass.jlog()) (H.const_string _msg)) e
    #<Else> e
    #<End>

  let debugverbose_in _msg e =
    #<If$minlevel 20> debug_in _msg e
    #<Else> e
    #<End>

  let match_option_expr e ?(tyval=H.type_inside_option e) fun_some expr_none =
    match e with
    | Q.Apply (_, Q.Ident (_, id_some), [e]) when id_some = Helpers_gen.id_some() ->
        fun_some e
    | _ ->
        let value = H.new_ident "x" in
        H.make_match e
          [ H.patt_some_var value tyval, fun_some (value @: tyval);
            H.patt_none tyval, expr_none ]

  let expr_equals e1 e2 = (* fixme: how to do this in a less verbose way ? *)
    let ty = H.type_from_annot e1 in
    assert (H.type_from_annot e2 = ty);
    H.make_match
      (H.apply_lambda' (Bypass.compare ty) [e1;e2])
      [ H.patt_const_int 0, H.expr_true();
        H.newpatt_annot (QC.patany ()) H.tyint, H.expr_false() ]

  let expr_switch e caselist dflt =
    let ty1 = H.type_from_annot e in
    let ty2 = H.type_from_annot (snd (List.hd caselist)) in
    assert (List.for_all (fun (e1,e2) -> H.type_from_annot e1 = ty1 && H.type_from_annot e2 = ty2) caselist);
    let id = H.new_ident "switch" in
    H.make_letin (id, e)
      (List.fold_right
         (fun (e1,e2) acc ->
            H.make_ifthenelse (expr_equals (id @: ty1) e1) e2 acc)
         caselist dflt)

  let key_int e = H.apply_lambda (Bypass.key_int()) e
  let key_const_int i = key_int (H.const_int i)
  let key_string e = H.apply_lambda (Bypass.key_string()) e
  let key_const_string s = key_string (H.const_string s)
  let rec key_list gamma l_fields e =
    let l_fields =
      List.sort (fun a b -> String.compare (fst a) (fst b)) l_fields in
    let id = H.new_ident "record_key" in
    let ty = Q.TypeRecord (Q.TyRow(l_fields,None)) in
    let rec key_of (fn,fty) = match fty with
    | Q.TypeConst Q.TyInt -> key_int (H.make_dot ~ty:fty (id @: ty) fn)
    | Q.TypeConst Q.TyString -> key_string (H.make_dot ~ty:fty (id @: ty) fn)
    | Q.TypeRecord (Q.TyRow(l_fields,_)) -> key_list gamma l_fields (H.make_dot ~ty:fty (id @: ty) fn)
    | Q.TypeName (tylst,tid) ->
      let context = QmlError.Context.expr e in
      let fty = Schema_private.get_type_from_name ~context gamma tylst tid in
      key_of (fn,fty)
    | _ -> error ("Set keys containing fields of type [%a] are not supported yet.") QmlPrint.pp#ty fty in
    let l_key = List.map key_of l_fields in
    H.make_letin (id, e) (H.apply_lambda (Bypass.key_list()) (Bypass.make_ocaml_list l_key (tykey ())))
  let dbpath_add path key = H.apply_lambda' (Bypass.dbpath_add()) [path;key]

  let path_of_intkeylist () = List.fold_left (fun acc i -> dbpath_add acc (key_const_int i)) (Bypass.dbpath_root())
  let data_root () = path_of_intkeylist () data_root_keylist
  let config_key_path k = path_of_intkeylist () (db_config_keylist @ [List.assoc k config_keys])

  let leaf_data leaf_t value =
    match leaf_t with
      | C.Leaf_int -> H.apply_lambda (Bypass.data_int()) value
      | C.Leaf_float -> H.apply_lambda (Bypass.data_float()) value
      | C.Leaf_text -> H.apply_lambda (Bypass.data_text()) value
      | C.Leaf_binary -> H.apply_lambda (Bypass.data_binary()) value

  let apply_inside_opt f_expr opt_expr =
    let ty2 =
      match AnnotTable.getTypeOfAnnot (Q.QAnnot.expr f_expr) with
      | Q.TypeArrow ([ty1], ty2) ->
          assert
            (let ty = AnnotTable.getTypeOfAnnot (Q.QAnnot.expr opt_expr) in
             ty = Q.typeNull || ty = H.typeoption ty1 ||
                 #<If>
                    debug "apply_inside_opt: %a ~=~ %a"
                      QmlPrint.pp#ty (H.typeoption ty1)
                      QmlPrint.pp#ty (AnnotTable.getTypeOfAnnot (Q.QAnnot.expr opt_expr));
                    false
                 #<Else> false #<End>);
          ty2
      | Q.TypeArrow _ -> assert false
      | ty when ty = Q.typeNull -> Q.typeNull
      | _ -> assert false
    in
    match_option_expr opt_expr
      (fun data -> make_some (H.apply_lambda f_expr data))
      (Helpers_gen.expr_none ty2)

  let get_code tr path leaf_t =
    apply_inside_opt (Bypass.proj_dbtype leaf_t)
      (H.apply_lambda' (Bypass.get_opt())
         [tr @: (tytrans()); path])

  let get_code_noopt tr path leaf_t =
    let id = H.new_ident "x" in
    let ty = SchemaGraphLib.type_of_leaf leaf_t in
    H.make_match
      (get_code tr path leaf_t)
      [ H.patt_some_var id ty, id @: ty;
        H.patt_none ty, H.apply_lambda' (Bypass.fatal_error ty)
          [H.const_string "Impossible to read structure data from the database";H.const_string "";H.const_string""] ]

  let set_code tr path value leaf_t =
    H.apply_lambda' (Bypass.set()) [tr @: (tytrans()); path; leaf_data leaf_t value]

  let set_unit_code tr path =
    H.apply_lambda' (Bypass.set()) [tr @: (tytrans()); path; H.apply_lambda' (Bypass.data_unit()) []]

  let enter_transaction_expr db =
    debugverbose_in "ENTER TRANSACTION" (
    match_option_expr (H.apply_lambda (Bypass.get_global_transaction_opt()) (db @: C.Db.t ()))
      (fun tr -> tr)
      (H.apply_lambda (Bypass.trans_start()) (db @: C.Db.t ()))
    )

  let leave_transaction_expr db tr =
    debugverbose_in "LEAVE TRANSACTION" (
    match_option_expr (H.apply_lambda (Bypass.get_global_transaction_opt()) (db @: C.Db.t ()))
      (fun _ -> H.apply_lambda' (Bypass.set_global_transaction()) [db @: C.Db.t (); tr @: (tytrans())])
      (H.apply_lambda' (Bypass.trans_commit()) [tr @: (tytrans())])
    )

  (* Label of keys inside the edge-map, which is a record containing the
     bindings high-level edge -> physical db edge *)
  let edge_fld_label edge =
    Printf.sprintf "edge_%s_%d" (V.label (E.src edge)).C.nodeid (SchemaGraphLib.fieldid_of_edge edge)


  (* --------- Generation functions ----------- *)

  let patt_of_node sch node =
    match (V.label node).C.nlabel with
      | C.Product ->
          let fieldmap, fields =
            SchemaGraph0.fold_succ_e (
              fun e (fieldmap, fields) ->
                let f = SchemaGraphLib.fieldname_of_edge e in
                let id_f = H.new_ident f in
                let fieldmap = (f, id_f) :: fieldmap in
                let sub_pat =
                  H.newpatt_annot (QC.patvar id_f)
                    (SchemaGraphLib.type_of_node (E.dst e))
                in
                let fields = (f, sub_pat) :: fields in
                fieldmap, fields
            )
              sch node
              ([], [])
          in
          let pat = Q.PatRecord (H.nolabel, fields, `closed) in
          fieldmap, H.newpatt_annot pat (SchemaGraphLib.type_of_node node)
      | _ -> assert false

  let newkey_expr tr path =
    key_int
      (H.apply_lambda' (Bypass.get_new_key())
        [tr; path])

  (** From a dbpath pointing to the source of a link edge, returns a dbpath
      pointing to the parent of the destination of that link. Links within
      elements of a map should be handled correctly. *)
  (* The algorithm isn't trivial: we go up then down to find the "shortest" path,
     then use the uppath and add_key bypasses to build the corresponding path *)
  let dbpath_from_link_expr sch edge_num_from_edge tr edge dbpath =
    assert (not (E.label edge).C.is_main); (* edge is a link *)
    let fail () =
      failwith (Printf.sprintf "Invalid link to %s at %s"
                  (SchemaGraphLib.string_path_of_node sch (E.dst edge))
                  (SchemaGraphLib.string_path_of_node sch (E.src edge))) in
    let src, dst = E.src edge, E.dst edge in
    let rec parents_list acc n = if SchemaGraphLib.is_root n then n::acc
      else parents_list (n::acc) (E.src (SchemaGraphLib.get_parent_edge sch n)) in
    let up = parents_list [] src in
    let down = parents_list [] dst in
    let rec find_last_tail a = function
      | x::r when x = a -> Some r | _::r -> find_last_tail a r | [] -> None in
    let rec find_shortest acc upl downl = match upl with
      | x::r -> (match find_last_tail x downl with
                   | Some down -> (x::acc, down)
                   | None -> find_shortest (x::acc) r downl)
      | [] -> assert false (* a tree's rooted *)
    in
    let up, down = find_shortest [] (List.rev up) down in
    let up_expr =
      List.fold_left (fun path _ -> H.apply_lambda' (Bypass.uppath()) [tr @: (tytrans()); path]) dbpath up in
    let _, updown_expr =
      List.fold_left
        (fun (nprev, path) n ->
           (n,
            match (V.label nprev).C.nlabel with
              | C.Multi ->
                  (match SchemaGraphLib.multi_key sch nprev with
                     | C.Kint -> dbpath_add path (newkey_expr (tr @: (tytrans())) path)
                     | C.Kfields _ ->
                         error "linking to within a set not handled yet" (* todo *)
                     | C.Kstring -> fail())
              | C.Hidden -> dbpath_add path (key_const_int 0)
              | C.Sum ->
                  (* dbpath_add path (key_const_int 0) with override ? *)
                  error "Unhandled write to a link pointing to a specific case of a sum type"
              | C.Product ->
                  dbpath_add path
                    (key_int (edge_num_from_edge (SchemaGraph0.find_edge sch nprev n)))
              | _ -> fail()))
        (List.hd up, up_expr)
        down
    in
      updown_expr

  (* ------------------------------------------------------------ *)
  (* This module generates the whole database accessors code      *)
  (* ------------------------------------------------------------ *)

  (* GenAccessors generates default values, readers and writers *)
  module GenAccessors = struct

    (* Idents for default values and accessors *)

    module Idents = struct
      type t = {
        default_idents : (string * ExprIdent.t option) list;
        reader_idents  : (string * ExprIdent.t option) list;
        writer_idents  : (string * ExprIdent.t option) list;
        generic_reader : (C.leaf * ExprIdent.t) list;
        generic_writer : (C.leaf * ExprIdent.t) list;
        edge_map_ids   : (ExprIdent.t * Q.ty) StringMap.t (* package_name -> edgemap id * its type *)
      }

      let merge idents1 idents2 =
        {
          default_idents = idents1.default_idents @ idents2.default_idents;
          reader_idents  = idents1.reader_idents @ idents2.reader_idents;
          writer_idents  = idents1.writer_idents @ idents2.writer_idents;
          generic_reader = idents1.generic_reader;
          generic_writer = idents1.generic_writer;
          edge_map_ids   =
            StringMap.merge
              (fun id1 _id2 -> id1)
              idents1.edge_map_ids
              idents2.edge_map_ids
        }

      let generate sch =
        let default_idents, reader_idents, writer_idents =
          SchemaGraph0.fold_vertex
            (fun n (d_acc,r_acc,w_acc) -> let nodeid = (V.label n).C.nodeid in
             let d,r,w =
               if SchemaGraphLib.is_root n then None, None, None
               else match (V.label n).C.nlabel with
               | C.Leaf _l -> None, None, None
                   (* No accessors for leaves, they use the generic ones *)
               | C.Multi when SchemaGraphLib.is_node_set sch n ->
                   None,
                   (Some (H.new_ident (Printf.sprintf "get_dbset%s" nodeid))),
                   (Some (H.new_ident (Printf.sprintf "set_dbset%s" nodeid)))
                   (*
                     | C.Hidden -> None, None, None
                   (* No accessors for hidden nodes as well, paths should be
                     built to access their children directly *)
                   *)
               | C.Product ->
                   (None, (* Default accessor of product is useless *)
                    Some (H.new_ident ("get_n"^nodeid)),
                    Some (H.new_ident ("set_n"^nodeid)))
               | _ ->
                   (Some (H.new_ident ("default_n"^nodeid)),
                    Some (H.new_ident ("get_n"^nodeid)),
                    Some (H.new_ident ("set_n"^nodeid)))
             in
             ( (nodeid,d)::d_acc, (nodeid,r)::r_acc, (nodeid,w)::w_acc)
            )
            sch ([],[],[])
        in {
          default_idents = default_idents;
          reader_idents  = reader_idents;
          writer_idents  = writer_idents;
          generic_reader =
            (let int = H.new_ident "r_int" in
             let binary = H.new_ident "r_binary" in
             let text = H.new_ident "r_text" in
             let float = H.new_ident "r_float" in
             [(C.Leaf_int, int); (C.Leaf_binary, binary);
              (C.Leaf_text, text); (C.Leaf_float, float)]
(*              function C.Leaf_int -> int | C.Leaf_binary -> binary | C.Leaf_text -> text | C.Leaf_float -> float *));
          generic_writer =
            (let int = H.new_ident "w_int" in
             let binary = H.new_ident "w_binary" in
             let text = H.new_ident "w_text" in
             let float = H.new_ident "w_float" in
             [(C.Leaf_int, int); (C.Leaf_binary, binary);
              (C.Leaf_text, text); (C.Leaf_float, float)]
(*              function C.Leaf_int -> int | C.Leaf_binary -> binary | C.Leaf_text -> text | C.Leaf_float -> float *));
          edge_map_ids =
            (let package = ObjectFiles.get_current_package_name() in
             let edgemap_edges =
               SchemaGraph0.fold_edges_e
                 (fun e acc ->
                    if SchemaGraphLib.package_of_node (E.dst e) <> package then acc
                    else match (E.label e).C.label with
                    | C.SumCase _ | C.Field _ -> edge_fld_label e :: acc
                    | _ -> acc)
                 sch []
             in
             StringMap.singleton package
               (H.new_ident ("edge_map_"^package),
                H.tyrecord (List.map (fun lbl -> lbl, H.tyint) edgemap_edges)))
        }

      let get_default idents n = List.assoc (V.label n).C.nodeid idents.default_idents
      let get_reader  idents n =
        List.assoc (V.label n).C.nodeid idents.reader_idents
      let get_writer  idents n = List.assoc (V.label n).C.nodeid idents.writer_idents

      let get_generic_reader idents lf = List.assoc lf idents.generic_reader
      let get_generic_writer idents lf = List.assoc lf idents.generic_writer

    end

    (* Default values *)
    module Default = struct

      let rec expr sch idents n = match Idents.get_default idents n with
      | Some id -> id @: SchemaGraphLib.type_of_node n
      | None -> H.copy_expr (H.newexpr_annot (expr_for_def sch idents n) (SchemaGraphLib.type_of_node n))
      and expr_for_def sch idents n =
        let ty = SchemaGraphLib.type_of_node n in
        let _ =
          let pos = QmlError.Context.get_pos ((V.label n).C.context) in
          H.start_built_pos pos in
        let r =
        match (V.label n).C.default with
        | Some e -> e
        | None ->
            if Schema_private.has_dflt_in_parents sch n then
              let e = SchemaGraphLib.get_parent_edge sch n in
              H.make_dot ~ty (expr sch idents (E.src e)) (SchemaGraphLib.fieldname_of_edge e)
            else
              match (V.label n).C.nlabel with
              | C.Multi -> (match SchemaGraphLib.multi_key sch n with
                          | C.Kint -> Helpers_gen.expr_intmap_empty ty
                          | C.Kstring -> Helpers_gen.expr_stringmap_empty ty
                          | C.Kfields _ -> assert false
                              (* sets cannot be accessed directly *))
              | C.Hidden -> expr sch idents (SchemaGraph.unique_next sch n)
              | C.Sum -> H.convert_case_to_sum ty (expr sch idents (E.dst (Schema_private.find_nonrec_child_edge sch n)))
              | C.Product ->
                  H.newexpr_annot
                    (QC.record
                       (List.map (fun e -> SchemaGraphLib.fieldname_of_edge e, expr sch idents (E.dst e)) (SchemaGraph0.succ_e sch n)))
                    ty
              | C.Leaf C.Leaf_int -> H.const_int 0
              | C.Leaf C.Leaf_text -> H.const_string ""
              | C.Leaf C.Leaf_binary -> H.const_string ""
              | C.Leaf C.Leaf_float -> H.newexpr_annot (QC.const (Q.Float 0.)) ty
        in H.end_built_pos ();
        r


      let add_valdefs sch idents =
        let default_values =
          SchemaGraph0.fold_vertex
            (fun n acc ->
               match Idents.get_default idents n with
               | Some id -> (id, expr_for_def sch idents n)::acc
               | None -> acc)
            sch [] in
        let rec aux acc dflt = (* builds nested letand / letin *)
          let ids = List.map fst dflt in
          match List.partition (fun (_id,e) -> H.are_ids_free_in_expr ids e) dflt with
          | [],[] -> acc
          | [],_dflt1 -> error "Unsound recursive types in DB definitions"
          | dflt0,dflt1 ->
              aux (fun e -> acc (
                     let label = Annot.nolabel "DbGen_private.gen_node_accessors" in
                     (Q.NewVal (label, dflt0)) :: e)
                  ) dflt1
        in aux (fun e -> e) default_values

    end

    module Type = struct
      let generic_reader leaf =
        let ty = SchemaGraphLib.type_of_leaf leaf in
        H.tyfun [ty; (tytrans()); (typath()); H.over_valpath_ty ty]
          (* == ty -> reader *)
      let reader n =
        H.tyfun [(tytrans()); (typath()); H.over_valpath_ty (SchemaGraphLib.type_of_node n)]

      let generic_writer leaf =
        let ty = SchemaGraphLib.type_of_leaf leaf in (* == db_writer_type *)
        H.tyfun [(tytrans()); (typath()); ty; (tytrans())]
      let writer n =
        H.tyfun [(tytrans()); (typath()); SchemaGraphLib.type_of_node n; (tytrans())]

    end

    let make_embedded_path _node tr path =
      H.apply_lambda' (Bypass.embedded_path ())
        [tr @: (tytrans()); path ]

    let edge_map_infos sch db_ident db_diff_id =
(*       let oc1 = open_out ("schema") in *)
(*       let _ = Schema_io.to_dot sch oc1 in *)
(*       let _ = close_out oc1 in *)
      let defs = SchemaGraph0.fold_edges_e
        (fun e acc ->
           if SchemaGraphLib.package_of_node (E.dst e) <> ObjectFiles.get_current_package_name() then acc
           else match (E.label e).C.label with
           | C.SumCase i | C.Field (_,i) ->
               let nodeid = (V.label (E.src e)).C.nodeid in
               let edgeid_expr =
                 if nodeid = "root" then
                   (* for separated compilation: query the hashtbl initialised during `init *)
                   H.apply_lambda' (Bypass.get_registered_root_edge())
                     [ H.const_string (Ident.original_name db_ident);
                       H.const_string (SchemaGraphLib.package_of_node (E.dst e));
                       H.const_int i ]
                 else
                   H.const_int i
               in
               (edge_fld_label e,
                H.apply_lambda' (Bypass.matching_edge())
                  [ db_diff_id @: (tydiff ()); H.const_string nodeid; edgeid_expr ])
               :: acc
           | _ -> acc)
        sch [] in
      let record = H.make_record defs in
      let label = Annot.nolabel "DbGen_private.gen_node_accessors" in
      (label, record)

    (* For mapping with a pre-existing db, from its embedded schema *)
    let def_edge_map (label, record) edge_map_id =
      Q.NewVal (label, [edge_map_id, record])
    let edge_num_expr edge_map_ids =
      (fun edge ->
         let package_name = SchemaGraphLib.package_of_node (E.dst edge) in
         let edge_map_id, edge_map_ty = StringMap.find package_name edge_map_ids in
         H.make_dot ~ty:H.tyint (edge_map_id @: edge_map_ty) (edge_fld_label edge))

    let register_root_edge_map db_ident unrenumbered_sch sch =
      let defs =
        SchemaGraph0.fold_succ_e
          (fun e acc ->
             match (E.label e).C.label with
             | C.Field (fieldname, edgeid) ->
                 let package_name = SchemaGraphLib.package_of_node (E.dst e) in
                 let db_name = Ident.original_name db_ident in
                 let source_edge_id =
                   match
                     List.filter
                       (fun e ->
                          SchemaGraphLib.package_of_node (E.dst e) = package_name &&
                          SchemaGraphLib.fieldname_of_edge e = fieldname)
                       (SchemaGraph0.succ_e unrenumbered_sch (SchemaGraphLib.get_root unrenumbered_sch))
                   with
                   | [e] -> SchemaGraphLib.fieldid_of_edge e
                   | _ -> assert false
                 in
                 (H.new_ident "_",
                  H.apply_lambda' (Bypass.register_root_edge())
                    [ H.const_string db_name;
                      H.const_string package_name;
                      H.const_int source_edge_id;
                      H.const_int edgeid ])
                 :: acc
             | _ -> assert false)
          sch (SchemaGraphLib.get_root sch) [] in
      let expr = H.make_letand defs (H.expr_unit()) in
      let label = Annot.nolabel "DbGen_private.gen_node_accessors" in
      Q.NewVal (label, [H.new_ident "_", expr])


    module Reader = struct

      module Generic = struct
        let expr leaf =
          let ty = SchemaGraphLib.type_of_leaf leaf in
          let dflt = H.new_ident "default" in
          let tr = H.new_ident "tr" in
          let path = H.new_ident "curpath" in
          H.make_lambda' [dflt, ty; tr, (tytrans()); path, (typath())]
            (let res = H.new_ident "res" in
             H.make_letin
               (res,
                get_code tr (path @: (typath())) leaf)
               (match_option_expr (res @: H.typeoption ty)
                  (fun x -> x)
                  (dflt @: ty)))
      end

      let expr sch idents n tr path =
        match Idents.get_reader idents n with
        | Some acc ->
            H.apply_lambda' (acc @: Type.reader n) [tr; path]
        | None ->
            match (V.label n).C.nlabel with
            | C.Leaf leaf ->
                H.apply_lambda' (Idents.get_generic_reader idents leaf @: Type.generic_reader leaf)
                  [Default.expr sch idents n; tr; path]
            | C.Multi when SchemaGraphLib.is_node_set sch n ->
                assert false (* Reader of dbset are generated on idents *)
            | _ -> assert false

      let get_multi sch idents tr path n ty =
        let nextnode = SchemaGraph.unique_next sch n in
        let tychld = SchemaGraphLib.type_of_node nextnode in
        (* see get_dbset, get_multi should not be called on set node *)
        let expr_empty () = match SchemaGraphLib.multi_key sch n with
          | C.Kint -> Helpers_gen.expr_intmap_empty ty
          | C.Kstring -> Helpers_gen.expr_stringmap_empty ty
          | _ -> assert false in
        let expr_read =
          H.apply_lambda'
            (Bypass.fold_children tychld ty)
            [
              tr @: (tytrans());
              path @: (typath());
              (let tr, path = H.new_ident "tr", H.new_ident "path" in
               H.make_lambda' [tr, (tytrans()); path, (typath())]
                 (expr sch idents nextnode (tr @: (tytrans())) (path @: (typath()))));
              (let acc, key, value = H.new_ident "acc", H.new_ident "key", H.new_ident "value" in
               H.make_lambda'
                 [acc, ty; key, (tykey ()); value, tychld]
                 (match SchemaGraphLib.multi_key sch n with
                  | C.Kint ->
                      H.apply_lambda'
                        (Helpers_gen.expr_stringmap_add H.tyint tychld)
                        [ H.apply_lambda (Bypass.key_value_int()) (key @: (tykey ()));
                          value @: tychld;
                          acc @: ty ]
                  | C.Kstring ->
                      H.apply_lambda'
                        (Helpers_gen.expr_stringmap_add H.tystring tychld)
                        [ H.apply_lambda (Bypass.key_value_string()) (key @: (tykey ()));
                          value @: tychld;
                          acc @: ty ]
                  | _ -> assert false));
              expr_empty();
            ]
        in
        #<If:DBGEN_FLAGS$flag "sharing">
          let value = H.new_ident "value" in
          H.make_letin (value, expr_read)
            (H.make_ifthenelse (expr_equals (value @: ty) (expr_empty())) (* Do not embed data if empty *)
               (value @: ty)
               (H.apply_lambda'
                  (Bypass.embed_record_data ty)
                  [ value @: ty; make_some (make_embedded_path n tr (path @: (typath())))]))
        #<Else>
          expr_read
        #<End>

      let get_dbset sch idents tr path node dbsetty =
        (* Extract type inside dbset *)
        let ty = C.get_dbset_ty dbsetty  in
        (* The dbset reader is the reader that read a child node *)
        let reader =
          let child = SchemaGraph.unique_next sch node in
          match Idents.get_reader idents child with
          | None -> OManager.i_error "Child reader of a dbset not found"
          | Some reader -> reader @: Type.reader child in
        H.apply_lambda' (Bypass.create_dbset ty)
          [tr @: (tytrans()); path @: (typath()); reader]

      let def sch idents =
        let make_accessor n =
          let _ =
            let pos = QmlError.Context.get_pos ((V.label n).C.context) in
            H.start_built_pos pos in
          let ty0 = SchemaGraphLib.type_of_node n in
          let ty = H.over_valpath_ty ty0 in
          let tr = H.new_ident "tr" in
          let path = H.new_ident "curpath" in
          let r = H.make_lambda'
            [ tr, (tytrans());
              path, (typath()) ]
            (H.make_coerce ~ty
               (match (V.label n).C.nlabel with
                | C.Multi when SchemaGraphLib.is_node_set sch n ->
                    get_dbset sch idents tr path n ty0
                | C.Multi -> get_multi sch idents tr path n ty0
                | C.Hidden ->
                    expr sch idents (SchemaGraph.unique_next sch n) (tr @: (tytrans()))
                      (dbpath_add (path @: (typath())) (key_const_int 0))
                | C.Sum ->
                    expr_switch
                      (get_code tr (path @: (typath())) C.Leaf_int)
                      (List.map
                         (fun e ->
                            let edgenum() = edge_num_expr idents.Idents.edge_map_ids e in
                            make_some (edgenum()),
                            H.make_coerce
                              (H.convert_case_to_sum
                                 ~lazy_param:(Some (make_embedded_path n tr (path @: (typath()))))
                                 ty0
                                 (expr sch idents (E.dst e) (tr @: (tytrans()))
                                    (dbpath_add (path @: (typath())) (key_int (edgenum()))))))
                         (SchemaGraph0.succ_e sch n))
                      (Default.expr sch idents n)
                | C.Product ->
                    H.make_coerce
                      (H.make_lazyrecord
                         (make_embedded_path n tr (path @: (typath())))
                         (List.map
                            (fun e ->
                               let f = SchemaGraphLib.fieldname_of_edge e in
                               f,
                               expr sch idents (E.dst e) (tr @: (tytrans()))
                                 (dbpath_add (path @: (typath())) (key_int (edge_num_expr idents.Idents.edge_map_ids e))))
                            (SchemaGraph0.succ_e sch n))
                         ty0)
                | C.Leaf _ -> assert false)) (* handled by accessors_generic *)
          in H.end_built_pos (); r
        in
        SchemaGraph0.fold_vertex
          (fun n acc ->
             match Idents.get_reader idents n with
             | None -> acc
             | Some id -> (id, make_accessor n)::acc)
          sch []

    end

    module Writer = struct

      module Generic = struct
        let expr leaf =
          let ty = SchemaGraphLib.type_of_leaf leaf in
          let tr = H.new_ident "tr" in
          let path = H.new_ident "curpath" in
          let value = H.new_ident "value" in
          H.make_lambda' [tr, (tytrans()); path, (typath()); value, ty]
            (set_code tr (path @: (typath())) (value @: ty) leaf)
      end

      let expr sch idents n =
        match Idents.get_writer idents n with
        | Some id -> id @: Type.writer n
        | None -> match (V.label n).C.nlabel with
          | C.Leaf leaf -> Idents.get_generic_writer idents leaf @: Type.generic_writer leaf
          | C.Multi when SchemaGraphLib.is_node_set sch n ->
              H.apply_lambda (Bypass.error (Type.writer n)) (H.const_string "Writing in a database set node is forbidden. This feature will be available in a future version.")
          | _ -> assert false

      let apply_writer sch idents node tr path_expr value =
        H.apply_lambda'
          (expr sch idents node)
          [tr @: (tytrans()); path_expr; H.make_coerce (H.newexpr_annot value (SchemaGraphLib.type_of_node node))]


      let write_child sch idents tr parent_path key edge value =
        let n = E.dst edge in
        if (E.label edge).C.is_main then
          apply_writer sch idents n tr (dbpath_add (parent_path @: (typath())) key) value
        else
          (* Write a recursive structure:
             (1) create node with new key at the link-to point
             -- need to find the path of the map: express the link in terms of ../.. -> this is done by dbpath_from_link_expr
             -- append the node to the map, with a fresh key
             (2) link to the newly created node
             Remark: if we have a 1c path here, maybe we could try & link instead of generating a new key *)
          let dbpath_dest = H.new_ident "dbpath_dest"
          and newkey = H.new_ident "newkey"
          and newpath_dest = H.new_ident "newpath_dest"
          and tr1 = H.new_ident "trans1" in
          let by_link_or_copy =
            #<If:DBGEN_FLAGS$flag "copylink">
              (Bypass.set_current_copy())
              #<Else> (Bypass.set_link())
              #<End>
          in
          H.make_letin'
            [ dbpath_dest,
              dbpath_from_link_expr sch (edge_num_expr idents.Idents.edge_map_ids) tr edge (parent_path @: (typath()));
              newkey,
              newkey_expr (tr @: (tytrans())) (dbpath_dest @: (typath()));
              newpath_dest,
              dbpath_add (dbpath_dest @: (typath())) (newkey @: (tykey ()));
              tr1,
              apply_writer sch idents n tr (newpath_dest @: (typath())) value ]
            (H.apply_lambda' by_link_or_copy
               [ tr1 @: (tytrans());
                 dbpath_add (parent_path @: (typath())) key;
                 newpath_dest @: (typath()) ])

      let write_multi sch idents tr path n _ty value =
        let tykeys, key_make = match SchemaGraphLib.multi_key sch n with
        | C.Kint -> H.tyint, key_int
        | C.Kstring -> H.tystring, key_string
        | C.Kfields _ -> assert false (* todo: implement indexed writing to sets ! *)
        in
        let edge = SchemaGraph.out_edge sch n in
        let tychld = SchemaGraphLib.type_of_node (E.dst edge) in
        let tr1 = H.new_ident "loc_trans1" in
        let tr2 = H.new_ident "loc_trans2" in
        H.make_letin'
          [tr2, H.apply_lambda' (Bypass.clear())
             (* warn: that would break a link: we should not allow maps as destinations of links in the schema *)
             [tr @: (tytrans()); path @: (typath())];
           tr1, set_unit_code tr2 (path @: (typath()))]
          (* Cleanup the node before re-setting *)
          (H.apply_lambda'
             (match SchemaGraphLib.multi_key sch n with
              | C.Kint -> Helpers_gen.expr_intmap_fold tykeys tychld (tytrans())
              | C.Kstring -> Helpers_gen.expr_stringmap_fold tykeys tychld (tytrans())
              | _ -> assert false)
             [ (let k,x,acc = H.new_ident "k", H.new_ident "x", H.new_ident "trans_acc" in
                H.make_lambda' [k, tykeys; x, tychld; acc, (tytrans())]
                  (write_child sch idents acc path (key_make (k @: tykeys)) edge (QC.ident x)));
               value;
               tr1 @: (tytrans()) ])

      let set_dbset sch idents tr path value node dbsetty =
        (* Extract type inside dbset *)
        let ty = C.get_dbset_ty dbsetty in
        (* The dbset writer is the child writer *)
        let writer =
          let child = SchemaGraph.unique_next sch node in
          match Idents.get_writer idents child with
          | None -> OManager.i_error "Child writer of a dbset not found"
          | Some writer -> writer @: Type.writer child in
        H.apply_lambda' writer
          [tr @: (tytrans()); path @: (typath()); value @: ty]

      let def sch idents =
        let make_writer n =
          let _ =
            let pos = QmlError.Context.get_pos ((V.label n).C.context) in
            H.start_built_pos pos in
          let ty = SchemaGraphLib.type_of_node n in
          let value = H.new_ident "cur_value" in
          let tr = H.new_ident "trans" in
          let path = H.new_ident "path" in
          let attempt_to_copy e =
            #<If:DBGEN_FLAGS$flag "sharing">
              (H.make_match
                 (H.apply_lambda (Bypass.get_lazy_info_opt ty) (value @: ty))
                 [ (let ldata = H.new_ident "lazy_data" in
                    H.patt_some_var ldata (Helpers_gen.ty_lazy_data ()),
                    debug_in "This data is already in the DB: mark as copy instead of write"
                      (H.apply_lambda' (Bypass.copy ty)
                         [ tr @: (tytrans()); ldata @: Helpers_gen.ty_lazy_data (); path @: (typath()) ]));
                   H.patt_none (C.val_path_ty ty),
                   debug_in "Marking your data with DB info (for later sharing)"
                     (let tr = H.new_ident "tr" in
                      H.make_letin'
                        [ tr, e;
                          H.new_ident "_",
                          (H.apply_lambda' (Bypass.inject_record_data ty)
                             [ value @: ty;
                               make_some
                                 (H.apply_lambda' (Bypass.embedded_path ())
                                    [tr @: (tytrans()); path @: (typath())]) ]) ]
                        (tr @: (tytrans()))) ])
              #<Else> e
              #<End>
          in
          let r = H.make_lambda'
            [ tr, (tytrans());
              path, (typath());
              value, ty ]
            (* Writes belong to one of three cases:
               (1) paths that always lead to a recursive call (eg rectypes == multi with a default case)
               these don't access the contents of "value" anyway.
               (2) then try to see if we can do a path_copy instead of writing, checking for hidden lazy data
               (3) handle actual writes for the remaining cases *)
            (
              debugverbose_in (Printf.sprintf "Writing node %s (%s)" (V.label n).C.nodeid (SchemaGraphLib.string_path_of_node sch n))
                (match (V.label n).C.nlabel with
                 | C.Multi when SchemaGraphLib.is_node_set sch n ->
                     set_dbset sch idents tr path value n ty
                 | C.Multi -> attempt_to_copy (write_multi sch idents tr path n ty (value @: ty))
                 | C.Hidden ->
                     (* we don't copy in these cases, which are hidden to the user: no valpath can point here *)
                     let tr1 = H.new_ident "tr1" in
                     H.make_letin
                       (tr1, H.apply_lambda' (Bypass.remove_children()) [tr @: (tytrans()); (path @: (typath()))])
                       (write_child sch idents tr1 path (key_const_int 0) (SchemaGraph.out_edge sch n) (QC.ident value))
                 | C.Sum ->
                     let tr1 = H.new_ident "tr1" in
                     attempt_to_copy
                       (H.make_letin
                          (tr1, H.apply_lambda' (Bypass.remove_children()) [tr @: (tytrans()); (path @: (typath()))])
                          (H.make_match (value @: ty)
                             (List.map
                                (fun e ->
                                   let edgenum() = edge_num_expr idents.Idents.edge_map_ids e in
                                   let fldlist,patt = patt_of_node sch (E.dst e) in
                                   let tr2 = H.new_ident "tr2" in
                                   patt,
                                   H.make_letin
                                     (tr2, set_code tr1 (path @: (typath())) (edgenum()) C.Leaf_int)
                                     (write_child sch idents tr2 path (key_int (edgenum())) e
                                        (* (value @: ty).e *)
                                        (QC.record (* = (value @: ty).e, only useful for typing/conversion *)
                                           (List.map
                                              (fun (fd,id) -> fd, id @: (SchemaGraphLib.type_of_node (E.dst (SchemaGraphLib.find_field_edge sch (E.dst e) fd))))
                                              fldlist))))
                                (SchemaGraph0.succ_e sch n))))
                 | C.Product -> attempt_to_copy
                     (let tr_expr = (* record presence of empty records: *)
                        if SchemaGraph0.succ_e sch n = []
                        then set_unit_code tr (path @: (typath()))
                        else (tr @: (tytrans()))
                      in
                      SchemaGraph0.fold_succ_e
                        (fun e acc ->
                           let tr = H.new_ident "tr" in
                           H.make_letin (tr, acc)
                             (write_child sch idents tr path (key_int (edge_num_expr idents.Idents.edge_map_ids e)) e
                                (QC.dot (value @: ty) (SchemaGraphLib.fieldname_of_edge e))))
                        sch n tr_expr)
                 | C.Leaf _leaf -> assert false)) (* Handled by the generic writers *)
          in H.end_built_pos (); r
        in
        SchemaGraph0.fold_vertex
          (fun n acc -> match Idents.get_writer idents n with
           | Some id -> (id, make_writer n)::acc
           | None -> acc)
          sch []

    end

  end

  module S =
  struct
    type t = GenAccessors.Idents.t StringListMap.t
    let pass = "pass_DbAccessorsGeneration"
    let pp f _ = Format.pp_print_string f "<dummy>"
  end
  module R = ObjectFiles.Make(S)


  let gen_node_accessors sch db_ident db_diff_id merge_with_idents =
(*     let db_default_val, db_accessor_fun, db_writer_fun = GenAccessors.idents sch *)
(*     in *)
    let current_package_name = ObjectFiles.get_current_package_name() in
    let partial_sch =
      SchemaGraphLib.schema_of_package sch current_package_name
    in
    let partial_idents = GenAccessors.Idents.generate partial_sch in
(*     let idents = R.fold GenAccessors.Idents.merge partial_idents in *)
    let idents = merge_with_idents partial_idents in
    (* todo (opt): if generic readers/writers have been already generated
       in a previous package, don't generate them again *)
    let default_values_def = GenAccessors.Default.add_valdefs partial_sch idents in
    (* For mapping with a pre-existing db, from its embedded schema *)
    let edge_map_infos = GenAccessors.edge_map_infos sch db_ident db_diff_id in
    let edge_map_id, _edge_map_ty = StringMap.find current_package_name idents.GenAccessors.Idents.edge_map_ids in
    let def_edge_map = GenAccessors.def_edge_map edge_map_infos edge_map_id in
    let edge_num_expr = GenAccessors.edge_num_expr idents.GenAccessors.Idents.edge_map_ids in
    let accessor_generic_expr leaf = GenAccessors.Reader.Generic.expr leaf in
    let apply_accessor n = GenAccessors.Reader.expr sch idents n in
    let accessor_funs = GenAccessors.Reader.def partial_sch idents in
    let writer_generic_expr leaf = GenAccessors.Writer.Generic.expr leaf in
    let apply_writer n = GenAccessors.Writer.apply_writer sch idents n in
    let writer_funs = GenAccessors.Writer.def partial_sch idents in

    let def_vals =
      let label = Annot.nolabel "DbGen_private.gen_node_accessors" in
      default_values_def
        [
          def_edge_map ;
          Q.NewVal (label, List.map (fun lf -> GenAccessors.Idents.get_generic_reader idents lf, accessor_generic_expr lf) SchemaGraphLib.leaves) ;
          Q.NewVal (label, List.map (fun lf -> GenAccessors.Idents.get_generic_writer idents lf, writer_generic_expr lf) SchemaGraphLib.leaves) ;
          Q.NewValRec (label, accessor_funs) ;
          Q.NewValRec (label, writer_funs) ;
        ]
    in
    let get_expr n tr path =
      assert ((V.label n).C.nlabel <> C.Hidden);
      apply_accessor n (tr @: (tytrans())) path
    in
    let write_expr n tr path value =
      assert ((V.label n).C.nlabel <> C.Hidden);
      let trid = H.new_ident "tr" in
      H.make_letin (trid,tr) (apply_writer n trid path value)
    in
    def_vals, get_expr, write_expr, GenAccessors.Default.expr sch idents, edge_num_expr, partial_idents

  let serial_schema schema =
    (* Io.Compr.compress_string @* *) Schema_io.to_gml_string schema
  (* let compute_hash = Digest.to_hex @* Digest.string @* Schema_io.to_gml_string *)

  let init_ret_ty () =
    Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:false ["trans", (tytrans()); "diff", (tydiff ())])

  let set_init_code serial_sch tr =
    let tr' = H.new_ident "tr" in
    debug_in (Printf.sprintf "Initialising database, hash=%S" (Digest.to_hex (Digest.string serial_sch)))
    (let tr_df_ty = H.tyrecord ["trans",Helpers_gen.(tytrans()); "diff",Helpers_gen.(tydiff ())] in
    let schema_id = H.new_ident "schema" in
    let tr_df_diff = H.new_ident "tr_df" in
    (H.make_letin'
      [schema_id, H.const_string serial_sch;
       (tr_df_diff,
         (H.make_letin
            (tr', set_code tr (config_key_path `version) (H.const_int DbGen_common.version) C.Leaf_int)
            (H.make_record
               ["trans", (set_code tr' (config_key_path `schema) (H.const_string serial_sch) C.Leaf_binary);
               "diff", (Bypass.empty_diff())])))]
      (H.make_tuple (schema_id @: H.tystring) (tr_df_diff @: tr_df_ty))))

  let check_init_code db_ident serial_sch tr =
    (* Don't print the full filename when testing *)
    let filename () =
      #<If:TESTING>
        H.const_string "the database files"
      #<Else>
        H.apply_lambda (Bypass.db_prefix (C.Db.t ())) (db_ident @: C.Db.t ())
      #<End> in
    let msg_error =
      [ H.const_string "Can't recognise the meta-data in this database. You can clear it by removing \"";
        filename();
        H.const_string "*\"." ] in
    let msg_version =
      [ H.const_string ("This database was created using an older version of the OPA compiler.\n"^
                      "You can clear it by removing \"");
        filename();
        H.const_string ("*\".\nUpdate will be available in an upcoming version.") ] in
    let msg_sch =
      [ H.const_string ("The database was created with a different application, and cannot be migrated.\n"^
                      "Automatic update supports add, remove and rename, extension and reduction of sum types.\n"^
                      "To continue, remove your database files (\"");
        filename();
        H.const_string ("*\"). Or contact us for more options.") ]
    in
    let schema_id = H.new_ident "schema" in
    let read_schema_id = H.new_ident "read_schema" in
    let diff_id = H.new_ident "diff" in
    let tr_df_id = H.new_ident "tr_df" in
    let tr_df_ty = H.tyrecord ["trans",(tytrans()); "diff",(tydiff ())] in
    let ret_ty = H.tytuple H.tystring tr_df_ty in
    let diffedsch =(H.new_ident "diffed_schema") in
    H.make_letin'
      [ schema_id, H.const_string serial_sch;
        H.new_ident "_",
        (H.make_match (get_code tr (config_key_path `version) C.Leaf_int)
           [ H.patt_some (H.patt_const_int DbGen_common.version),
               H.expr_unit ();
             H.patt_some (H.newpatt_annot (QC.patany ()) H.tyint),
               H.apply_lambda' (Bypass.fatal_error H.tyunit) msg_version;
             H.patt_none H.tyint,
               H.apply_lambda' (Bypass.fatal_error H.tyunit) msg_error]) ]
      (H.make_letin
         (read_schema_id, get_code_noopt tr (config_key_path `schema) C.Leaf_binary)
         (H.make_letin
            (diff_id,
               H.apply_lambda' (Bypass.diff())
                 [read_schema_id @: H.tystring; schema_id @: H.tystring])
            (H.make_match
               (H.apply_lambda (Bypass.diff_status()) (diff_id @: (tydiff ())))
               [ H.patt_const_int 0, (* See codes in mlbsl/dbgraph.ml ! 0 is no difference *)
                   (H.make_letin
                     (tr_df_id, H.make_record ["trans", tr @: (tytrans()); "diff", diff_id @: (tydiff ())])
                     (H.make_tuple (schema_id @: H.tystring) (tr_df_id @: tr_df_ty)));
                 H.patt_const_int 2048, (* this means we can't migrate *)
                   H.apply_lambda' (Bypass.fatal_error ret_ty) msg_sch;
                 H.newpatt_annot (QC.patany ()) H.tyint,
                   H.make_letin
                     (H.new_ident "_",
                        H.apply_lambda (Bypass.jlog())
                          (H.apply_lambda' (Bypass.diff_message())
                             [H.const_string
                                "The structure of this database doesn't match the current program.\nIt differs by ";
                              (diff_id @: (tydiff ()))]))
                     (H.make_ifthenelse (H.apply_lambda' (Bypass.shall_i_upgrade()) [db_ident @: C.Db.t ()])
                        (H.make_letin
                           (H.new_ident "_",
                            H.apply_lambda (Bypass.jlog()) (H.const_string "Automatic update done. Saving..."))
                           (H.make_letin
                             (diffedsch,
                              (H.apply_lambda (Bypass.get_diffed_schema()) (diff_id @: (tydiff ()))))
                             (H.make_letin
                               (tr_df_id,
                                (H.make_record
                                   ["trans", set_code tr (config_key_path `schema) (diffedsch @: H.tystring) C.Leaf_binary;
                                    "diff", diff_id @: (tydiff ())]))
                                (H.make_tuple (diffedsch @: H.tystring) (tr_df_id @: tr_df_ty)))))
                        (H.make_letin
                           (H.new_ident "_",
                            H.apply_lambda (Bypass.jlog())
                              (H.const_string "Automatic update is possible. If you agree with the above database migration\ninformation, re-run with the option --db-force-upgrade."))
                           (H.apply_lambda' (Bypass.fatal_error ret_ty)
                              [H.const_string "Not doing database upgrade, giving up now.";
                               H.const_string "";
                               H.const_string ""])))
                     ])))

  let gen_dbinit db_ident db_diff_id point schema gamma engine_id engine_expr engine_type =
    (* The ids have to be fresh and passed along, because many are generated
       for many dbs. The mount point suffixes are only added for debug. *)
    let db_name = String.concat "_" ("db" :: point) in
    let engine_decl = Q.NewVal (Annot.nolabel ("DbGen_private.engine."^db_name), [ engine_id, engine_expr ]) in
    let db_id = H.new_ident db_name in
    let db_exp = H.apply_lambda' (Bypass.open_db()) [engine_id @: engine_type] in
    let tr1_id = H.new_ident "tr1" in
    let tr1_exp = H.apply_lambda (Bypass.trans_start()) (db_id @: C.Db.t ()) in
    let rec_ty = H.tyrecord ["trans",Helpers_gen.(tytrans()); "diff",Helpers_gen.(tydiff ())] in
    let init_tuple_ty = H.tytuple H.tystring rec_ty in
    let init_tuple_id = H.new_ident "init_tuple" in
    let serial_sch = serial_schema schema in
    let init_tuple_exp =
      H.make_match
        (H.apply_lambda (Bypass.is_db_new()) (db_id @: C.Db.t ()))
        [
          (H.newpatt_annot (QC.patconst (Q.Int 1)) H.tyint,
          set_init_code serial_sch tr1_id);
          (H.newpatt_annot (QC.patconst (Q.Int 0)) H.tyint,
           check_init_code db_id serial_sch tr1_id)
        ]
    in

    let schema_id = H.new_ident "schema" in
    let schema_exp = H.make_fst (init_tuple_id @: init_tuple_ty) in
    let config_id = H.new_ident "node_config" in
    let config_exp = H.apply_lambda (Bypass.node_config_construct()) (schema_id @: H.tystring) in
    let properties = H.apply_lambda' (Bypass.node_properties()) [db_id @: C.Db.t (); config_id @: Helpers_gen.tynodeconfig ()] in

    let init_record_id = H.new_ident "init_record" in
    let init_record_exp = H.make_snd (init_tuple_id @: init_tuple_ty) in
    let tr2_id = H.new_ident "tr2" in
    let tr2_exp = H.make_dot (init_record_id @: (init_ret_ty ())) "trans" in
    let trans_commit_exp =
      H.apply_lambda'
        (Bypass.trans_commit()) [tr2_id @: (tytrans())]
    in
    let database_field = db_id @: C.Db.t () in
    let diff_field =
      let diff = H.make_dot (init_record_id @: (init_ret_ty ())) "diff" in
      #<If>
        let diff_id = H.new_ident "diff" in
        H.make_letin'
          [
            (diff_id, diff);
            (H.new_ident "_",
            H.apply_lambda (Bypass.jlog())
              (H.apply_lambda (Bypass.print_tree())
                (H.apply_lambda
                  (Bypass.get_diffed_schema()) (diff_id @: (tydiff ())))))
          ]
          (diff_id @: (tydiff ()))
          #<Else>
          diff
          #<End>
    in
    let general_init =
      H.make_letin'
        [
          db_id, db_exp;
          tr1_id, tr1_exp;
          init_tuple_id, init_tuple_exp;
          schema_id, schema_exp;
          config_id, config_exp;
          H.new_ident "_", properties;
          init_record_id, init_record_exp;
          tr2_id, tr2_exp;
          H.new_ident "_", trans_commit_exp;
        ]
        (H.make_record [("database", database_field); ("diff", diff_field)])
    in
    let dbinit_ty =
      Q.TypeRecord
        (QmlAstCons.Type.Row.make
           ~extend:false ["database", C.Db.t (); "diff", (tydiff ())])
    in
    let register_db_init =
      H.apply_lambda'
        (Bypass.register_db_ident dbinit_ty)
        [H.const_string ("db_init__" ^ (Ident.original_name db_ident));
         general_init]
    in
    let label = Annot.nolabel "DbGen_private.gen_node_accessors" in
    let init_decl = Q.NewVal (label, [ H.new_ident "_", register_db_init ]) in
    (* let gamma = *)
    (*   QmlTypes.Env.Ident.add *)
    (*     db_ident (QmlTypes.Scheme.quantify (C.Db.t ())) gamma *)
    (* in *)
    let gamma =
      QmlTypes.Env.Ident.add
        db_diff_id (QmlTypes.Scheme.quantify (tydiff ())) gamma
    in
    (gamma, engine_decl, init_decl)

  let setup_db_accessors gamma schema db_ident db_diff_id merge_with_idents =
    let accessor_defs, access_fun, writer, default_expr, edge_num_expr, partial_idents =
      gen_node_accessors schema db_ident db_diff_id merge_with_idents
    in
    let gamma = List.fold_left
      (fun gamma -> function
        | Q.NewVal (_, defs) | Q.NewValRec (_, defs) ->
            List.fold_left
              (fun gamma (id,e) ->
                QmlTypes.Env.Ident.add id
                  (QmlTypes.Scheme.quantify
                    (H.type_from_annot e))
                  gamma)
              gamma defs
        | _ -> assert false)
      gamma accessor_defs
    in
    let dbinfo =
      {
        access_fun = access_fun;
        writer = writer;
        default_expr = default_expr;
        edge_num_expr = edge_num_expr;
        db_ident = db_ident;
      }
    in
    (dbinfo, gamma, accessor_defs, partial_idents)

  (* returns (engine_expr, engine_type) *)
  let gen_engine engine db_ident_opt ~is_multi =
      let wrap_opt f x ty =
        match x with
        | Some x -> make_some (f x)
        | None   -> Helpers_gen.expr_none ty
      in
      let db_name_opt =
        wrap_opt H.const_string
          (Option.map ExprIdent.original_name
             (if is_multi then db_ident_opt else None))
             (* database name in options is only required as soon as there are multiple dbs *)
          H.tystring
      in
      let engine_options = match engine with
        | `db3 file ->
            H.apply_lambda' (Bypass.local_options())
              [ db_name_opt; wrap_opt H.const_string file H.tystring ]
        | `db3light file ->
            H.apply_lambda' (Bypass.light_options())
              [ db_name_opt; wrap_opt H.const_string file H.tystring ]
        | `meta ->
            H.apply_lambda' (Bypass.local_options())
              [ db_name_opt; Helpers_gen.expr_none H.tystring ]
        | `client (host,port) ->
            H.apply_lambda' (Bypass.client_options())
              [ db_name_opt;
                wrap_opt H.const_string host H.tystring;
                wrap_opt H.const_int port H.tyint ]
      in
      H.apply_lambda (Bypass.make_engine()) engine_options, Helpers_gen.tyengine

  (* generate the db declaration which will be put in the package
     note: it uses a bypass to obtain the db ident which will be set at `init *)
  let gen_db_decl gamma db_ident db_diff_id =
    let dbinit_id = H.new_ident "dbinit" in
    let dbinit_ty =
      Q.TypeRecord
        (QmlAstCons.Type.Row.make
           ~extend:false ["database", C.Db.t (); "diff", (tydiff ())])
    in
    let init =
      H.apply_lambda
        (Bypass.get_registered_db_ident dbinit_ty)
        (H.const_string ("db_init__" ^ (Ident.original_name db_ident)))
    in
    let label = Annot.nolabel "DbGen_private.gen_node_accessors" in
    let dec1 = dbinit_id, init in
    let dec2 = db_ident, H.make_dot (dbinit_id @: dbinit_ty) "database" in
    let dec3 = db_diff_id, H.make_dot (dbinit_id @: dbinit_ty) "diff" in
    List.fold_left_map
      (fun gamma ((i,e) as dec) ->
         let gamma = QmlTypes.Env.Ident.add i (QmlTypes.Scheme.id (AnnotTable.getTypeOfAnnot (Q.QAnnot.expr e))) gamma in
         gamma, Q.NewVal (label, [dec])
      ) gamma [dec1; dec2; dec3]

  let initialize ?(annotmap=None) ?(valinitial_env=Arg.ValInitial.empty) gamma sch =
    let number_of_dbs = StringListMap.size sch in
    let _ = AnnotTable.open_table ~annotmap () in
    let _ =
      (* setting up the default env used to shorten the code generation calls;
         see Helpers_gen in dbGenHelpers.ml *)
      Helpers_gen.valinitial_env_ref := valinitial_env
    in
    if number_of_dbs = 0 then
      let _ = R.save StringListMap.empty in
      (StringListMap.empty, gamma, AnnotTable.close_table (), [], [])
    else try
      (* engine defs need to be put before for options parsing *)
      let dbinfo_map, gamma, engine_defs, db_defs, defs, idents_to_store =
        let setup_dbs point db_def (dbinfo_map, gamma, engine_defs, db_defs, defs, idents_to_store) =
(*           if Schema_private.package_name_of_def db_def <> ObjectFiles.get_current_package_name() *)
(*           then (dbinfo_map, gamma, defs) *)
(*           else *)
          assert (db_def.Schema_private.options.DbAst.backend = `db3);
          let engine = `meta in
          let engine_id =
            H.new_ident (Printf.sprintf "engine_%s"
                         (ExprIdent.original_name db_def.Schema_private.ident))
          in
          let engine_expr,engine_type = gen_engine engine (Some db_def.Schema_private.ident) ~is_multi:(number_of_dbs > 1) in
          let db_diff_name = String.concat "_" ("db_diff" :: point) in
          let db_diff_id = H.new_ident db_diff_name in
          let db_ident = db_def.Schema_private.ident in
          let new_map, gamma, engine_defs, db_decls, more_defs, idents_to_store =
            if ObjectFiles.compilation_mode() = `init
            then
              let sch = Schema_private.renumber_root_edges db_def.Schema_private.schema in
              let (gamma, engine_decl, init_decl) =
                gen_dbinit db_ident db_diff_id point sch gamma
                  engine_id engine_expr (engine_type())
              in
              let gamma, db_decls = gen_db_decl gamma db_ident db_diff_id in
              let root_edge_map_decl =
                GenAccessors.register_root_edge_map db_ident db_def.Schema_private.schema sch
              in
              dbinfo_map,
              gamma,
              engine_defs @ [engine_decl],
              (init_decl::db_decls),
              [root_edge_map_decl],
              idents_to_store
            else
              let merge idents_map1 idents_map2 =
                (* merge idents of a given database
                   in a map from database to idents *)
                match (StringListMap.find_opt point idents_map1,
                       StringListMap.find_opt point idents_map2) with
                | Some idents1, Some idents2 ->
                    let idents = GenAccessors.Idents.merge idents1 idents2 in
                    StringListMap.singleton point idents
                | Some idents, _ | _, Some idents ->
                    (* note: some packages may have not generated idents
                       for the current database *)
                    StringListMap.singleton point idents
                | _ ->
                    (* should never happen
                       because the init of the fold contains a value at key [point]*)
                    StringListMap.empty
              in
              let merge_with_idents idents =
                StringListMap.find point
                  (R.fold merge (StringListMap.singleton point idents))
              in
              let dbinfo, gamma, more_defs, partial_idents =
                setup_db_accessors gamma db_def.Schema_private.schema db_ident db_diff_id merge_with_idents
              in
              let new_map = StringListMap.add point dbinfo dbinfo_map in
              let gamma, db_decls = gen_db_decl gamma db_ident db_diff_id in
              let idents_to_store = StringListMap.add point partial_idents idents_to_store in
              new_map, gamma, engine_defs, db_decls, more_defs, idents_to_store
          in
          (new_map, gamma, engine_defs, db_defs @ db_decls, defs @ more_defs, idents_to_store)
        in
        let init = StringListMap.empty, gamma, [], [], [], StringListMap.empty in
        StringListMap.fold setup_dbs sch init
      in
      let check_args =
        if ObjectFiles.compilation_mode() = `init then
          [ Q.NewVal (Annot.nolabel "DbGen_private.check_args",
                      [H.new_ident "_", H.apply_lambda' (Bypass.check_remaining_arguments ()) []])]
        else []
      in
      let _ = R.save idents_to_store in
      let _ = Helpers_gen.valinitial_env_ref := Arg.ValInitial.empty in (* reset *)
      (dbinfo_map, gamma, AnnotTable.close_table (), engine_defs @ check_args @ db_defs, defs )
    with
    | e -> AnnotTable.emergency_close (); raise e

  let magic_newkey_expr dbinfo path =
    let tr, res = H.new_ident "tr", H.new_ident "res" in
    H.make_letin'
      [ tr, enter_transaction_expr dbinfo.db_ident;
        res, newkey_expr (tr @: (tytrans())) path;
        H.new_ident "_", leave_transaction_expr dbinfo.db_ident tr ]
      (res @: (tykey ()))

  (* /!\ Synchronise with find_exprpath in Schema_private /!\ *)

  let pathcode_of_qmlpath sch dbinfo gamma ?(node0 = SchemaGraphLib.get_root sch) ?(dbpath = data_root()) path0 =
    let write_link_if_necessary wr_expr path edge key_expr =
      if (E.label edge).C.is_main then wr_expr
      else
        fun tr ->
          let pathid = H.new_ident "path" and tr2 = H.new_ident "tr" and chldpath = H.new_ident "child_path" in
          let by_link_or_copy =
            #<If:DBGEN_FLAGS$flag "copylink"> (Bypass.set_current_copy()) #<Else> (Bypass.set_link()) #<End>
          in
          debug_in "WRITE LINK EXPR" (
          H.make_letin'
            [ tr2, wr_expr tr;
              pathid, H.copy_expr path;
              chldpath, dbpath_add (pathid @: (typath())) key_expr ]
            (H.make_ifthenelse (H.apply_lambda' (Bypass.exists()) [tr @: (tytrans()); chldpath @: (typath())])
               ( (* The link already exists, nothing to do *)
                 debug_in "> ok, it's there"
                   (tr2 @: (tytrans())))
               ( (* Nothing there, we have to dynamically link to a fresh key to keep things correct
                    (cf write_child) *)
                 let dbpath_dest = H.new_ident "dbpath_dest"
                 and newkey = H.new_ident "newkey"
                 and newpath_dest = H.new_ident "newpath_dest"
                 in
                 debug_in "> I have to make the link" (
                 H.make_letin'
                   [ dbpath_dest,
                     dbpath_from_link_expr sch dbinfo.edge_num_expr tr2 edge (pathid @: (typath()));
                     newkey,
                     newkey_expr (tr2 @: (tytrans())) (dbpath_dest @: (typath()));
                     newpath_dest,
                     dbpath_add (dbpath_dest @: (typath())) (newkey @: (tykey ())) ]
                   (H.apply_lambda' by_link_or_copy
                      [ tr2 @: (tytrans());
                        chldpath @: (typath());
                        newpath_dest @: (typath()) ]) ) ))
          )
    in
    let rec aux (accpath,wr_expr) node path =
      (* Returns the tuple (dbpath, wr_expr: trans_id -> tr expr) *)
      match path, (V.label node).C.nlabel with
        (* Cases not consuming the key *)
        | [], C.Hidden ->
            aux
              (dbpath_add accpath (key_const_int 0),
              (fun tr -> H.apply_lambda' (Bypass.clear()) [wr_expr tr; (H.copy_expr accpath)]))
             (SchemaGraph.unique_next sch node)
              path
        | path, C.Hidden ->
            aux (dbpath_add accpath (key_const_int 0), wr_expr) (SchemaGraph.unique_next sch node) path
        | [], _ ->
            accpath, wr_expr, node
        | k::_, C.Sum ->
            let e = SchemaGraphLib.find_field_edge sch node (match k with Db.FldKey f -> f | _ -> assert false) in
            let e_num_expr() = dbinfo.edge_num_expr e in
            let wr_expr =
              let tr2 = H.new_ident "tr" and tr3 = H.new_ident "tr" in
              let pathid = H.new_ident "path2" in
              let current_edge_id = H.new_ident "current_edge" in
              fun tr ->
              H.make_letin' [tr2, wr_expr tr; pathid, H.copy_expr accpath]
                (H.make_match
                   (get_code tr2 (pathid @: (typath())) C.Leaf_int)
                   [ H.patt_some_var current_edge_id H.tyint,
                       H.make_ifthenelse (expr_equals (current_edge_id @: H.tyint) (e_num_expr()))
                         (tr2 @: (tytrans()))
                         (H.make_letin
                            (tr3,
                             H.apply_lambda' (Bypass.remove_children())
                               [ tr2 @: (tytrans());
                                 dbpath_add (pathid @: (typath())) (key_int (current_edge_id @: H.tyint)) ])
                            (set_code  tr3 (pathid @: (typath())) (e_num_expr()) C.Leaf_int));
                     H.patt_none H.tyint,
                       (set_code  tr2 (pathid @: (typath())) (e_num_expr()) C.Leaf_int) ])
            in
            let accpath = dbpath_add accpath (key_int (e_num_expr())) in
            aux (accpath, wr_expr) (E.dst e) path
        (* Cases consuming the key *)
        | (Db.ExprKey (Q.Coerce (_, e, Q.TypeConst ty)))::subpath, C.Multi ->
            let nextnode = SchemaGraph.unique_next sch node in
            let k () = match ty with Q.TyInt -> key_int e | Q.TyString -> key_string e | _ -> assert false in
            let wr_expr = write_link_if_necessary wr_expr accpath (SchemaGraph.out_edge sch node) (k()) in
            aux (dbpath_add accpath (k()), wr_expr) nextnode subpath
        | (Db.ExprKey (Q.Coerce (_, e, Q.TypeRecord (Q.TyRow(l_fields,_)))))::subpath, C.Multi ->
            let nextnode = SchemaGraph.unique_next sch node in
            let k () = key_list gamma l_fields e in
            let wr_expr = write_link_if_necessary wr_expr accpath (SchemaGraph.out_edge sch node) (k()) in
            aux (dbpath_add accpath (k()), wr_expr) nextnode subpath
        | (Db.ExprKey (Q.Coerce _))::_, C.Multi -> error "this type of key is not handled in paths yet"
        | (Db.ExprKey _)::_, C.Multi -> error "Uncoerced key found in path. Did you preprocess ?"
        | Db.NewKey::subpath, C.Multi ->
            assert (SchemaGraphLib.multi_key sch node = C.Kint);
            let nextnode = SchemaGraph.unique_next sch node in
            let pathid = H.new_ident "path" in
            let accpath =
              H.make_letin (pathid, accpath)
                (dbpath_add (pathid @: (typath())) (magic_newkey_expr dbinfo (pathid @: (typath())))) in
            aux (accpath, wr_expr) nextnode subpath
        | (Db.FldKey s)::subpath, C.Product ->
            let edge = SchemaGraphLib.find_field_edge sch node s in
            let k () = key_int (dbinfo.edge_num_expr edge) in
            let wr_expr = write_link_if_necessary wr_expr accpath edge (k()) in
            aux
              (dbpath_add accpath (k()), wr_expr)
              (E.dst edge) subpath
        | (Db.ExprKey _)::_, C.Product -> error "Unhandled sugar in path"
        | _, _ -> assert false (* inconsistency in path wrt to schema *)
    in
    let res, writer, _node =
      aux (dbpath, (fun tr -> tr @: C.Db.t ())) node0 path0 in
    (* assert(node = fst (find_exprpath sch ~node:node0 path0)); *)
    res, writer

  let get_path_expr_aux ?(transget=(fun x -> x)) node dbinfo tr dbpath kind =
    match kind with
      | Db.Option ->
          let pathid = H.new_ident "path" in
          let access_code = dbinfo.access_fun node tr (pathid @: (typath())) in
          let access_code = transget access_code in
          let ty = H.type_from_annot access_code in
          H.make_letin (pathid, dbpath)
            (H.make_ifthenelse
               (H.apply_lambda' (Bypass.exists()) [tr @: (tytrans()); pathid @: (typath())])
               (make_some access_code)
               (Helpers_gen.expr_none ty))
      | Db.Default ->
          transget (dbinfo.access_fun node tr dbpath)
      | Db.Valpath ->
          let pathid = H.new_ident "path" in
          let access_code = dbinfo.access_fun node tr(pathid @: (typath())) in
          let access_code = transget access_code in
          let access_fun =
            let tr = H.new_ident "trans" in
            H.make_lambda (tr, (tytrans())) access_code
          in
          let ty = H.type_from_annot access_code in
          let fun_id = H.new_ident "access_fun" in
          let valpath = H.make_letin'
            [ pathid, dbpath;
              fun_id, access_fun ]
            (H.apply_lambda' (Bypass.get_val_path ty)
               [tr @: (tytrans()); pathid @: (typath()); fun_id @: H.type_from_annot access_fun])
          in H.apply_lambda' (Helpers_gen.expr_val_to_val ty) [valpath]
      | Db.Ref -> assert false
      | Db.Update _ -> assert false


  let get_path_expr ?transget sch dbinfo gamma node path kind =
    let tr = H.new_ident "tr" in
    let dbpath, _write_expr = pathcode_of_qmlpath sch dbinfo gamma path in
    let get_code = get_path_expr_aux ?transget node dbinfo tr dbpath kind in
    debug_in
      (Format.sprintf "get path at %s (%a)" (SchemaGraphLib.string_path_of_node sch node)
         QmlPrint.pp#ty (H.type_from_annot get_code))(
    H.make_letin
      (tr, enter_transaction_expr  dbinfo.db_ident)
      get_code
    )

  let make_ref_path
      ?transget
      ?(transset=(fun _p _t x -> x))
      sch dbinfo gamma node path =
    let ty = SchemaGraphLib.type_of_node node in
    let tr, pathid, x = H.new_ident "tr", H.new_ident "path", H.new_ident "x" in
    let dbpath, wr_expr = pathcode_of_qmlpath sch dbinfo gamma path in
    let to_val_path_expr =
      let tr = H.new_ident "tr" in
      H.make_lambda (tr, (tytrans()))
        (get_path_expr_aux ?transget node dbinfo tr (pathid @: (typath())) Db.Default)
    in
    let writer = dbinfo.writer node (wr_expr tr) (pathid @: (typath())) (x @: ty) in
    let writer = transset pathid (x,ty) writer in
    let ref_path =
    H.make_letin (pathid, dbpath)
      (H.apply_lambda' (Bypass.get_ref_path ty)
         [ dbinfo.db_ident @: C.Db.t ();
           pathid @: (typath());
           to_val_path_expr;
           H.make_lambda' [tr, (tytrans()); x, ty] writer])
    in
    let ref_path = H.apply_lambda' (Helpers_gen.expr_ref_to_ref ty) [ref_path] in
    debug_in
      (Format.sprintf "Build ref path at %s (%a)" (SchemaGraphLib.string_path_of_node sch node)
         QmlPrint.pp#ty (match H.type_from_annot ref_path with Q.TypeName (ty::_, _) -> ty | _ -> assert false))
      ref_path

  let make_simple_virtual_val_path val_path =
    let read_ty = C.get_val_path_ty (H.type_from_annot val_path) in
    let identity =
      let x = H.new_ident "x" in
      H.make_lambda' [x, read_ty] (x @: read_ty) in
    let make_virt_path = Helpers_gen.expr_make_virtual_val read_ty read_ty in
    H.apply_lambda' make_virt_path [val_path; identity]

  let make_virtualset_fullpath sch dbinfo gamma node path kind wty =
    match kind with
    | Db.Ref ->
        (* Create a virtual path from a ref path, read function is
           identity, write function is restricted with already binded
           keys. *)
        (* 1 - Extract record from path and replace it by an ident *)
        let record_key = H.new_ident "record_key" in
        let path, record =
          let rec aux acc = function
            | [Db.ExprKey record] ->
                let ty = H.type_from_annot record in
                let rk = record_key @: ty in
                let rk = H.make_coerce ~ty rk in
                let rpath = (Db.ExprKey rk)::acc in
                (List.rev rpath), record
            | t::q -> aux (t::acc) q
            | _ -> assert false
          in aux [] path in
        (* 2 - Create the virtual path from a ref path *)
        let ref_path = make_ref_path sch dbinfo gamma node path in
        let data_ty = C.get_val_path_ty (H.type_from_annot ref_path) in
        let read =
          let x = H.new_ident "x" in
          H.make_lambda' [x, data_ty] (x @: data_ty) in
        let write =
          let x = H.new_ident "x" in
          H.make_lambda' [x, wty]
            (let record_key_ty = H.type_from_annot record in
             let fields =
               let rec aux get ty acc =
                 let tyfields = match ty with
                 | Q.TypeRecord (Q.TyRow (fields, _)) -> fields
                 | _ -> assert false in
                 let rec aux2 acc = function
                   | (fld, ty)::q ->
                       aux2 ((fld, H.make_dot ~ty (get ()) fld)::acc) q
                   | [] -> acc in
                 aux2 acc tyfields in
               (* Fields from record_key *)
               let fields =
                 aux (fun () -> record_key @: record_key_ty) record_key_ty [] in
               (* Fields from write param *)
               aux (fun () -> x @: wty) wty fields
             in H.make_record fields
            )
        in
        let make_virt_path = Helpers_gen.expr_make_virtual_ref data_ty data_ty wty in
        let virt_path = H.apply_lambda' make_virt_path [ref_path; read; write] in
        H.make_letin (record_key, record) virt_path
    | Db.Valpath ->
        (* Create a simple virtual path from a val path *)
        let val_path = get_path_expr sch dbinfo gamma node path kind in
        make_simple_virtual_val_path val_path

    | Db.Option | Db.Default ->
        get_path_expr sch dbinfo gamma node path kind
    | _ -> assert false (* TODO - ... *)

  let create_partial_key sch node fields record =
    match SchemaGraphLib.multi_key sch node with
    | C.Kfields [fldlist] ->
        let node_of_elts = List.hd (SchemaGraph0.succ sch node) in
        let fields =
          List.sort
            (fun x y -> String.compare (fst x) (fst y))
            fields in
        let fldlist = List.sort String.compare fldlist in
        let r, e =
          List.fold_right
            (fun fld (fields, e) ->
               match fields with
               | [] -> fields, H.apply_lambda' (Bypass.add_hole ()) [e]
               | (f,_)::rfields when f = fld ->
                   let ty = (E.dst (SchemaGraphLib.find_field_edge sch node_of_elts fld)).C.ty in
                   let make_key = match ty with
                   | Q.TypeConst (Q.TyInt) -> Bypass.key_int ()
                   | Q.TypeConst (Q.TyString) -> Bypass.key_string ()
                   | _ -> assert false in
                   let key = H.apply_lambda make_key (H.make_dot ~ty record fld) in
                   let add_key = H.apply_lambda' (Bypass.add_key ()) [key; e] in
                   (rfields, add_key)
               | _ -> fields, H.apply_lambda' (Bypass.add_hole ()) [e]
            ) fldlist (fields, (Bypass.empty_partial_key ()))
        in assert (r = []); e
  | C.Kint | C.Kstring | C.Kfields _ -> Schema_private.internal_error "type_of_partial_key"

  let make_virtualset_partialpath sch dbinfo gamma node path kind wty record =
    let rident = H.new_ident "record_key" in
    H.make_letin (rident, record) (
    (* Transform access to dbset to dbset with partial keys *)
    let transget dbset =
      let fields =
        match record with
        | Q.Coerce (_, Q.Record (_, fields), _) -> fields
        | _ -> assert false in
      let partial_key = create_partial_key sch node fields
        (rident @: (H.type_from_annot record)) in
      let set_dbset_keys = Bypass.set_dbset_keys (H.type_from_annot dbset) in
      let dbset = H.apply_lambda' set_dbset_keys [dbset; partial_key] in
      dbset
    in
    match kind with
    | Db.Option | Db.Default ->
        let path = List.remove_last path in
        get_path_expr ~transget sch dbinfo gamma node path kind
    | Db.Valpath ->
        let path = List.remove_last path in
        let val_path = get_path_expr ~transget sch dbinfo gamma node path kind in
        make_simple_virtual_val_path val_path
    | Db.Ref ->
        (* The writing function on real path take build key from
           full record, complete the path (path/key) and write the
           full record. *)
        let transset pathid (data,ty) writer =
          let fields = match SchemaGraphLib.multi_key sch node with
          | C.Kfields [fldlist] ->
              let node_of_elts = List.hd (SchemaGraph0.succ sch node) in
              List.fold_right
                (fun f acc ->
                   (f, (E.dst (SchemaGraphLib.find_field_edge sch node_of_elts f)).C.ty)::acc)
                fldlist []
          | _ -> assert false in
          let kl = key_list gamma fields (data @: ty) in
          let path =
            H.apply_lambda' (Bypass.dbpath_add ())
              [(pathid @: (typath())); kl] in
          H.make_letin (pathid, path) writer
        in
        let ref_path =
          let path = List.remove_last path in
          make_ref_path ~transget ~transset sch dbinfo gamma node path in
        (* Create the virtual path from the ref path *)
        let data_ty = C.get_val_path_ty (H.type_from_annot ref_path) in
        let read =
          let x = H.new_ident "x" in
          H.make_lambda' [x, data_ty] (x @: data_ty) in
        let write =
          let x = H.new_ident "x" in
          H.make_lambda' [x, wty]
            (let record_key_ty = H.type_from_annot record in
             let fields =
               let rec aux get ty acc =
                 let tyfields = match ty with
                 | Q.TypeRecord (Q.TyRow (fields, _)) -> fields
                 | _ -> assert false in
                 let rec aux2 acc = function
                   | (fld, ty)::q ->
                       aux2 ((fld, H.make_dot ~ty (get ()) fld)::acc) q
                   | [] -> acc in
                 aux2 acc tyfields in
               (* Fields from record_key *)
               let fields =
                 aux (fun () -> rident @: record_key_ty) record_key_ty [] in
               (* Fields from write param *)
               aux (fun () -> x @: wty) wty fields
             in H.make_record fields
            )
        in
        let make_virt_path = Helpers_gen.expr_make_virtual_ref data_ty data_ty wty in
        let virt_path = H.apply_lambda' make_virt_path [ref_path; read; write] in
        virt_path
    | _ -> assert false (* TODO - ... *)
    )

  let make_virtualpath sch dbinfo gamma node path kind ihandler rty wty =
    let e =
      match kind with
      | Db.Ref -> make_ref_path sch dbinfo gamma node path
      | _ -> get_path_expr sch dbinfo gamma node path kind in
    let realty = node.C.ty in
    let handler = ihandler @: H.typevirtualhandler realty rty wty in
    let read = H.make_dot handler "read" in
    match kind with
    | Db.Default ->
        (* Just apply read function *)
        H.apply_lambda' read [e]
    | Db.Option ->
        (* Map option with virtual read function *)
        match_option_expr ~tyval:realty e
          (fun data ->
             H.apply_lambda' (Helpers_gen.expr_some rty) [H.apply_lambda' read [data]])
          (Helpers_gen.expr_none rty)
    | Db.Valpath ->
        (* Create virtual val path from val path *)
        let make_virt_path = Helpers_gen.expr_make_virtual_val realty rty in
        H.apply_lambda' make_virt_path [e; read]
    | Db.Ref ->
        (* Create virtual ref path from ref path *)
        let write = H.make_dot handler "write" in
        let make_virt_path = Helpers_gen.expr_make_virtual_ref realty rty wty in
        H.apply_lambda' make_virt_path [e; read; write]
    | _ -> assert false (* TODO - ... *)

  (* let make_update_value value update = *)
  (*   match update with *)
  (*   | UFlds flds  ->  *)
  (*   | UExpr e     -> *)
  (*   | UIncr i     -> H.const_int i *)
  (*   | UAppend     -> *)
  (*   | UAppendAll  -> *)
  (*   | UPrepend    -> *)
  (*   | UPrependAll ->  *)
  (*   | UPop        -> *)
  (*   | UShift      -> *)

  let make_update_path ~context sch dbinfo gamma node path update =
    let error () =
      QmlError.error context "This update operation is not yet handled by db3 generator\n"
    in
    let rec aux update =
      match update with
      | Db.UExpr expr ->
          let rpath = make_ref_path sch dbinfo gamma node path in
          let dbwrite = Helpers_gen.expr_write node.C.ty in
          H.apply_lambda' dbwrite [rpath; expr]
      | Db.UFlds flds ->
          let rec build_record flds ty =
            let flds =
              List.map
                (function
                   | [f], Db.UExpr expr -> f, expr
                   | [f], Db.UFlds flds ->
                       f, build_record flds (Schema_private.dots gamma [f] ty)
                   | _ -> error ()
                )
                flds
            in
            let flds = List.sort (fun (f1,_) (f2,_) -> String.compare f1 f2) flds in
            let _check =
              let aux_check tyflds =
                List.iter2 (fun (f1,_) (f2,_) -> if String.compare f1 f2 <> 0 then error ())
                  tyflds flds
              in
              match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
              | Q.TypeRecord ty_row ->
                  begin match QmlAstCons.Type.Row.sort ty_row with
                  | Q.TyRow (tyflds, _) -> aux_check tyflds
                  end
              | Q.TypeSum ty_sum ->
                  begin match QmlAstCons.Type.Col.sort ty_sum with
                  | Q.TyCol (cols, _) ->
                      let ismatch col =
                        (List.make_compare (fun x y -> String.compare (fst x) (fst y))
                           col flds)
                        = 0
                      in
                      let col =
                        try
                          List.find ismatch cols
                        with Not_found -> error ()
                      in
                      aux_check col
                  end
              | _ -> error ()
            in H.make_record flds
          in aux (Db.UExpr (build_record flds node.C.ty))
      | _ -> error ()
    in aux update

  let get_expr ~context t dbinfo_map gamma path kind =
    let _ =
      let pos = QmlError.Context.get_pos context in
      H.start_built_pos pos in
    let prefix, db_def, path = Schema_private.database_def_of_path_expr ~context t path in
    let dbinfo = StringListMap.find prefix dbinfo_map in
    let _, node, virtual_ = Schema_private.find_exprpath db_def.Schema_private.schema db_def.Schema_private.virtual_path ~kind path in
    let r = match virtual_ with
    | `virtualset (_, wty, false, _) ->
        make_virtualset_fullpath db_def.Schema_private.schema dbinfo gamma node path kind wty
    | `virtualset (_, wty, true, record) ->
        let record = match record with
        | Some record -> record
        | None ->
            QmlError.error context
              "This kind of dbset access is not yet implemented by Db3"
        in
        make_virtualset_partialpath db_def.Schema_private.schema dbinfo gamma node path kind wty record
    | `virtualpath (ident, rty, wty) ->
        make_virtualpath db_def.Schema_private.schema dbinfo gamma node path kind ident rty wty
    | `realpath ->
        match kind with
        | Db.Ref ->
            make_ref_path db_def.Schema_private.schema dbinfo gamma node path
        | Db.Update update ->
            make_update_path ~context db_def.Schema_private.schema dbinfo gamma node path update
        | _ ->
            get_path_expr db_def.Schema_private.schema dbinfo gamma node path kind
    in H.end_built_pos (); r
end

module DatabaseAccess ( Arg : DbGenByPass.S ) = struct
  (* /!\ The consistency of the access functions here, and typing of paths on the
     yet unfinished tree in module Schema_private must be guaranteed. Double-check it. *)

  module CodeGenerator = CodeGenerator ( Arg )
  module Helpers_gen = CodeGenerator.Helpers_gen

  let rec replace_path_exprs_aux ?(refresh_db_id=fun x -> x) t dbinfo_map gamma e =
    let context = QmlError.Context.expr e in
    let context = Schema_private.HacksForPositions.map context in
    let f tra = function
      | Q.Coerce (_, Q.Path (_, p, kind), _)
      | Q.Path (_, p, kind) ->
          let e = CodeGenerator.get_expr ~context t dbinfo_map gamma p kind in
          (* needs to be traversed again because db idents may be introduced *)
          tra e
      | Q.Ident (label, id) ->
          (* database idents are refreshed to be uniq in each package *)
          Q.Ident (label, (refresh_db_id id))
      | e -> tra e
    in
    QmlAstWalk.Expr.traverse_map f e

  let replace_path_exprs =
    fun t dbinfo_map gamma ?(annotmap=None) ?(valinitial_env=Arg.ValInitial.empty) e ->
      let _ = AnnotTable.open_table ~annotmap () in
      let _ = Helpers_gen.valinitial_env_ref := valinitial_env in
      try
        let ans = replace_path_exprs_aux t dbinfo_map gamma e in
        Helpers_gen.valinitial_env_ref := Arg.ValInitial.empty;
        AnnotTable.close_table(), ans
      with e ->
        Helpers_gen.valinitial_env_ref := Arg.ValInitial.empty; AnnotTable.emergency_close(); raise e

  let replace_path_ast t dbinfo_map gamma ?(annotmap=None) ?(valinitial_env=Arg.ValInitial.empty) code =
    let _ = AnnotTable.open_table ~annotmap () in
    let _ = Helpers_gen.valinitial_env_ref := valinitial_env in
    (* ugly hack alpha converting db idents ... will be removed *)
    let refresh_db_id =
      let fold_fun _point db_def acc =
        let id = db_def.Schema_private.ident in (id, Ident.refresh id)::acc
      in
      let db_idents = StringListMap.fold fold_fun t [] in
      fun id -> try List.assoc id db_idents with Not_found -> id
    in
    let replace_bnd l = List.map (fun (id,e) -> refresh_db_id id, replace_path_exprs_aux ~refresh_db_id t dbinfo_map gamma e) l in
    try
      let ans =
        Q.map_code
          (function
             | Q.NewVal (label, blist) -> Q.NewVal (label, (replace_bnd blist))
             | Q.NewValRec (label, blist) -> Q.NewValRec (label, (replace_bnd blist))
             | other -> other)
          code
      in
      let gamma =
        QmlTypes.Env.Ident.fold
          (fun ident scheme gamma ->
             let ident' = refresh_db_id ident in
             if ident == ident' then gamma (* just to avoiding the whole gamma *)
             else QmlTypes.Env.Ident.remove ident (QmlTypes.Env.Ident.add ident' scheme gamma)
          ) gamma gamma in
      Helpers_gen.valinitial_env_ref := Arg.ValInitial.empty;
      AnnotTable.close_table(), ans, gamma
    with e ->
      Helpers_gen.valinitial_env_ref := Arg.ValInitial.empty; AnnotTable.emergency_close(); raise e

  let replace_path_code_elt t dbinfo_map gamma ?(annotmap=None) ?(valinitial_env=Arg.ValInitial.empty) code_elt =
    let flag, ans, gamma = replace_path_ast t dbinfo_map gamma ~annotmap ~valinitial_env [code_elt] in
    match ans with
    | [code_elt] -> flag, code_elt, gamma
    | _ -> assert false


  let initialize = CodeGenerator.initialize
end
