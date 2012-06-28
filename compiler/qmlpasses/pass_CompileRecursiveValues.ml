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
module Q = QmlAst
module List = BaseList
module Format = BaseFormat

module IdentAssoc = List.MakeAssoc(Ident)

exception InvalidRecursion

module Warning = struct

  let recval =
    let doc = "Recursive values" in
    WarningClass.create ~name:"recval" ~doc ~err:true ~enable:true ()

  let recval_lambda =
    let doc = "Recursive value as a lambda - deprecated in js-like syntax (S4)" in
    WarningClass.create ~parent:recval ~name:"lambda" ~doc ~err:true ~enable:true ()

  let set = WarningClass.Set.create_from_list [
    recval;
    recval_lambda;
  ]
end

let warning_set = Warning.set

let map_intersection merge_value map1 map2 =
  IdentMap.fold
    (fun k v1 acc ->
       try let v2 = IdentMap.find k map2 in
           IdentMap.add k (merge_value v1 v2) acc
       with Not_found ->
         acc
    ) map1 IdentMap.empty

(* this function takes a binding (from a recursive set of bindings)
 * and distinguishes 3 cases:
 * - the expression is tagged with @recval (coming from rec val or and val in the syntax)
 *   returns Some of a map from the direct dependencies of this expression on the other
 *   identifiers of the bindings of their positions
 * - the expression is a lambda -> return None
 * - in other cases, the recursion is invalid, and the InvalidRecursion is raised
 *)
let is_a_val_binding idents (_i, e) =
  let merge_value = (@) in
  let find_deps e =
    QmlAstWalk.Expr.self_traverse_fold
      (fun self tra deps e ->
         match e with
         | Q.Ident (label, i) when IdentSet.mem i idents -> IdentMap.add i [Annot.pos label] deps
         | Q.Match (_, e, pel) -> (
             let deps = self deps e in
             let depss = List.map (fun (_p,e) -> self IdentMap.empty e) pel in
             match depss with
             | [] -> assert false
             | h :: t ->
                 (* we can sure that we depend on an identifier only if all the
                  * branches depend on that identifier
                  * hence we must take the intersection of the dependencies of the branches
                  * and NOT their union *)
                 let intersection = List.fold_left (map_intersection merge_value) h t in
                 IdentMap.merge merge_value intersection deps
           )
         | Q.Lambda _ ->
             deps
         | _ ->
             tra deps e
      ) IdentMap.empty e in
  let rec is_a_val = function
    | Q.Lambda _ -> None
    | Q.Directive (_, `recval, [e], _) ->
        (* TODO *)
        (* checking that you don't put a val rec on a function *)
        (try match is_a_val e with
         | None ->
             let context = QmlError.Context.expr e in
             QmlError.warning ~wclass:Warning.recval_lambda context
               "This expression is a function, it can be recursive without being tagged with 'val'.";
             Some (find_deps e)
         | Some _ -> Some (find_deps e)
         with InvalidRecursion -> Some (find_deps e));
    | Q.Directive (_, `recval, _, _) -> assert false
    | Q.Coerce (_, e, _)
    (* BEWARE before editing: keep this set of directive in sync with the one
     * in remove_toplevel_directives *)
    | Q.Directive (_, (#Q.type_directive | #Q.binding_directive), [e], _) -> is_a_val e
    | _ -> raise InvalidRecursion in
  is_a_val e

let lazy_type gamma var =
  let typeident = Q.TypeIdent.of_string Opacapi.Types.finite_single_thread_lazy in
  let (typeident, _) = QmlTypes.Env.TypeIdent.findi ~visibility_applies:true typeident gamma in
  (* grabbing the typeident from the gamma, or else we might have the infamous
   * assert failure somewhere in the typer saying "call type_of_type" *)
  Q.TypeName ([var], typeident)
let lazy_force_type gamma var =
  Q.TypeArrow ([lazy_type gamma var], var)
let mutable_make_type gamma ty =
  let var = QmlAstCons.Type.next_var () in
  Q.TypeArrow ([var], lazy_type gamma ty)
let mutable_set_type gamma ty =
  Q.TypeArrow ([lazy_type gamma ty; ty], Q.TypeRecord (Q.TyRow ([], None)))

let force ~val_ gamma annotmap label lazy_i =
  let lazy_force = val_ Opacapi.FiniteSingleThreadLazy.force in
  let ty = QmlAnnotMap.find_ty_label label annotmap in
  let annotmap, force = QmlAstCons.TypedExpr.ident annotmap lazy_force (lazy_force_type gamma ty) in
  let annotmap, lazy_i_expr = QmlAstCons.TypedExpr.ident annotmap lazy_i (lazy_type gamma ty) in
  let annotmap, forced_lazy = QmlAstCons.TypedExpr.apply gamma annotmap force [lazy_i_expr] in
  annotmap, forced_lazy

let partition_map p l =
  let rec aux acc1 acc2 = function
    | [] -> List.rev acc1, List.rev acc2
    | h :: t ->
        match p h with
        | None -> aux acc1 (h :: acc2) t
        | Some v -> aux ((h, v) :: acc1) acc2 t in
  aux [] [] l

let rec drop_until p = function
  | [] -> None, []
  | h :: t ->
      if p h then
        Some h, t
      else
        drop_until p t

(* simple check to reject at compile time some cases of illegal value recursion
 * such as [val rec x = x] *)
let check_lack_of_cycle val_deps_bindings =
  let val_deps = List.map (fun ((i,_e),deps) -> (i,deps)) val_deps_bindings in
  let pos_of_def i =
    let (_, e), _ = List.find (fun ((j,_),_) -> Ident.equal i j) val_deps_bindings in
    Q.Pos.expr e in
  let rec aux occur i posl =
    if IdentAssoc.mem i occur then (
      (* the dependencies that cause the immediate loop *)
      let calls = List.rev ((i,posl) :: occur) in
      (* the relevant part of the dependencies *)
      let _, calls = drop_until (fun (j,_) -> Ident.equal i j) calls in
      OManager.serror "@[<v>%a@]@\n@[<v2>Invalid recursive value binding: @{<bright>%s@} depends on itself@]@\n@[<v2>Hint:@ Here is the chain of immediate dependencies:@ %a@]"
        FilePos.pp (pos_of_def i)
        (Ident.original_name i)
        (Format.pp_list "@\n"
           (fun f (i,posl) ->
              (* we have several positions when there are branching,
               * but perhaps it gives too much information to show all
               * the positions *)
              let pos = List.hd posl in
              Format.fprintf f "@[<v2>%s at %a@]" (Ident.original_name i) FilePos.pp pos
           )) calls;
      (* exiting to give only one error message in the recursive group *)
      raise InvalidRecursion
    ) else
      let occur = (i,posl) :: occur in
      (* only non lambda bindings are in val_deps, so we can get a Not_found here *)
      let deps = try IdentAssoc.find i val_deps with Not_found -> IdentMap.empty in
      IdentMap.iter (aux occur) deps in
  try
    List.iter
      (fun (i,_) -> aux [] i [])
      val_deps
  with InvalidRecursion -> ()

let move_ei_tsc_gen label annotmap e =
  let tsc_gen_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
  assert (QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap = None);
  QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr e) tsc_gen_opt annotmap

(* now the typing directive are just freaking annoying, because there may be
 * more slicer directives under them so let's remove them *)
let remove_toplevel_directives annotmap e =
  let rec aux dirs annotmap = function
    | Q.Coerce (label, e, _)
    | Q.Directive (label, #Q.type_directive, [e], _) ->
        let annotmap = move_ei_tsc_gen label annotmap e in
        aux dirs annotmap e
    | Q.Directive (label, (#Q.binding_directive as v), [e], []) ->
        let annotmap = move_ei_tsc_gen label annotmap e in
        aux (v :: dirs) annotmap e
    | Q.Directive (_, #Q.slicer_directive, _, _) -> assert false
    | e -> annotmap, dirs, e in
  aux [] annotmap e

let rec put_back_toplevel_directives annotmap dirs e =
  match dirs with
  | [] -> annotmap, e
  | dir :: dirs ->
      let label = Q.Label.expr e in
      let new_label = Annot.refresh label in
      let ty = QmlAnnotMap.find_ty_label label annotmap in
      let tsc_gen_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
      let annotmap = QmlAnnotMap.remove_tsc_label label annotmap in
      let annotmap = QmlAnnotMap.add_ty_label new_label ty annotmap in
      let annotmap = QmlAnnotMap.add_tsc_opt_label new_label tsc_gen_opt annotmap in
      let e = Q.Directive (new_label, dir, [e], []) in
      put_back_toplevel_directives annotmap dirs e

(*
 * rewrites [rec val x = e1
 *           and f() = e2]
 * into
 * [lazy_x = mutable_make(0)
 *  rec f() = e2[lazy_force(lazy_x) / x ]
 *  _ = mutable_set(lazy_x, ( -> e1[lazy_force(lazy_x) / x ]))
 *  x = lazy_force(lazy_x)
 * ]
 * The only expressions in the recursive bindings after this rewriting are lambdas
 * (modulo coercions, some directives, etc.)
 *)
let process_bindings ~val_ gamma annotmap bindings =
  let idents = List.fold_left (fun acc (i,_) -> IdentSet.add i acc) IdentSet.empty bindings in

  let invalid_bindings = ref [] in
  let val_deps_bindings, fun_bindings =
    partition_map
      (fun b ->
         try is_a_val_binding idents b
         with InvalidRecursion ->
           invalid_bindings := b :: !invalid_bindings;
           None
      ) bindings in
  if !invalid_bindings <> [] then (
    (match bindings with
     | [(i,e)] ->
         (* a more concise error message in the common case of not mutual recursion *)
         let context = QmlError.Context.expr e in
         QmlError.serror context "@[<v2>  The recursive definition of @{<bright>%s@} is invalid." (Ident.original_name i)
     | _ ->
         OManager.serror "@[<v2>In the recursive group consisting of {@[<h>%a@]}, the following recursive definitions are invalid:@\n%a@]@\n@]"
          (Format.pp_list ",@ " (fun f i -> Format.pp_print_string f (Ident.original_name i))) (IdentSet.elements idents)
           (Format.pp_list "@ " (fun f (i,e) -> Format.fprintf f "@{<bright>%s@} at %a" (Ident.original_name i) FilePos.pp (Q.Pos.expr e))) !invalid_bindings
    );
    None
  ) else (
    check_lack_of_cycle val_deps_bindings;
    let val_bindings = List.map fst val_deps_bindings in
    if val_bindings = [] then
      None
    else (
      let mutable_make = val_ Opacapi.Mutable.make in
      let mutable_set = val_ Opacapi.Mutable.set in

      (* when we write @server rec val x = ..., then we remove the directive
       * @server from the body of x and we will put it on all the toplevel
       * bindings generated from x *)
      let annotmap, val_bindings =
        List.fold_left_map
          (fun annotmap (i,e) ->
             let annotmap, dirs, e = remove_toplevel_directives annotmap e in
             annotmap, (i, e, dirs)
          ) annotmap val_bindings in
      let lazy_idents = List.map (fun (i,_,_) -> Ident.refreshf ~map:"lazy_%s" i) val_bindings in
      let annotmap, lazy_defs =
        List.fold_left_map2
          (fun annotmap i (_,e,dirs) ->
             let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
             let annotmap, mutable_make = QmlAstCons.TypedExpr.ident annotmap mutable_make (mutable_make_type gamma ty) in
             (* could put a well typed value if needed (like {evaluating}) *)
             let annotmap, zero = QmlAstCons.TypedExpr.int annotmap 7 in
             let annotmap, def = QmlAstCons.TypedExpr.apply gamma annotmap mutable_make [zero] in
             let annotmap, def = put_back_toplevel_directives annotmap dirs def in
             annotmap, (i, def)
          ) annotmap lazy_idents val_bindings in
      let annotmap, lazy_sets =
        List.fold_left_map2
          (fun annotmap i (_, e, dirs) ->
             let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
             let annotmap, mutable_set = QmlAstCons.TypedExpr.ident annotmap mutable_set (mutable_set_type gamma ty) in
             let annotmap, lambda = QmlAstCons.TypedExpr.lambda annotmap [] e in
             let annotmap, lazy_body = QmlAstCons.TypedExpr.sum_element annotmap ["delayed", lambda] in
             let annotmap, ref_ = QmlAstCons.TypedExpr.ident annotmap i (lazy_type gamma ty) in
             let annotmap, set = QmlAstCons.TypedExpr.apply gamma annotmap mutable_set [ref_; lazy_body] in
             let annotmap, set = put_back_toplevel_directives annotmap dirs set in
             annotmap, (Ident.next "set_lazy", set)
          ) annotmap lazy_idents val_bindings in
      let annotmap, original_bindings =
        List.fold_left_map2
          (fun annotmap lazy_i (i, e, dirs) ->
             let annotmap, forced_lazy = force ~val_ gamma annotmap (Q.Label.expr e) lazy_i in
             let annotmap, forced_lazy = put_back_toplevel_directives annotmap dirs forced_lazy in
             annotmap, (i, forced_lazy)
          ) annotmap lazy_idents val_bindings in
      let assoc_ident = List.map2 (fun lazy_i (i,_,_) -> (i,lazy_i)) lazy_idents val_bindings in
      let rewrite_binding annotmap (i,e) =
        let annotmap, e = QmlAstWalk.Expr.traverse_foldmap
          (fun tra annotmap e ->
             match e with
             | Q.Ident (label, i) -> (
                 try
                   let lazy_i = IdentAssoc.find i assoc_ident in
                   force ~val_ gamma annotmap label lazy_i
                 with Not_found ->
                   annotmap, e
               )
             | _ -> tra annotmap e
          ) annotmap e in
        annotmap, (i, e) in
      let rewrite_bindings annotmap l =
        List.fold_left_map rewrite_binding annotmap l in
      let annotmap, lazy_sets = rewrite_bindings annotmap lazy_sets in
      let annotmap, fun_bindings = rewrite_bindings annotmap fun_bindings in
      Some (
        annotmap,
        lazy_defs,
        (if fun_bindings = [] then None else Some fun_bindings),
        lazy_sets,
        original_bindings
      )
    )
  )

let process_bindings_for_toplevel ~val_ gamma annotmap label bindings =
  match process_bindings ~val_ gamma annotmap bindings with
  | None ->
      None
  | Some (annotmap, lazy_defs, fun_bindings_opt, lazy_sets, original_bindings) ->
      let code =
        Q.NewVal (Annot.refresh label, lazy_sets) ::
          Q.NewVal (Annot.refresh label, original_bindings) ::
          [] in
      let code =
        match fun_bindings_opt with
        | None -> code
        | Some fun_bindings -> Q.NewValRec (Annot.refresh label, fun_bindings) :: code in
      let code = Q.NewVal (Annot.refresh label, lazy_defs) :: code in
      let add_to_gamma gamma bindings =
        List.fold_left
          (fun gamma (i,e) ->
             let tsc = QmlTypes.Scheme.quantify (QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap) in
             QmlTypes.Env.Ident.add i tsc gamma
          ) gamma bindings in
      let gamma = add_to_gamma gamma lazy_defs in
      let gamma = add_to_gamma gamma lazy_sets in
      Some (gamma, annotmap, code)

let process_code ~val_ gamma annotmap code =

  (* rewriting newvalrec *)
  let (gamma, annotmap), code =
    List.fold_left_collect
      (fun (gamma, annotmap) c ->
         match c with
         | Q.NewValRec (label, bindings) -> (
             match process_bindings_for_toplevel ~val_ gamma annotmap label bindings with
             | None -> (gamma, annotmap), [c]
             | Some (gamma, annotmap, code) -> (gamma, annotmap), code
           )
         | _ -> (gamma, annotmap), [c]
      ) (gamma, annotmap) code in

  (* rewriting letrec and removing @recval *)
  let annotmap, code =
    QmlAstWalk.CodeExpr.fold_map
      (QmlAstWalk.Expr.self_traverse_foldmap
         (fun self tra annotmap e ->
            match e with

            | Q.LetRecIn (label, bindings, e_in) -> (
                match process_bindings ~val_ gamma annotmap bindings with
                | None -> tra annotmap e
                | Some (annotmap, lazy_defs, fun_bindings_opt, lazy_sets, original_bindings) ->
                    let label2 = Annot.refresh label in
                    let label4 = Annot.refresh label in
                    let label5 = Annot.refresh label in
                    (* not copying the information for ei *)
                    let ty = QmlAnnotMap.find_ty_label label annotmap in
                    let annotmap = QmlAnnotMap.add_ty_label label2 ty annotmap in
                    let annotmap = QmlAnnotMap.add_ty_label label4 ty annotmap in
                    let annotmap = QmlAnnotMap.add_ty_label label5 ty annotmap in
                    let e_in =
                      Q.LetIn (label2, lazy_sets,
                               Q.LetIn (label, original_bindings, e_in)) in
                    let e_in =
                      match fun_bindings_opt with
                      | None -> e_in
                      | Some fun_bindings -> Q.LetRecIn (label4, fun_bindings, e_in) in
                    let e_in =
                      Q.LetIn (label5, lazy_defs, e_in) in
                    (* need to go down to rewrite e_in and lazy_sets
                     * (although we could just rewrite them instead of calling ourselves
                     * recursively on the term produced) *)
                    tra annotmap e_in
              )

            | Q.Directive (label, `recval, [e], []) ->
                (* it is possible that we have a recval on a let that is not recursive
                 * for instance when we say [rec val x = 1] because the dependency analysis
                 * will transform newvalrec and letrec into newval and letin if possible
                 * also other rewriting (such as the one for let pattern = expr in expr)
                 * may duplicate @recval and put them in not quite legal places, so i prefer
                 * not to give an error and ignore everything *)
               (* cannot instantiate on a recval, but it has possibly been generalized *)
               assert (QmlAnnotMap.find_tsc_inst_opt_label label annotmap = None);
               let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr e) (QmlAnnotMap.find_tsc_opt_label label annotmap) annotmap in
               self annotmap e

            | Q.Directive (_, `recval, _, _) ->
                assert false

            | _ -> tra annotmap e

         )
      ) annotmap code in

  gamma, annotmap, code
