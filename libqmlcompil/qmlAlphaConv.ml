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
(* cf mli *)

(* URGENT : THIS MODULE SHOULD BE REWRITTEN USING A WALKER ON EXPR *)
(* IT WOULD SIMPLIFY ITS MAINTENABILITY (Nary would have had no fix to do) *)

(* refactoring in progress *)

(* depends *)
module List = BaseList

(* shorthands *)
module Q = QmlAst
module QC = QmlAstCons.UntypedExpr

(* go *)

module HacksForPositions =
struct
  let annotmap = ref ( QmlAnnotMap.empty : Q.annotmap )
  let set_annotmap map = annotmap := map
  let free_annotmap () = annotmap := QmlAnnotMap.empty
  (*
    Map the context, and add the current annotmap.
    Should be used each time a context is created.
  *)
  let map context =
    let c = QmlError.Context.annotmap !annotmap in
    QmlError.Context.merge2 context c
end

type t =
    {
      map : Ident.t IdentMap.t ; (* current -> alpha *)
      rev : Ident.t IdentMap.t ; (* we keep alpha -> org *)
      (*  this map is the smallest and contain first level alpah ident actually used *)
      weak : Ident.t -> bool ;
      (* used for the check unbound (TODO(K1) document this) *)
      val_ : (string -> Ident.t) option ;
    }

let to_string t =
  let tmp1 = IdentMap.fold (
    fun org neww acc ->
      Printf.sprintf "%s%s --> %s\n" acc (Ident.to_string org) (Ident.to_string neww)
  ) t.map "" in
  let tmp2 = IdentMap.fold (
    fun org neww acc ->
      Printf.sprintf "%s[%s] = {%s}\n" acc (Ident.to_string org) (Ident.to_string neww)
  ) t.rev "" in
  Printf.sprintf "keys = %d\n%s\n-----------------------------------------------\nkeys = %d\n%s"
    (IdentMap.size t.map) tmp1 (IdentMap.size t.rev) tmp2

let create_from_maps ~map ~revmap =
  {
    map = map ;
    rev = revmap ;
    weak = (fun _ -> false) ;
    val_ = None ;
  }


let next ?(weak=fun _ -> false) () =
  {
    map = IdentMap.empty ;
    rev = IdentMap.empty ;
    weak = weak ;
    val_ = None ;
  }

let empty = next ~weak:(fun _ -> true) ()

let env_weak ~weak t = { t with weak = weak }

(* Fresh : don't allow any anonymous conv *)
let conv t s =
  let id = Ident.refresh s in
  let map, rev = match IdentMap.find_opt s t.rev with
  | None ->
      t.map,
      (* this key is not in rev so we add it *)
      IdentMap.add id s t.rev
  | Some u ->
      (* we update the relation org name -> alpha *)
      IdentMap.add u id t.map,
      (* we update new alpha -> org, and we delete the old one *)
      IdentMap.remove s (IdentMap.add id u t.rev)
  in
  { t with map = IdentMap.add s id map; rev = rev }, id

module Error =
struct
  (*
    This module is used for Checking (check_fail) and for public errors.
    We use an internal reference for knowing the current mode.
  *)
  let check_fail = ref None
  let serror context fmt =
    match !check_fail with
    | Some cond_id ->
        QmlError.scheck_fail cond_id context fmt
    | None ->
        QmlError.serror context fmt

  let unbound_ident context ident =
    serror context
      "Unbound value @{<bright>%s@}"
      (Ident.to_string ident)

  let several_bound context ident =
    serror context
      "the ident @{<bright>%s@} is bound several time"
      (Ident.to_string ident)

  let several_bound_pattern context ident =
    serror context
      "the ident @{<bright>%s@} is bound several time in this pattern"
      (Ident.to_string ident)

  (* | NotAlphaRenamed       of QmlError.context IdentMap.t *)

end

(*
  | NotAlphaRenamed map ->
      let buffer =
        IdentMap.fold
          (fun id nodes buffer ->
            FBuffer.addln buffer
              (locates expr_pat_or_newval (List.map (fun (_,b) -> b) nodes)
                 (Printf.sprintf "ident %S is defined multiple times"
                    (Ident.to_string id))))
          map (FBuffer.create 10)
      in
      FBuffer.contents buffer
*)

let pat t p =
  let rec aux check t p =
    let conv t id =
      let check id =
        if IdentSet.mem id check then
          let context =
            let c = QmlError.Context.pat p in
            let c = HacksForPositions.map c in
            c
          in
          Error.several_bound_pattern context id;
          check
        else IdentSet.add id check in
      let check = check id in check, conv t id in
    let (!!) p0 =
      let annot = Q.QAnnot.pat p in
      Q.QAnnot.New.pat p0 annot in
    match p with
    | Q.PatRecord (label, fields, rowvar) ->
        let foldmap (check, t) (field, pat_) =
          let check, (t, pat_) = aux check t pat_ in
          (check, t), (field, pat_)
        in
        let (check, t), fields = List.fold_left_map foldmap (check, t) fields in
        check, (t, !! (Q.PatRecord (label, fields, rowvar)))

    | Q.PatConst _ -> check, (t, p)
    | Q.PatVar (label, id) ->
        let check, (t, id) = conv t id in
        check, (t, !! (Q.PatVar (label, id)))
    | Q.PatAny _ -> check, (t, p)
    | Q.PatCoerce (label, pat, ty) ->
        let check, (t, pat) = aux check t pat in
        check, (t, !! (Q.PatCoerce (label, pat, ty)))
    | Q.PatAs (label, pat, id) ->
        let check, (t, id) = conv t id in
        let check, (t, pat) = aux check t pat in
        check, (t, !! (Q.PatAs (label, pat, id)))
  in
  snd (aux IdentSet.empty t p)

let several_bound_check check id expr =
  let check =
    if IdentSet.mem id check
    then (
      let context =
        let c = QmlError.Context.expr expr in
        let c = HacksForPositions.map c in
        c
      in
      Error.several_bound context id ;
      check
    )
    else
      IdentSet.add id check
  in
  check

let rec expr t e =
  let (!!) e0 =
    let annot = Q.QAnnot.expr e in
    Q.QAnnot.New.expr e0 annot in
  let c =
    match e with
    | (Q.Const _) as c -> c
    | Q.Ident (label, id) as ident -> (
        match IdentMap.find_opt id t.map with
        | None ->
            if t.weak id
            then
              ident
            else (
              let context =
                let c = QmlError.Context.expr e in
                let c = HacksForPositions.map c in
                c
              in
              Error.unbound_ident context id ;
              ident
            )
        | Some id -> Q.Ident (label, id)
      )

    | Q.LetIn (label, let_, in_) ->
        let fold_map (check, ft) (id, expr_) =
          let check = several_bound_check check id expr_ in
          let ft, id = conv ft id in
          (* beware in [expr t expr_], it is really t not ft :(thing about it) *)
          (check, ft), (id, expr t expr_)
        in
        let (_, t), let_ = List.fold_left_map fold_map (IdentSet.empty, t) let_ in
        Q.LetIn (label, let_, expr t in_)

    | Q.LetRecIn (label, let_, in_) ->
        let (_, t) = List.fold_left (fun (check, t) (id, expr_) ->
          let check = several_bound_check check id expr_ in
          check, fst (conv t id))
          (IdentSet.empty, t) let_ in
        let fmap (id, exp) =
          match IdentMap.find_opt id t.map with
          | None -> assert false (* we've just put it into *)
          | Some id -> (id, expr t exp) in
        Q.LetRecIn (label, List.map fmap let_, expr t in_)

    | Q.Lambda (label, fl, x) ->
        let t, fl = List.fold_left_map_stable conv t fl in
        Q.Lambda (label, fl, expr t x)

    | Q.Apply (label, f, args) ->
        let f = expr t f in
        let args = List.map_stable (expr t) args in
        Q.Apply (label, f, args)

    | Q.Match (label, e2, pat_expr) ->
        let map (pat_, expr_) =
          let t, pat_ = pat t pat_ in
          let expr_ = expr t expr_ in
          (pat_, expr_) in
        Q.Match (label, expr t e2, List.map map pat_expr)

    | Q.Record (label, fields) -> Q.Record (label, (List.map (fun (f, e) -> (f, expr t e)) fields))

    | Q.Dot (label, e, f) -> Q.Dot (label, expr t e, f)
    | Q.ExtendRecord (label, f, e, n) -> Q.ExtendRecord (label, f, expr t e, expr t n)
    | (Q.Bypass _) as by -> by
    | Q.Coerce (label, e, ty) -> Q.Coerce (label, expr t e, ty)
    | Q.Path (label, p,h) -> Q.Path (label, List.map (function Q.Db.ExprKey e -> Q.Db.ExprKey (expr t e) | k -> k) p, h)

    | Q.Directive (_, `backend_ident _,_,_) as e ->
        (* not going inside `backend_ident, because it does contain
         * an unbound ident, but this in on purpose *)
        e

    | Q.Directive (_, (#QmlDependencies.directive_dep as dir), _, _) as expr0 ->
        let _ =
          match t.val_ with
          | Some val_ ->
              QmlDependencies.fold_directive_deps (fun x -> x) val_ dir
                (fun id _ ->
                   if not (IdentMap.mem id t.map) && not (t.weak id)
                   then
                     let context =
                       let c = QmlError.Context.expr e in
                       let c = HacksForPositions.map c in
                       c
                     in
                     Error.unbound_ident context id
                )
                ()
          | None -> ()
        in
        expr0

    | Q.Directive (label, d, e, ty) -> Q.Directive (label, d, List.map (expr t) e, ty)
  in !! c

(* wait for a next commit with unification of NewVal and NewValRec, with a flag `valrec | `valand *)
let code_elt t = function
  | Q.NewVal (label, val_) ->
      let fold_map (check, ft) (id, expr_) =
        let check = several_bound_check check id expr_ in
        let ft, id = conv ft id in
        (check, ft), (id, expr t expr_) in
      let (_, t), val_ = List.fold_left_map fold_map (IdentSet.empty, t) val_ in
      t, Q.NewVal (label, val_)

  | Q.NewValRec (label, val_) ->
      let (_, t) = List.fold_left (
        fun (check, t) (id, expr_) ->
          let check = several_bound_check check id expr_ in
          check, fst (conv t id)
      )
        (IdentSet.empty, t) val_
      in
      let map (id, exp) =
        match IdentMap.find_opt id t.map with
        | None -> assert false (* same thing *)
        | Some id -> (id, expr t exp) in
      let val_ = List.map map val_ in
      t, Q.NewValRec (label, val_)

  | Q.Database (_, db_id, _, _) as elt ->
      (* a hack: register the id from db, but don't alpha-convert it;
         TODO: complain early (here?) if 2 db have the same prefix *)
      { t with weak = fun id -> Ident.equal id db_id || t.weak id }, elt

  | elt -> t, elt

let code t code =
  List.fold_left_map code_elt t code

let ident t id = IdentMap.find_opt id t.map
let rev_ident t id = IdentMap.find_opt id t.rev

let next_code ?(weak=fun _ -> false) code_ =
  let next = next ~weak () in
  snd (code next code_)

let clean t =
  let tmp = next () in
  let map = IdentMap.fold (
    fun neww org map ->
      IdentMap.add org neww map
  ) t.rev tmp.map
  in
  { tmp with rev = t.rev; map = map }

(*  this function update the qmlAlphaConv acc to be able to make external first level renaming
    without losing the succession org -> alpha -> alpha -> ... -> last alpha
    An alpha convversion must be launched after this
*)
let update t m =
  let t = clean t in
  let tmp = IdentMap.fold (
    fun neww org acc ->
      match IdentMap.find_opt neww m with
      | None -> acc
      | Some i -> IdentMap.add i org (IdentMap.remove neww acc)
  ) t.rev t.rev
  in
  { t with rev = tmp }

module Check =
struct
  type ('env, 'a) checker = ('env -> 'a)  -> 'env PassHandler.cond

  let cond_ident =
    let doc = "Idents checks" in
    WarningClass.create
      ~parent:WarningClass.cond
      ~name:"ident"
      ~doc
      ~err:true
      ~enable:true
      ()
  let cond_ident_alpha =
    WarningClass.create
      ~parent:cond_ident
      ~name:"alpha"
      ~doc:"Alpha-conversion preconditions"
      ~err:true
      ~enable:true
      ()
  let cond_ident_unbound =
    WarningClass.create
      ~parent:cond_ident
      ~name:"unbound"
      ~doc:"Checking existence of unbound idents"
      ~err:true
      ~enable:true
      ()
  let cond_ident_unicity =
    WarningClass.create
      ~parent:cond_ident
      ~name:"unicity"
      ~doc:"Checking unicity of idents"
      ~err:true
      ~enable:true
      ()

  let id = PassHandler.define_cond cond_ident
  let alpha_id = PassHandler.define_cond cond_ident_alpha
  let unicity_id = PassHandler.define_cond cond_ident_unicity
  let unbound_id = PassHandler.define_cond cond_ident_unbound

  (* always accept that identifiers from other packages are unbound
   * (in the current package of course) *)
  let default_weak ident =
    match Ident.safe_get_package_name ident with
    | Some p -> ObjectFiles.get_current_package_name () <> p
    | None -> false

  (* Checks that alpha-conversion preconditions are satisfied. *)
  let alpha extract =
    PassHandler.make_condition unicity_id
      (fun env ->
         let annotmap, code_ = extract env in
         HacksForPositions.set_annotmap annotmap ;
         Error.check_fail := Some alpha_id ;
         ignore (code (next ~weak:default_weak ()) code_) ;
         HacksForPositions.free_annotmap () ;
         Error.check_fail := None ;
         ()
      )

  let unbound ?(weak=fun _ -> false) val_ extract =
    let weak ident = weak ident || default_weak ident in
    PassHandler.make_condition unbound_id
      (fun env ->
         let annotmap, code_ = extract env in
         HacksForPositions.set_annotmap annotmap ;
         Error.check_fail := Some alpha_id ;
         let t = next ~weak () in
         let t = { t with val_ = Some val_ } in
         ignore (code t code_) ;
         HacksForPositions.free_annotmap () ;
         Error.check_fail := None ;
         ()
      )

  (* unicity *)

  let map_add k v m =
    IdentMap.add
      k (v :: (try IdentMap.find k m with Not_found -> [])) m

  (*
    Compute a map of id, in which each id is associated to the list of its binding.
    At the end, any id with an non singleton binding list means a duplication of identifier.
  *)
  let identmap code =
    let fold_expr identmap e =
      QmlAstWalk.ExprPatt.fold
        (fun identmap e ->
           match e with
           | Q.LetIn (_, ds, _)
           | Q.LetRecIn (_, ds, _) ->
               List.fold_left
                 (fun identmap (id,_) -> map_add id (`expr e) identmap)
                 identmap ds
           | Q.Lambda (_, args, _) ->
               List.fold_left
                 (fun identmap id -> map_add id (`expr e) identmap)
                 identmap args
           | _ -> identmap)
        (fun identmap p ->
           match p with
           | Q.PatVar (_, id) | Q.PatAs (_, _, id) ->
               map_add id (`pat p) identmap
           | _ -> identmap)
        identmap e
    in
    let check_elt identmap = function
      | Q.NewVal (_, ds)
      | Q.NewValRec (_, ds) ->
          List.fold_left
            (fun identmap (id, e) ->
               let identmap = map_add id (`expr e) identmap in
               fold_expr identmap e)
            identmap ds
      | Q.Database (_, id, _, _) ->
          let dummy_expr = QC.ident id in
          map_add id (`expr dummy_expr) identmap
      | Q.NewDbValue (_, dbval) ->
          fst (Q.Db.foldmap_expr (fun m e -> fold_expr m e, e) identmap dbval)
      | _ -> identmap
    in
    List.fold_left check_elt IdentMap.empty code

  let unicity extract =
    PassHandler.make_condition unicity_id
      (fun env ->
         let annotmap, code = extract env in
         HacksForPositions.set_annotmap annotmap ;
         let identmap = identmap code in
         let iter id = function
           | [] -> assert false
           | [_] -> ()
           | hd::tl ->
               let make_context = function
                 | `expr e ->
                     QmlError.Context.expr e
                 | `pat p ->
                     QmlError.Context.pat p
               in
               let context =
                 let fold acc bind =
                   let context = make_context bind in
                   QmlError.Context.merge2 acc context
                 in
                 List.fold_left fold (make_context hd) tl
               in
               let context = HacksForPositions.map context in
               QmlError.scheck_fail unicity_id context (
                 "The ident @{<bright>%s@} is not uniq"
               )
                 (Ident.to_string id)
         in
         let () = IdentMap.iter iter identmap in
         HacksForPositions.free_annotmap ();
         ()
      )
end
