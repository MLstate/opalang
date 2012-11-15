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
    @author Sebastien Briais
*)

(* references:

    Lambda lifting in quadratic time
    Olivier Danvy, Ulrik Schultz

    ML-Style Typing, Lambda Lifting and Partial Evaluation
    Peter Thiemann
*)

(* Lambda lifting is composed of three passes:

   1-name every anonymous lambda
   2-compute the extra parameters to add to the functions and lift them
   3-hoist the (now) closed functions to toplevel

   1-The first pass is trivial.
   The job is done by name_anonymous_lambda_* functions.

   2-The second pass is the difficult one.
   The job is done by parameterLift* functions.
   It is done as explained in Danvy and Schultz article.

   At each "let rec ... and ...", the call graph of the currently defined
   functions is built.
   The solution for these functions (ie the extra parameters to add) is
   then computed by walking through the strongly connected components of
   this call graph in reverse order.
   Once the solution is found, the code is rewritten. Some care must be
   taken in order to guarantee that identifiers are defined only once.

   3-The third pass is less easy than it might appear at first glance,
   especially if type information must be maintained.
   Some reorder is needed here.
   The job is done by hoist* functions and split_and_reorder.

   Some non trivial examples:

   1-Parameter lifting, code reordering

   val f u v w =
     let rec g x = x + v
     and h y = g y + i y
     and i z =
       let j () = h z + w in j ()
     in i u

   This shows the difficulty to compute the extra parameters.
   Despite h is closed, some extra parameters need to be added
   to it, since it calls i (which is non closed).

   The call graph for the big "let rec ... and ..." which
   define g, h and i will be

   g ->
   h -> g, i
   i -> h

   The free variables (not functions) are

   fv(g) =  v
   fv(h) = 0
   fv(i) = w

   The scc are
   {h, i} -> {g}

   The solution is computed by walking throught the scc in reverse order.
   We get:

   {g} -> {v}
   {h, i} -> {v, w}

   Hence, after the parameter lifting pass, we get:

   val f u v w =
     let rec g v x = x + v
     and h v w y = g v y + i v w y
     and i v w z =
       let j v w z () = h v w z + w in j v w z ()
     in i v w u

   After code hoisting, we get:

   val f u v w = i v w u
   val g v x = x + v
   val h v w y = g v y + i v w y
   val i v w z = j v w z ()
   val j v w z () = h v w z + w

   Obviously, some reordering is needed. We finally get:

   val g v x = x + v

   val rec h v w y = g v y + i v w y
   and i v w z = j v w z ()
   and j v w z () = h v w z + w

   val f u v w = i v w u

   2-Typing issues - parameter lifting

   val id x = x
   val h v =
     let y = (id,v) in
     let g z = y.fst y.fst y.snd
     in g v

   Here, y has type ('a->'a,'b)

   Since y is free in g, it needs to be given as an extra parameter to g.

   But, the following code will not type in ML.

   val id x = x
   val h v =
     let y = (id,v) in
     let g y z = y.fst y.fst y.snd
     in g y v

   This is because the type scheme of y is instantiated with two
   different types in g.

   One solution is to lift y several times, one for each instantiation
   of y.

   If rank-2 polymorphism is possible, then things become easier. We
   just need to annotate the type of y, when added as an extra
   parameter to g.

   val id x = x
   val h v =
     let y = (id,v) in
     let g (y:forall 'a.('a->'a,'b)) z = y.fst y.fst y.snd
     in g y v

   3-Typing issues - code hoisting

   Assume

   null: 'a list -> bool
   tl : 'a list -> 'a

   The following code shows that code hoisting is not that easy, if
   one do no want to break typability.

   val rec f y z =
     let rec g x =
       if (null x) then f [] z else g (tl x)
       in
         if null y then 0
         else (g [0]) + (g [true])

   Basically, we will get:

   val rec g x = if (null x) then f [] z else g (tl x)
   and f y z = if null y then 0 else (g [0]) + (g [true])

   Unfortunately, this does not type anymore.

   One solution is to implement what is explained in Thiemann's article.

   Another solution is to have rank-2 polymorphism and polymorphic recursion.

   In this latter case, we will simply annotate g with the
   type forall 'a.('a list->int).
*)

(* depends *)
module List = Base.List

(* shorthands *)
module Q = QmlAst

(* refactoring in progress *)

(* -- *)

type options = {
  mode : [ `typed | `untyped | `fun_action of (IdentSet.t ref * IdentSet.t ref) ];
}

#<Debugvar:LAMBDA_DEBUG>

let debug_coerce annotmap body _ty =
  #<If:LAMBDA_COERCE>
    QmlAstCons.TypedExpr.coerce annotmap body _ty
  #<Else>
    annotmap,body
  #<End>

let pp_ident_set f set =
  IdentSet.iter (fun x -> Format.fprintf f "%s@ " (Ident.to_string x)) set

type 'a ignored_directive =[
| Q.type_directive
| Q.lambda_lifting_directive
| Q.slicer_directive
| Q.closure_instrumentation_directive
| `async
]

(* some utility functions to get types and type schemes *)
let get_ty annotmap ann =
  QmlAnnotMap.find_ty ann annotmap

let get_tsc gamma x =
  match QmlTypes.Env.Ident.find_opt x gamma with
  | None -> failwith (Format.sprintf "get_tsc: cannot find %s in gamma" (Ident.to_string x))
  | Some(tsc) -> tsc

let get_explicit_tsc gamma x =
  QmlTypes.Scheme.explicit_forall (get_tsc gamma x)

let get_tsc_annotmap annotmap ann =
  match QmlAnnotMap.find_tsc_opt ann annotmap with
  | None -> QmlTypes.Scheme.id (get_ty annotmap ann)
  | Some(tsc) -> tsc

(* compute the free names of an expression
   input:
     e: Q.expr
        the expression to analyse
   output:
       the set of free names of e
*)
let fn_of_expr e =
  QmlAstWalk.Expr.fold_with_env
    (* collect the binders *)
    (fun bn x _ ->
       IdentSet.add x bn)
    (* no binder initially *)
    IdentSet.empty
    (* collect the free variables *)
    (fun bn fn e ->
       match e with
       | Q.Ident (_, x) ->
           if IdentSet.mem x bn then fn
           else IdentSet.add x fn
       | _ -> fn)
    (* no free variables initially *)
    IdentSet.empty
    (* the expression to analyse *)
    e

let ftv_of_expr annotmap e =
  let ty_e = get_ty annotmap (Q.QAnnot.expr e) in
  let fv_e = QmlTypes.freevars_of_ty ty_e in
  QmlAstWalk.Expr.fold
    (fun fv_t e ->
       match e with
       | Q.Directive (_, `typeval subst, [], [ty]) ->
           assert (subst = None);
           let fv_ty =
             QmlTypes.FreeVars.diff (QmlTypes.freevars_of_ty ty) fv_e in
           QmlTypes.FreeVars.union fv_t fv_ty
       | _ -> fv_t)
    QmlTypes.FreeVars.empty
    e

(* depending on e,
   build the node
   let bnds in body, or
   let rec bnds in body
*)
let mk_let_rec e bnds body =
  match e with
  | Q.LetIn (label, _, _) ->
      (* QmlAstCons.UntypedExpr.letin bnds body *)
      Q.LetIn (label, bnds, body)
  | Q.LetRecIn (label, _, _) ->
      (* QmlAstCons.UntypedExpr.letrecin bnds body *)
      Q.LetRecIn (label, bnds, body)
  | _ -> assert false

(* give a name to anonymous lambda *)
let name_anonymous_lambda_expr ~options annotmap (toplevel_name,e) =
  let mk_let annotmap x e =
    (* build the node "let x = e in x" *)
    if options.mode = `typed then
      let ty = get_ty annotmap (Q.QAnnot.expr e) in
      let annotmap,id_x = QmlAstCons.TypedExpr.ident annotmap x ty in
      let annotmap,let_in = QmlAstCons.TypedExpr.letin annotmap [x,e] id_x in
      annotmap,let_in
    else (
      let pos_let = Q.Pos.expr e in
      let label_let = Annot.next_label pos_let in
      let label_ident = Annot.next_label pos_let in
      (annotmap,
       (QmlAstCons.UntypedExprWithLabel.letin
          ~label: label_let [x,e]
          (QmlAstCons.UntypedExprWithLabel.ident ~label: label_ident x)))
     ) in
  (* is_anonymous is a flag which indicates
     whether the given expression is the rhs of a let *)
  let aux =
    match options.mode with
    | `typed
    | `untyped ->
        let rec aux tra is_anonymous annotmap e =
          match e with
          | Q.Lambda _ when is_anonymous ->
              let fun_ident = Ident.next "an" in
              let annotmap, e = aux tra false annotmap e in
              mk_let annotmap fun_ident e
          | Q.LetIn (_, bnds, body)
          | Q.LetRecIn (_, bnds, body) ->
              let annotmap,bnds' =
                List.fold_left_map_stable
                  (fun annotmap ((x,e) as p) ->
                     let annotmap,e' = aux tra false annotmap e in
                     if e == e' then annotmap, p else
                       annotmap,(x,e'))
                  annotmap bnds in
              let annotmap,body' = aux tra true annotmap body in
              if body' == body && bnds == bnds' then annotmap, e else
                annotmap,mk_let_rec e bnds' body'
          | Q.Coerce _
          | Q.Directive(_, #ignored_directive,_,_) ->
              tra is_anonymous annotmap e
          | _ ->
              tra true annotmap e in
        aux
    | `fun_action (public_set,client_set) ->
        let rec aux tra _ annotmap e =
          match e with
          | Q.Directive (_, `fun_action v, [e], _) ->
              assert (v = None);
              (* here we don't check that we actually have a lambda inside the fun_action
               * it is on purpose: we want to lift anything, not just lambdas *)
              (* we remove the directive `fun_action, but we will put it back later *)
              let fun_ident = Ident.next "fa" in
              let fun_ident' = Ident.next "fae" in
              public_set := IdentSet.add fun_ident  !public_set;
              client_set := IdentSet.add fun_ident' !client_set;
              let annotmap, e = aux tra false annotmap e in
              let pos = Q.Pos.expr e in
              let label () = Annot.next_label pos in
              annotmap,
              QmlAstCons.UntypedExprWithLabel.letin ~label:(label()) [fun_ident, e]
                (QmlAstCons.UntypedExprWithLabel.letin  ~label:(label())
                   [fun_ident', QmlAstUtils.Lambda.eta_expand_ast 1 (QmlAstCons.UntypedExprWithLabel.ident ~label:(label()) fun_ident)]
                   (QmlAstCons.UntypedExprWithLabel.ident ~label:(label()) fun_ident'))
          | Q.Directive (_, `fun_action _, _, _) -> assert false
          | _ -> tra false annotmap e in
        aux in
  let acc, e = QmlAstWalk.Expr.traverse_foldmap_context_down aux false annotmap e in
  acc, (toplevel_name, e)

(* returns whether e is a function or not *)
let get_arity_of_lambda e =
  QmlAstWalk.Expr.traverse_findmap
    (fun tra e ->
       match e with
       | Q.Lambda (_, args, _) -> Some (List.length args)
       | Q.Coerce _
       | Q.Directive(_, #ignored_directive,_,_) -> tra e
       | _ -> None
    ) e
let is_lambda e = get_arity_of_lambda e <> None

type env = {
  funcs: (QmlTypes.FreeVars.t * Ident.t list * int) IdentMap.t;
  (* maps lifted ident to their typevars environment * environment * original arity *)
  (* maps from identifiers that will be lifted to their free variables *)
  gamma: QmlTypes.gamma (* the gamma this is given back by the pass
                         * starts empty and grows with each definition toplevel def
                         * it contains all toplevel types and is used to determine if
                         * a name is defined at toplevel or not *);
  hoisted : (Ident.t * Q.expr) list list;
  hierarchy : Ident.t list; (* see the description of @lifted_lambda *)
}

(* In the functions that take an env and a gamma, the gamma
 * is the environment only for the current declaration (but it
 * contains everything, not only toplevel identifiers)
 * this gamma never goes 'up', its value from recursive calls
 * is always ignored
 * It is used only to propagate types
 *)


type binding = Ident.t * Q.expr


(* compute a map which give for each function identifiers
   of the funcs declarations
   its set of free functions symbols and its set of free variables
   ie
      function ident -> (free functions, free variables, free typeval)
*)
let get_vars ~options annotmap env (funcs : binding list) =
  let fun_names =
    List.fold_left
      (fun s (x,_e) -> IdentSet.add x s)
      IdentSet.empty funcs in
  List.fold_left
    (fun map (x,e) ->
       let fn = fn_of_expr e in
       let ftv_x =
         match options.mode with
         | `typed -> ftv_of_expr annotmap e
         | _ -> QmlTypes.FreeVars.empty
       in
       let ftv_x,ff_x,fv_x =
         IdentSet.fold
           (fun n (ftv_x, ff_x,fv_x) ->
              try
                let (tv_env, env,_) = IdentMap.find n env.funcs in
                let ftv_x = QmlTypes.FreeVars.union tv_env ftv_x in
                (ftv_x, ff_x,
                 (* when you call a local function, then you need its environment
                  * because you will replace 'f' in your body by f(env1,...,envn) *)
                 List.fold_left
                   (fun env fv -> IdentSet.add fv env)
                   fv_x
                   env)
              with Not_found ->
                if IdentSet.mem n fun_names then
                  (ftv_x,IdentSet.add n ff_x,fv_x)
                else (ftv_x,ff_x,IdentSet.add n fv_x))
           fn
           (ftv_x,IdentSet.empty,IdentSet.empty)
       in
       let fv_x = (* removing the names from the computed gamma
                   * (because they are at the toplevel) *)
         IdentSet.filter
           (fun x -> not (QmlTypes.Env.Ident.mem x env.gamma)) fv_x
       in
       IdentMap.add x (ff_x,fv_x, ftv_x) map)
    IdentMap.empty
    funcs

module M1 =
struct
  type t = {
    ident : Ident.t;
    mutable set : IdentSet.t;
    mutable vars : QmlTypes.FreeVars.t
  }
  let compare {ident=ident1} {ident=ident2} = Ident.compare ident1 ident2
  let hash {ident=ident} = Ident.hash ident
  let equal {ident=ident1} {ident=ident2} = Ident.equal ident1 ident2
end
module G1 = Graph.Imperative.Digraph.Concrete(M1)
module SCC1 = GraphUtils.Components.Make(G1)

(* compute the mapping that gives for each function its environment
   for a nest of mutually recursive functions

   the result is a list of list
   each sublist gives for each function identifiers its extra environment

   each sublist will be turned in a definition of mutually recursive
   functions
*)
let compute_solution ~options annotmap env funcs =
  match funcs with
  | [] -> [] (* no function in the let bindings *)
  | [(i,_)] ->
      (* no mutual recursion -> no need to compute sccs *)
      let (_,fv_i,ftv) = IdentMap.find i (get_vars ~options annotmap env funcs) in
      [([i],IdentSet.elements fv_i,ftv)]
  | _ ->
      let size = 2 in
      let names = get_vars ~options annotmap env funcs in
      (* create the call graph *)
      let g = G1.create ~size () in
    (* first the vertices
        one vertex per function identifier *)
      let vertices =
        IdentMap.mapi
          (fun x (_,fv_x,ftv_x) ->
             let v_x = G1.V.create {M1.ident=x; M1.set=fv_x; M1.vars=ftv_x} in
             G1.add_vertex g v_x;
             v_x)
          names in
      (* then the edges
         if f calls g then add an edge from f to g
      *)
      IdentMap.iter
        (fun x (ff_x,_,_) ->
           let v_x = IdentMap.find x vertices in
           IdentSet.iter
             (fun y ->
                let v_y = IdentMap.find y vertices in
                G1.add_edge g v_x v_y)
             ff_x) names;
      (* compute the strongly connected components *)
      let scc = SCC1.scc ~size g in
      (* compute the vf sets
         walk through the scc in reverse topological order *)
      List.map
        (fun p ->
           let v, tv =
             List.fold_left
               (fun (v, tv) ({M1.set=vf_x_ref; vars=tvf_x_ref} as v_x) ->
                  let (v_x, tv_x) = G1.fold_succ
                    (fun {M1.set=vf_y_ref; vars=tvf_y_ref} (vf, tvf) ->
                       IdentSet.union vf vf_y_ref,
                       QmlTypes.FreeVars.union tvf tvf_y_ref
                    )
                    g v_x (vf_x_ref, tvf_x_ref) in
                  IdentSet.union v v_x,
                  QmlTypes.FreeVars.union tv tv_x
               )
               (IdentSet.empty, QmlTypes.FreeVars.empty)
               p in
           List.iter (fun v_x -> v_x.M1.set <- v; v_x.M1.vars <- tv) p;
           (* order the elements *)
           let elt_v = IdentSet.elements v in
           let f_idents = List.map (fun v_x -> v_x.M1.ident) p in
           f_idents,elt_v,tv)
        scc

let opatype gamma x =
  let ty = match x with
  | `ty  -> Opacapi.Types.OpaType.ty
  | `row -> Opacapi.Types.OpaType.row
  | `col -> Opacapi.Types.OpaType.col
  in fst (QmlTypes.type_of_type gamma (Q.TypeName ([], QmlAst.TypeIdent.of_string ty)))

(* get fresh identifiers for abstracting the functions *)
let get_fresh_identifiers env gamma =
  List.map
    (fun x ->
       let fresh_x =
         Ident.refresh ~descr:"extra" x in
       let ty = get_explicit_tsc gamma x in
       (fresh_x,ty))
    env
let get_fresh_identifiers_untyped env =
  List.map (Ident.refresh ~descr:"extra") env

let get_identifiers_from_ftv gamma ftv =
  let aux x = List.map (fun tv -> Ident.from_fresh tv, opatype gamma x) in
  let tvs, rvs, cvs = QmlTypes.FreeVars.export_as_lists ftv in
  let tvs, rvs, cvs = aux `ty tvs, aux `row rvs, aux `col cvs in
  tvs @ rvs @ cvs


(* add lambda on top of an expression *)
let absify ~toplevel env gamma_with_lambda_bindings annotmap e txs xs =
  match txs, xs with
  | [], [] when toplevel -> annotmap, e
  | _, [] when toplevel ->
      OManager.i_error
        "A lifted lambda has a type variables in it's environment but without a standard environment"
  | _ ->
      QmlAstWalk.Expr.traverse_foldmap
        (fun tra annotmap -> function
         | Q.Lambda (_, il, e) ->
             let orig_xs =
               List.map (fun i ->
                           let tsc = QmlTypes.Env.Ident.find i gamma_with_lambda_bindings in
                           let ty = QmlTypes.Scheme.explicit_forall tsc in
                           (i, ty)) il in
             let annotmap, e = QmlAstCons.TypedExpr.lambda annotmap (txs @ xs @ orig_xs) e in
             QmlAstCons.TypedExpr.directive_id annotmap
               (`lifted_lambda (List.length txs + List.length xs, List.tl env.hierarchy)) e
         | Q.Coerce _
         | Q.Directive (_, #ignored_directive, _, _) as e ->
             tra annotmap e
         | Q.Directive _ -> assert false
         | _ ->
             (* you don't add parameters to something that is not a function *)
             assert false
        )
        annotmap e



let absify_untyped ~toplevel env e xs =
  match xs with
  | [] when toplevel -> e
  | _ ->
      QmlAstWalk.Expr.traverse_map
        (fun tra expr ->
          match expr with
          | Q.Lambda (_, orig_xs, e) ->
              let pos = Q.Pos.expr expr in
              let label = Annot.next_label pos in
              let lambda = QmlAstCons.UntypedExprWithLabel.lambda ~label (xs @ orig_xs) e in
              QmlAstCons.UntypedExprWithLabel.directive ~label (`lifted_lambda (List.length xs, List.tl env.hierarchy)) [lambda] []
          | Q.Coerce _
          | Q.Directive (_, #ignored_directive, _, _) as e -> tra e
          | _ -> assert false)
        e



let absify_fun_action e xs =
  (* could use 0-ary functions, but since it's completely untested,
   * seems risky for now *)
  let xs = if xs = [] then [Ident.next "_"] else xs in
  let pos = Q.Pos.expr e in
  let label = Annot.next_label pos in
  QmlAstCons.UntypedExprWithLabel.lambda ~label xs e



(* substitution on expressions *)
let subst e sigma ftv =
  QmlAstWalk.Expr.map_up
    (fun e ->
       match e with
       | Q.Ident (label, x) ->
           begin
             try
               let y = IdentMap.find x sigma in
               Q.Ident (label, y)
             with Not_found -> e
           end
       | Q.Directive (l, `typeval subst, [], [ty]) ->
           assert (subst = None);
           if QmlTypes.FreeVars.is_empty ftv then e
           else
             let tvs, rvs, cvs = QmlTypes.FreeVars.export_as_lists ftv in
             let aux add = List.fold_left
               (fun map tv -> add tv (Ident.from_fresh tv) map)
             in
             let tvs = aux QmlTypeVars.TypeVarMap.add QmlTypeVars.TypeVarMap.empty tvs in
             let rvs = aux QmlTypeVars.RowVarMap.add QmlTypeVars.RowVarMap.empty rvs in
             let cvs = aux QmlTypeVars.ColVarMap.add QmlTypeVars.ColVarMap.empty cvs in
             Q.Directive (l, `typeval (Some (tvs, rvs, cvs)), [], [ty])
       | _ -> e
    ) e


let mk_let_rec_tree ~options (gamma,annotmap,env) e funcs vals body =
  let aux env f =
    List.fold_left_map
      (fun (env,annotmap) ((f,body) as bnd) ->
         match options.mode with
         | `fun_action (public_set, client_set) ->
             let label = Annot.refresh (Q.Label.expr body) in
             ignore (label, public_set, client_set);
             let body =
               if IdentSet.mem f !public_set then
                 Q.Directive (label, `visibility_annotation (`public `funaction), [body], [])
               else (
                 assert (IdentSet.mem f !client_set);
                 Q.Directive (label, `side_annotation `client, [body], [])
               ) in
             (env, annotmap), (f, body)
         | `untyped ->
             (env, annotmap), bnd
         | `typed ->
             let annot = Q.QAnnot.expr body in
             let tsc = QmlTypes.Env.Ident.find f env.gamma in
             let annotmap =
               if QmlGenericScheme.is_empty tsc then
                 annotmap
               else
                 (* tsc_gen for ei, only when non trivial *)
                 QmlAnnotMap.add_tsc annot tsc annotmap in
             (*let ty = QmlTypes.Scheme.explicit_forall tsc in
               let annotmap,body = debug_coerce annotmap body ty in*)
             (env, annotmap), bnd
      ) env f in
  let annotmap, body =
    match vals with
    | [] ->
        (* beware of not dumping the annot for ei that was (maybe) on the original LetIn *)
        let annot_e = Q.QAnnot.expr e in
        let annot_body = Q.QAnnot.expr body in
        assert (QmlAnnotMap.find_tsc_opt annot_body annotmap = None);
        let annotmap =
          QmlAnnotMap.add_tsc_opt annot_body
            (QmlAnnotMap.find_tsc_opt annot_e annotmap) annotmap in
        annotmap, body
    | _ ->
        annotmap, mk_let_rec e vals body in
  let (env, annotmap), funcs = List.fold_left_map aux (env,annotmap) funcs in
  let env = {env with hoisted = List.rev_append funcs env.hoisted} in
  (gamma,annotmap,env), body

let mk_apply gamma annotmap e args old_args =
  let annotmap, args =
    List.fold_left_map (fun annotmap (x,ty) -> QmlAstCons.TypedExpr.ident annotmap x ty) annotmap args in
  QmlAstCons.TypedExpr.apply gamma annotmap e (args @ old_args)

type context = (* this datatype represents an apply node
                * its use is described in Apply case below *) {
  applied : Q.expr;
  args : Q.expr list;
  mutable used : bool;
  tsc_gen_opt : (Q.ty,unit) QmlGenericScheme.tsc option;
}

module IdentAssoc = BaseList.MakeAssoc(Ident)

let rec get_arrow_ty annotmap = function
  | Q.Lambda (label,_,_) -> (
      match QmlAnnotMap.find_ty_label label annotmap with
      | Q.TypeArrow (a,b) -> (a,b)
      | _ -> assert false
    )
  | Q.Coerce (_, e, _) ->
      get_arrow_ty annotmap e
  | Q.Directive (_, #ignored_directive, l, _) -> (
      match l with
      | [e] -> get_arrow_ty annotmap e
      | _ -> assert false
    )
  | _ -> assert false

let get_params_and_return_of_arrow_type gamma ty =
  match QmlTypesUtils.Inspect.follow_alias_noopt gamma ty with
  | Q.TypeArrow (params,ty) -> params,ty
  | _ -> assert false

let wrap_partial_apply ~partial annotmap e =
  let info = QmlAnnotMap.find (Q.QAnnot.expr e) annotmap in
  let annot = Annot.next () in
  let annotmap = QmlAnnotMap.add annot info annotmap in
  let label = Annot.make_label annot (Q.Pos.expr e) in
  annotmap, Q.Directive (label, partial, [e], [])

let wrap_partial_apply_untyped ~partial e =
  let label = Annot.next_label (Q.Pos.expr e) in
  Q.Directive (label, partial, [e], [])

(* solution: map function names to list of variables

   e: the expression to lift *)
(* gamma = typing environment of the currently analysed expression *)
let rec parameterLiftExp ~options ?outer_apply ((gamma,annotmap,env) as full_env) e =
  match e with
  | Q.Coerce _
  | Q.Directive (_, #ignored_directive, _, _) ->
      (* propagating outer_apply *)
      QmlAstWalk.Expr.foldmap_nonrec (fun acc e -> parameterLiftExp ~options ?outer_apply acc e) full_env e
  | Q.Apply (label, e1, es) ->
      (* we may need to regroup this apply in the case f(args) to create
       * f(env,args) and not f(env)(args)
       * so we remember it in [outer_apply] and we use it in the ident case
       * we can't match Apply (Ident _, _) because we may have directives or
       * coercions in the middle *)
      let acc, es' = List.fold_left_map (fun acc e -> parameterLiftExp ~options acc e) full_env es in
      let tsc_gen_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
      let outer_apply = {applied = e1; args = es'; used = false; tsc_gen_opt} in
      let acc, e1' = parameterLiftExp ~options ~outer_apply acc e1 in
      acc, if e1 == e1' && es == es' then e else
             if outer_apply.used then e1' else Q.Apply (label, e1', es')
  | (Q.Ident (label, x)) as whole_expr ->
      begin
        try
          (* if ident is a function symbol *)
          (* the args have not yet been refreshed
             (need to be substituted afterwards) *)
          let (ftv,args,original_arity) = IdentMap.find x env.funcs in
          match QmlTypes.FreeVars.is_empty ftv, args, options.mode with
          | true, [], `typed ->
              let tsc = QmlTypes.Env.Ident.find x env.gamma in
              let annotmap =
                if QmlGenericScheme.is_empty tsc then annotmap
                else QmlAnnotMap.add_tsc_inst_label label tsc annotmap in
              (gamma,annotmap,env), e
          | true, [], `untyped ->
              (* we need to reinsert the @fun_action directive even if the lambda had no
               * environment *)
              full_env, e
          | _ ->
              let e, old_args, partial, orig_tsc_gen_opt =
                match outer_apply with
                | None ->
                    assert (original_arity <> -1);
                    let tsc_gen_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
                    e, [], `partial_apply (Some original_arity,false), tsc_gen_opt
                    (* ident with an env -> partial application *)
                | Some ({applied = e; args = el; used; tsc_gen_opt} as context) ->
                    (* full apply (if the user code didn't contain
                     * any partial apply) *)
                    assert (used = false);
                    (* we say that we already used the outer apply
                     * or else we would end up with [f(env,args)(args)]
                     * instead of [f(env,args)] *)
                    context.used <- true;
                    e, el, `full_apply (List.length args + QmlTypes.FreeVars.size ftv), tsc_gen_opt in
              (match options.mode with
               | `typed ->
                   let args =
                     List.map
                       (fun x ->
                          let ty = get_explicit_tsc gamma x in x,ty)
                       args in
                   let args = get_identifiers_from_ftv env.gamma ftv @ args in
                   let ty =
                     let ty = get_ty annotmap (Q.QAnnot.expr e) in
                     match partial with
                     | `partial_apply _ ->
                       (* if we create [f(env)], we say that
                        * that [f] has type [env -> args -> return] *)
                       Q.TypeArrow (List.map snd args, ty)
                     | `full_apply _ ->
                       (* if we create [f(env,args)], we say that
                        * [f] has type [env, args -> return] *)
                       let params, ty = get_params_and_return_of_arrow_type env.gamma ty in
                       Q.TypeArrow (List.map snd args @ params, ty) in
                   let annotmap,e = QmlAstCons.TypedExpr.ident annotmap x ty in
                   let annotmap =
                     let tsc = QmlTypes.Env.Ident.find x env.gamma in
                     if QmlGenericScheme.is_empty tsc then annotmap
                     else QmlAnnotMap.add_tsc_inst (Q.QAnnot.expr e) tsc annotmap in
                   let annotmap,e =
                     let annotmap, e = mk_apply gamma annotmap e args old_args in
                     wrap_partial_apply ~partial annotmap e in
                   (* propagating the tsc_gen that was (maybe) on the ident or apply *)
                   let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr e) orig_tsc_gen_opt annotmap in
                   (gamma,annotmap,env),e
               | `untyped ->
                   assert (QmlTypes.FreeVars.is_empty ftv);
                   let e =
                     let args =
                       List.map
                         (fun arg_expr ->
                           (* Use location of the whole initial expression we
                              are processing. *)
                           let pos = Q.Pos.expr whole_expr in
                           let label = Annot.next_label pos in
                           QmlAstCons.UntypedExprWithLabel.ident
                             ~label arg_expr)
                         args in
                     (* Like above, let's use location of the whole initial
                        expression we are processing. *)
                     let pos_for_e = Q.Pos.expr whole_expr in
                     let label_for_e = Annot.next_label pos_for_e in
                     let e =
                       QmlAstCons.UntypedExprWithLabel.apply
                         ~label: label_for_e e (args @ old_args) in
                     wrap_partial_apply_untyped ~partial e in
                   (full_env, e)
               | `fun_action (public_set,_) ->
                   let pos = Annot.pos label in
                   let args =
                     (* could use a 0-ary function in the case args=[] *)
                     if args = [] then [QmlAstCons.UntypedExprWithLabel.int ~label:(Annot.next_label pos) 0]
                     else List.map (fun i -> QmlAstCons.UntypedExprWithLabel.ident ~label:(Annot.next_label pos) i) args in
                   let e = QmlAstCons.UntypedExprWithLabel.may_apply ~label:(Annot.next_label pos) (QmlAstCons.UntypedExprWithLabel.may_apply ~label:(Annot.next_label pos) e args) old_args in
                   if IdentSet.mem x !public_set then
                     full_env, e
                   else
                     let label = Annot.next_label (Q.Pos.expr e) in
                     let e = Q.Directive (label, `fun_action None, [e], []) in
                     full_env, e
              )
        with Not_found -> full_env,e
      end
  | Q.LetIn (_, bnds, body)
  | Q.LetRecIn (_, bnds, body) ->
      let full_env,(funcs,vals) =
        parameterLiftBnds ~options ~toplevel:false full_env bnds in
      let (_,annotmap,env),body =
        parameterLiftExp ~options full_env body in
      mk_let_rec_tree ~options (gamma,annotmap,env) e funcs vals body
  | Q.Match (label, expr, guards) ->
      let (_,annotmap,env),expr = parameterLiftExp ~options full_env expr in
      let (_,annotmap,env),guards =
        parameterLiftGuards ~options (gamma,annotmap,env) guards in
      (gamma,annotmap,env), Q.Match (label, expr, guards)
  | _ -> QmlAstWalk.Expr.foldmap_nonrec (fun acc e -> parameterLiftExp ~options acc e) full_env e

and parameterLiftGuards ~options (gamma,annotmap,env) guards =
  let update_gamma gamma annotmap pat =
    QmlAstWalk.Pattern.fold_down
      (fun gamma p ->
         match p with
         | Q.PatVar (_, x) ->
             let ty = get_ty annotmap (Q.QAnnot.pat p) in
             QmlTypes.Env.Ident.add x (QmlTypes.Scheme.id ty) gamma
         | Q.PatAs (_, _, x) ->
             (* Instead of:
                  let ty = get_ty annotmap (Q.QAnnot.pat p) in
                  let tsc = QmlTypes.Scheme.generalize env.gamma ty in
                we get the scheme specially stored for the alias in which
                column variables have been refreshed to make them generalizable.
                This way, we get the scheme whose body represent the type of
                only the aliased part of the pattern and not the global type
                of all the patterns unified. *)
             let tsc = get_tsc_annotmap annotmap (Q.QAnnot.pat p) in
             QmlTypes.Env.Ident.add x tsc gamma
         | _ -> gamma)
      gamma
      pat in
  let (annotmap,env),guards =
    List.fold_left_map
      (fun (annotmap,env) (pat,expr) ->
         let gamma = if options.mode = `typed then update_gamma gamma annotmap pat else gamma in
         let (_,annotmap,env),expr =
           parameterLiftExp ~options (gamma,annotmap,env) expr
         in
         (annotmap,env),(pat,expr))
      (annotmap,env) guards in
  (gamma,annotmap,env),guards

and parameterLiftBnds ~options ~toplevel (gamma,annotmap,env) bnds =
  let (funcs,vals) =
    match options.mode with
    | `fun_action (public_set,client_set) ->
        (match bnds with
         | [(name,_)] when IdentSet.mem name !public_set || IdentSet.mem name !client_set ->
             (* we name every fun_action, so we know these are always singleton *)
             bnds, []
         | _ -> [], bnds (* no lifting otherwise *)
        )
    | `typed | `untyped ->
        List.partition (fun (_x,e) -> is_lambda e) bnds in
  let gamma =
    if options.mode = `typed then
      List.fold_left
        (fun gamma (x,e) ->
           QmlTypes.Env.Ident.add x (get_tsc_annotmap annotmap (Q.QAnnot.expr e)) gamma)
        gamma
        vals
    else
      gamma
  in
  let funcs_sols = compute_solution ~options annotmap env funcs in
  let env =
    let solution =
      (* update the solution *)
      List.fold_left
        (fun solution (f_idents,env,tv_env) ->
           List.fold_left
             (fun solution f_ident ->
                let body = IdentAssoc.find f_ident funcs in
                let original_arity =
                  match get_arity_of_lambda body with
                  | Some i -> i
                  | None ->
                      assert (match options.mode with `fun_action _ -> true | _ -> false);
                      -1 in
                IdentMap.safe_add f_ident (tv_env,env,original_arity) solution)
             solution
             f_idents)
        env.funcs
        funcs_sols
    in
    { env with funcs = solution }
  in
  let env =
    match options.mode with
    | `typed ->
        let env_gamma =
          List.fold_left
            (fun env_gamma (f_idents,extra,f_tv) ->
               let tys = List.map (get_explicit_tsc gamma) extra in
               let tys = List.map snd (get_identifiers_from_ftv gamma f_tv) @ tys in
               List.fold_left
                 (fun env_gamma f_ident ->
                    let body = IdentAssoc.find f_ident funcs in
                    let ty_params,ty_ret = get_arrow_ty annotmap body in
                    let ty = Q.TypeArrow (tys @ ty_params,ty_ret) in
                    let tsc = QmlTypes.Scheme.quantify ty in
                    QmlTypes.Env.Ident.add f_ident tsc env_gamma
                 ) env_gamma f_idents
            ) env.gamma funcs_sols in
        {env with gamma = env_gamma}
    | `fun_action _ | `untyped -> env in

  let hierarchy = env.hierarchy in

  let (annotmap,env),funcs =
    (* rewrite the body of each function *)
    List.fold_left_map
      (fun (annotmap,env) (f_idents,extra,f_tv) ->
         List.fold_left_map
           (fun (annotmap,env) f_ident ->
              let body =
                (* get the original body of f_ident *)
                snd (List.find
                       (fun (x,_) -> Ident.equal x f_ident)
                       funcs) in
              (* lift the body *)
              let env = {env with hierarchy = f_ident :: hierarchy} in
              let (gamma_with_lambda_bindings,annotmap,env),body =
                parameterLiftLambda ~options (gamma,annotmap,env) body in
              let annotmap,body,sigma =
                match options.mode with
                | `typed ->
                    (* get fresh identifiers (that will be abstracted) *)
                    let fresh_extra = get_fresh_identifiers extra gamma in
                    let tv_extra = get_identifiers_from_ftv gamma f_tv in
                    (* abstract the new variables *)
                    let annotmap,body = absify ~toplevel env gamma_with_lambda_bindings annotmap body tv_extra fresh_extra in
                    (* compute the substitution
                       free variable -> fresh identifier *)
                    let sigma =
                      List.fold_left
                        (fun sigma (x,(y,_)) ->
                           IdentMap.add x y sigma)
                        IdentMap.empty
                        (List.combine extra fresh_extra) in
                    annotmap,body,sigma
                | `fun_action _ | `untyped as mode ->
                    assert (QmlTypes.FreeVars.is_empty f_tv);
                    (* Warning: the types are not the same as the code above *)
                    let fresh_extra = get_fresh_identifiers_untyped extra in
                    let body =
                      match mode with
                      | `fun_action _ -> absify_fun_action body fresh_extra
                      | `untyped -> absify_untyped ~toplevel env body fresh_extra in
                    let sigma = IdentMap.from_list (List.combine extra fresh_extra) in
                    annotmap,body,sigma in
              (* replace each free variables by the corresponding
                 fresh identifier *)
              let body = subst body sigma f_tv in
              (annotmap,env),(f_ident,body))
           (annotmap,env)
           f_idents)
      (annotmap,env)
      funcs_sols in
  let (annotmap,env),vals =
    List.fold_left_map
      (fun (annotmap,env) (x,e) ->
         let (_,annotmap,env),e = parameterLiftExp ~options (gamma,annotmap,env) e in
         (annotmap,env),(x,e))
      (annotmap,env)
      vals in

  let env = {env with hierarchy} in

  (gamma,annotmap,env),(funcs,vals)

(* the gamma returned by this function contains the identifiers bound by the lambda
 * this is actually needed by parameterLiftBinding to avoid recomputing the types
 * of the parameters (and since the code is globally renamed, having too much names
 * in the gamma isn't a problem) *)
and parameterLiftLambda ~options ((gamma,annotmap,env) as full_env) e =
  match e with
  | Q.Lambda (_, params,_) when options.mode = `typed ->
      begin
        let ty = get_ty annotmap (Q.QAnnot.expr e) in
        match ty with
        | Q.TypeArrow (ty_params,_) ->
            let gamma =
              List.fold_left2
                (fun gamma x ty -> QmlTypes.Env.Ident.add x (QmlTypes.Scheme.id ty) gamma)
                gamma params ty_params in
            let (gamma, annotmap, env), e =
              QmlAstWalk.Expr.foldmap_nonrec (parameterLiftLambda ~options) (gamma,annotmap,env) e
            in
            (try
              let annotmap, e =
                let fv_e = QmlTypes.freevars_of_ty ty in
                let fv_t = IdentMap.fold
                  (fun _ (fv, _, _) acc ->
                     QmlTypes.FreeVars.union fv acc
                  ) env.funcs QmlTypes.FreeVars.empty
                in
                let fv_et = QmlTypes.FreeVars.inter fv_e fv_t in
                let tvs, rvs, cvs = QmlTypes.FreeVars.export_as_lists fv_et in
                let annotmap, tvs = List.fold_left_map
                  (fun annotmap tv ->
                     let annotmap, e =
                       QmlAstCons.TypedExpr.directive annotmap (`typeval None) []
                         [QmlAstCons.Type.typevar tv] in
                     annotmap, (Ident.from_fresh tv, e)
                  ) annotmap tvs
                in
                let annotmap, rvs = List.fold_left_map
                  (fun annotmap rv ->
                     let ty = QmlAst.TypeRecord (Q.TyRow ([], Some rv)) in
                     let annotmap, e =
                       QmlAstCons.TypedExpr.directive annotmap (`typeval None) [] [ty] in
                     annotmap, (Ident.from_fresh rv, e)
                  ) annotmap rvs
                in
                let annotmap, cvs = List.fold_left_map
                  (fun annotmap cv ->
                     let ty = QmlAst.TypeSum (Q.TyCol ([], Some cv)) in
                     let annotmap, e =
                       QmlAstCons.TypedExpr.directive annotmap (`typeval None) [] [ty] in
                     annotmap, (Ident.from_fresh cv, e)
                  ) annotmap cvs
                in
                match e with
                | Q.Lambda (l, p, b)  ->
                    let annotmap, b =QmlAstCons.TypedExpr.letin annotmap (cvs @ rvs @ tvs) b in
                    annotmap, Q.Lambda (l, p, b)
                | _ -> assert false
              in
              (gamma, annotmap, env), e
             with Not_found -> (gamma, annotmap, env), e
            )
        | _ -> (* could happen with overloads, the clean way to solve this
                * would be to have annotations on lambda and let bound bindings *)
            let context = QmlError.Context.annoted_expr annotmap e in
            QmlError.i_error None context (
              "unexpected type @{<bright>%a@}"
            )
              QmlPrint.pp#ty ty
      end
  | Q.Lambda _ (* no need to look at the type of arguments in untyped mode *)
  | Q.Coerce _
  | Q.Directive (_, #ignored_directive, _, _) ->
      QmlAstWalk.Expr.foldmap_nonrec (fun acc e -> parameterLiftLambda ~options acc e) full_env e
  | _ -> parameterLiftExp ~options full_env e



let name_anonymous_lambda_code_elt ~options annotmap elt =
  QmlAstWalk.Top.fold_map_name_expr (name_anonymous_lambda_expr ~options) annotmap elt

(* depending on elt,
   build the node
   val bnds, or
   val rec bnds *)
let mk_val_rec elt bnds =
  match elt with
  | Q.NewVal (label, _) -> Q.NewVal (label, bnds)
  | Q.NewValRec (label, _) -> Q.NewValRec (label, bnds)
  | _ -> assert false

module G2 = Graph.Imperative.Digraph.Concrete(
  struct
    type t = Ident.t * Q.expr
    let compare (i1,_) (i2,_) = Ident.compare i1 i2
    let equal (i1,_) (i2,_) = Ident.equal i1 i2
    let hash (i,_) = Ident.hash i
  end)
module SCC2 = GraphUtils.Components.Make(G2)

(* given a list of list of bindings,
   return a list of list of val rec declarations
   that is correctly ordered *)
let split_and_reorder label bnds =
  let size = 10 (* use List.length bnds or something? *) in
  let g = G2.create ~size () in
  let map =
    List.fold_left
      (fun map bnds ->
         List.fold_left
           (fun map (x,e) ->
              let v_x = G2.V.create (x,e) in
              let fn_x = fn_of_expr e in
              G2.add_vertex g v_x;
              IdentMap.add x (v_x,fn_x) map)
           map
           bnds)
      IdentMap.empty
      bnds
  in
  IdentMap.iter
    (fun _x (v_x,fn_x) ->
       IdentSet.iter
         (fun y ->
            try
              let (v_y,_) = IdentMap.find y map in
              G2.add_edge g v_x v_y
            with Not_found -> ())
         fn_x)
    map;
  let scc = SCC2.scc ~size g in
  List.map
    (function
     | [v] ->
         let label = Annot.refresh label in
         if G2.mem_edge g v v then Q.NewValRec (label, [G2.V.label v])
         else Q.NewVal (label, [G2.V.label v])
     | vs ->
         let label = Annot.refresh label in
         Q.NewValRec (label, List.map G2.V.label vs))
    scc

(* lift function parameters *)
let lift_code_elt ~options (annotmap,env) elt =
  (* keeping the types but not the values in the gamma
   * since this gamma contains only local values
   * the types are necessary because this gamma is used for calls to TypedExpr *)
  let gamma = QmlTypes.Env.Ident.from_map IdentMap.empty env.gamma in
  match elt with
  | Q.NewVal (label, bnds)
  | Q.NewValRec (label, bnds) ->
      let (_,annotmap,({hoisted=hoisted} as env)),(funcs,vals) =
        parameterLiftBnds ~options ~toplevel:true (gamma,annotmap,env) bnds in
      let decs = List.rev_append funcs (vals :: hoisted) in
      let env = {env with funcs = IdentMap.empty} in (* this is not strictly necessary but it can
                                                      * hide some missing values in the gamma
                                                      * from the current compilation unit *)
      (annotmap,env), split_and_reorder label decs
  | _ ->
      let label = Q.Label.code_elt elt in
      let (_,annotmap,({hoisted=hoisted} as env)),elt =
        QmlAstWalk.Top.fold_map_expr
          (parameterLiftExp ~options) (QmlTypes.Env.empty,annotmap,env) elt in
      let bnds = split_and_reorder label hoisted in
      (annotmap,env), bnds @ [elt]

let process_code_elt ~options (annotmap,env) elt =
  let annotmap,elt = name_anonymous_lambda_code_elt ~options annotmap elt in
  let (annotmap,env),elts = lift_code_elt ~options (annotmap,env) elt in
  let env = {env with hoisted = []; hierarchy = []} in
  (annotmap,env),elts

let empty_env gamma = {
  gamma = gamma;
  funcs = IdentMap.empty;
  hoisted = [];
  hierarchy = [];
 }

(*
  this function doesn't check that the code is lambda lifted, it checks
  that (assuming the code is lambda lifted), it is _well_ lambda lifted:
  - no function is given a toplevel identifier as part of its environment
  - no function is given unused identifiers as part of its environment

  Note that this can't be a postcondition since no one cares about it, and
  to be able to check that, you need the environment used internally by the
  lambda lifting (otherwise, you can't make the difference between real
  parameters from the user code and the one added by the lambda lifting)
*)
let check_lambda_lifting _original_gamma env code =
  (* check that everything is in the gamma before lambda lifting
   * check that it is still true after (at least in typed mode) *)
  let {funcs=funcs; gamma=_new_gamma} = env in
  let toplevel_names = (* map to size of the env if lifted or 0 otherwise *)
    List.fold_left
      (fun map -> function
       | Q.NewVal (_, iel)
       | Q.NewValRec (_, iel) ->
           List.fold_left
             (fun map (i,e) ->
                let v =
                  try (** !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! **)
                    let (_,idents,_) = IdentMap.find i funcs in
                    let n = List.length idents in
                    (* checking that parameters introduced by the lambda lifting
                     * are used at least once *)
                    let idents, e =
                      match e with
                      | Q.Lambda (_, idents, e) -> idents, e
                      | _ -> [], e in
                    let idents = List.take n idents in
                    List.iteri
                      (fun ident index ->
                         if not (
                           QmlAstWalk.Expr.exists
                             (function
                              | Q.Ident (_, j) when Ident.equal ident j -> true
                              | _ -> false)
                             e
                         ) then (
                           Printf.printf "Toplevel ident %s has an env of size %d but doesn't use %s (%dth argument)\n%!" (Ident.to_string i) n (Ident.to_string ident) index;
                         )
                      ) idents;
                    n
                  with
                  | Not_found ->
                      (* FIXME?: could check that i is in the gamma, but then it
                       * would probably break and i wouldn't see anything... *)
                      0 in
                IdentMap.add i v map
             ) map iel
       | _ -> map
      ) IdentMap.empty code in
  let bounded_forall ident n f l =
    let rec aux m l =
      if m = n then
        true
      else
        match l with
        | [] ->
            Printf.printf "%s is not applied to its full env\n%!"
              (Ident.to_string ident); false
        | h :: t ->
            f h && aux (m + 1) t in
    aux 0 l in
  (* checking that when you apply a toplevel identifier, you don't give it
   * any toplevel identifiers *)
  let rec aux tra = function
    | Q.Apply (_, Q.Ident (_, i), args) ->
        (match IdentMap.find_opt i toplevel_names with
         | None -> ()
         | Some n ->
             ignore (
               bounded_forall i n
                 (function
                  | Q.Ident (_, j) ->
                      if IdentMap.mem j toplevel_names then (
                        Printf.printf "%s is given %s (which is a toplevel identifier) as part of its environment (size %d)\n%!" (Ident.to_string i) (Ident.to_string j) n;
                        false
                      ) else
                        true
                  | _ ->
                      Printf.printf "%s is given an expr for its env\n%!"
                        (Ident.to_string i);
                      false) args
             )
        );
        List.iter (fun e -> aux tra e) args
    | e -> tra e in
  QmlAstWalk.CodeExpr.iter
    (fun e -> QmlAstWalk.Expr.traverse_iter aux e)
    code

let process_code_gen ~mode gamma annotmap code =
  try
    let options = {mode} in
    let (annotmap,env),codes =
      List.fold_left_map (process_code_elt ~options) (annotmap,empty_env gamma) code in
    let code = List.concat codes in
    #<If:LAMBDA_CORRECT>check_lambda_lifting gamma env code#<End>;
    (env.gamma,annotmap),code
  with
  | Failure "safe_add" ->
      let context = QmlError.Context.annotmap annotmap in
      QmlError.cond_violation QmlAlphaConv.Check.alpha_id context "Failure %S" "safe_add"

(*-------------------------------------*)
(*------- normal lambda lifting -------*)
(*-------------------------------------*)
module MakeSaver(Name : sig val name : string end) =
  struct
    module S =
    struct
      type t = (QmlAst.ty, unit) QmlGenericScheme.tsc IdentMap.t
      let pass = Name.name
      let pp f _ = Format.pp_print_string f "<dummy>"
    end

    include ObjectFiles.MakeClientServer(S)
    let current_map ~input_gamma ~rebuilt_gamma ~output_gamma =
      IdentMap.diff2
        (QmlTypes.Env.Ident.to_map output_gamma)
        (QmlTypes.Env.Ident.to_map rebuilt_gamma)
        (QmlTypes.Env.Ident.to_map input_gamma)
    let load ~side gamma =
      fold_with_name ~side ~deep:true
        (fun package gamma map ->
           IdentMap.fold (fun ident tsc gamma ->
                            let tsc = QmlRefresh.refresh_typevars_from_tsc package tsc in
                            QmlTypes.Env.Ident.add ident tsc gamma) map gamma)
        gamma
  end
module R_early = MakeSaver(struct let name = "pass_EarlyLambdaLifting" end)
module R_late = MakeSaver(struct let name = "pass_LambdaLifting" end)
(* I know that can do it some other way with caml 3.12, but it won't be
   any shorter nor clearer *)
module R =
  struct
    let save ~early = if early then R_early.save else R_late.save
    let load ~early = if early then R_early.load else R_late.load
    let current_map = R_early.current_map
  end

let some_scheme = QmlTypes.Scheme.quantify Q.TypeAbstract

let process_code ~early ~side ~typed gamma annotmap code =
  let input_gamma = gamma in
  let filled_input_gamma =
    if typed then input_gamma
    else
      (* in untyped mode, the gamma may be missing a few values *)
      QmlAstWalk.CodeExpr.fold_names
        (fun gamma name ->
           QmlTypes.Env.Ident.add name some_scheme gamma)
        input_gamma
        code in
  let rebuilt_gamma = R.load ~early ~side filled_input_gamma in
  let (output_gamma, annotmap), code = process_code_gen ~mode:(if typed then `typed else `untyped) rebuilt_gamma annotmap code in
  let current_map = R.current_map ~input_gamma:filled_input_gamma ~rebuilt_gamma ~output_gamma in
  R.save ~early ~side current_map;
  let current_gamma = QmlTypes.Env.Ident.from_map current_map output_gamma in
  let returned_gamma = if typed then current_gamma else input_gamma in
  (returned_gamma, annotmap), code

(*-------------------------------------*)
(*----- fun action specific stuff -----*)
(*-------------------------------------*)
module S_fa =
struct
  type t = Ident.t list (* the toplevel names *)
  let pass = "funactionlifting"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R_fa = ObjectFiles.Make(S_fa)

let process_code_fun_action annotmap code =
  let client_set = ref (IdentSet.empty) in
  let public_set = ref (IdentSet.empty) in
  let gamma = (* this pass happens before typing so there is no gamma yet
               * so we just fill it with dummy types for all the toplevel values *)
    R_fa.fold
      (fun gamma names ->
         List.fold_left
           (fun gamma name ->
              QmlTypes.Env.Ident.add name some_scheme gamma) gamma names)
      QmlTypes.Env.empty in
  let these_names,gamma =
    QmlAstWalk.CodeExpr.fold_names
      (fun (these_names,gamma) name ->
         let these_names = name :: these_names in
         let gamma = QmlTypes.Env.Ident.add name some_scheme gamma in
         (these_names,gamma))
      ([],gamma)
      code in
  let (_gamma, annotmap), code = process_code_gen ~mode:(`fun_action (public_set,client_set)) gamma annotmap code in
  R_fa.save these_names;
  annotmap, code
