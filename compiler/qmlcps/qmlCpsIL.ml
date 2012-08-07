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
(**
   Intermediate language for CPS transformations.

   This language is based on "Compiling with Continuations, Continued",
   by Andrew Kennedy, in Proceedings of ICFP 2007.

   Note: In a near-future, the algebraic structure will be
   in part replaced by a graph structure, which permits
   numerous phases of optimization in O(n) rather than O(n^2)

   This file has no mli because it would just duplicate the IL ast.

   The file define a module IL because for convenience we write :

   IL.Continuation, IL.Bypass, etc... because of clash of constructor names
   between this module and the module QmlAst.

   We do not want to write the name of the module (QmlCpsIL),
   and neither do an 'open' leading to having constructors like Bypass in the
   scope.

   @author David Rajchenbach-Teller
   @author Mathieu Barbin
   @author Rudy Sicard
*)

(**
    TODO : if needed, the cps rewriter should return consistents gamma & annotmap
    The IL ast should be updated to support annotation
*)

module List = BaseList

module IL = struct
  type ident = Ident.t

  (** The name of a continuation.
      This may be replaced in the future by a (cident, ExprIdent.t) UnionFind.t
      where cident will be a black type. *)
  type cident = Continuation of ident

  (** The name of a value.
      Same remark, (vident, ExprIdent.t) UnionFind.t *)
  type vident = Value of (Annot.label * ident)

  (**The name of a field*)
  type fident = Field of string

  (** skey, restriction option.
      The restriction arg is used to distinguish between :
      - standard bypass   : {[QmlAst.Bypass key]}
        represented by : {[IL.Bypass (key, None)]}
      - restricted bypass : {[QmlAst.Directive (`restricted_bypass pass_id, [{ e = Bypass key }], _)|}
        the pass_id the string contained in the string option in this case :
        represented by : {[IL.Bypass (key, Some pass_id)]} *)
  type bypass = Bypass of (BslKey.t * string option)

  (* used to build stack traces *)
  type stack_trace_info =
      { caller_cont : cident option; (** continuation of the closest enclosing function if any *)
        callee_name : string option; (** local name of the function we're calling if any *)
        position : string option; (** position of the Apply if any *) }

  (* TODO : see if this is needed for later optimizations *)
  (*The type of a function definition*)
  (*
  type func =
      {return: cident;
       (**[None] if the function takes place outside of a transaction.*)
       transact: cident option;
  (*   (**[None] if the function is called from outside a request.*)
       request: cident option; *)
       arg: vident;
       term: term}
  and application =
      {app_called: vident;
       app_return: cident;
       app_transact: cident option;
       app_term
      }
  *)
  (* not used yet, either
  type application =
      {app_called: vident
      ;app_arg: vident
      ;app_return: cident
      ;app_transaction: cident option
      }
  *)

  type value =
    | Constant of QmlAst.const_expr
    | Record of (fident * vident * Annot.t) list
    | ExtendRecord of (fident * vident) list * vident
    | LazyRecord of (fident * vident * Annot.t) list * vident option
    | BasicBypass of bypass (* for value bypass only (not functions) *)
    | ValueSkip of QmlAst.expr

  and cps_function = CpsVident of vident | CpsBypass of bypass

  and term  =

    (**[LetVal("x", v, t)] represents [let x = v in t].
       At this stage, this is the only way of using a value in a term. *)
    | LetVal of vident * value * term

    (**[LetProj("x", ("y", "foo"), t)] represents [let x = y.foo in t].
       This is the only way of accessing a field. *)
    | LetProj of vident * (vident * fident) * term

    (**[LetCont(("k", "x", t, u), parent)] represents
       [let(*continuation*) k (x -> t) in u].
       This introduces a continuation [k] with one unique argument [x],
       to be used in [u].
       the parent is an option of the parent continuation id.

       <!> Assert: x MUST be fresh and appear ONLY in t
       or it would definitly break the substitution mechanism.
    *)
    | LetCont of ((cident * vident * term * term) * cident option)

    (**Introduce a set of mutually recursive definitions. *)
    | LetRecFun of (  vident      (** function name *)
                    * vident list (** arguments *)
                    * cident      (** the continuation *)
                    * term        (** body *)
                   ) list * term

    (**Introduce a set of non-recursive definitions. *)
    | LetFun of (vident * vident list * cident * term) list * term

    (**Apply a continuation to a value, e.g. to jump
       or return from a function. *)
    | ApplyCont of cident * vident

    (**[ApplyExpr(a,b,k)] computes [a] (which must be a rewrited-function)
       with argument [b] and continuation [k]
       The function [a] used to take arg [b] and return type c
       [k] is a 'c continuation, and function [a] has been rewritten to
       'b -> 'c continuation -> unit
    *)
    | ApplyExpr of cps_function * vident * cident

    (**
       Nary mode for application.
       TODO: remove ApplyExpr from the ast, keep only ApplyNary
    *)
    | ApplyNary of cps_function * vident list * cident * stack_trace_info option

    (**
       Special case of application of a non cps bypass, with all its arguments.
       Bypasses cannot be currified.
    *)
    | ApplyBypass of bypass * (vident list) * cident

    | Match of vident * (QmlAst.pat * term) list

    (**End of program, with result*)
    | Done of vident * string

    | Directive of
        (QmlAst.qml_directive * term list * QmlAst.ty list)

    (**In some cases, no operation, keep the qml 'as it is'*)
    (**Currently this node is unused*)
    | Skip of QmlAst.expr


  (** tools for code generation *)
  let fresh_c () = Continuation (Ident.next "cont")
  let fresh_v ?(label = Annot.nolabel "CpsIL.fresh_v") () =
    Value (label, Ident.next "val")

  let fresh_fun ?(label = Annot.nolabel "CpsIL.fresh_fun") () =
    Value (label, Ident.next "fun")

  let value ?(label = Annot.nolabel "CpsIL.value") v =
    Value (label, v)

end

(** {6 Traversal} *)
(** for optimisation and rewriting rules in IL *)
(** *)
module Subs : TraverseInterface.S2 with type 'a t = IL.term constraint 'a = _ * _ * _ =
struct
  open IL
  type 'a t = term constraint 'a = _ * _ * _

  let foldmap tra acc t =
    match t with
    | LetVal (vident, value, term) ->
        let acc, fterm = tra acc term in
        acc,
        if term == fterm then t else
          LetVal (vident, value, fterm)

    | LetProj (vident, (vident', fident), term) ->
        let acc, fterm = tra acc term in
        acc,
        if term == fterm then t else
          LetProj (vident, (vident', fident), fterm)

    | LetCont ((cident, vident, term, term'), cident') ->
        let acc, fterm = tra acc term in
        let acc, fterm' = tra acc term' in
        acc,
        if term == fterm &&  term' == fterm' then t else
          LetCont ((cident, vident, fterm, fterm'), cident')

    | LetRecFun (list, term) ->
        let foldmap acc ((vident, vident_list, cident, term) as t) =
          let acc, fterm = tra acc term in
          acc,
          if term == fterm then t else
            (vident, vident_list, cident, fterm) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        let acc, fterm = tra acc term in
        acc,
        if term == fterm && list == flist then t else
          LetRecFun (flist, fterm)

    | LetFun (list, term) ->
        let foldmap acc ((vident, vident_list, cident, term) as t) =
          let acc, fterm = tra acc term in
          acc,
          if term == fterm then t else
            (vident, vident_list, cident, fterm) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        let acc, fterm = tra acc term in
        acc,
        if list == flist && term == fterm then t else
          LetFun (flist, fterm)

    | ApplyCont (_cident, _vident) -> acc, t

    | ApplyExpr (_cps_function, _vident', _cident) -> acc, t

    | ApplyNary (_cps_function, _vident_list, _cident, _stack_infos) -> acc, t

    | ApplyBypass (_bypass, _vident_list, _cident) -> acc, t

    | Match (vident, pat_term_list) ->
        let foldmap acc ((pat, term) as t) =
          let acc, fterm = tra acc term in
          acc,
          if term == fterm then t else
            (pat, fterm) in
        let acc, fpat_term_list = List.fold_left_map_stable foldmap acc pat_term_list in
        acc,
        if pat_term_list == fpat_term_list then t else
          Match (vident, fpat_term_list)

    | Done (_vident, _string) -> acc, t

    | Directive (qml_directive, terms, tys) ->
        let acc, fterms = List.fold_left_map_stable tra acc terms in
        acc,
        if terms == fterms then t else
          Directive (qml_directive, fterms, tys)

    | Skip _expr -> acc, t

  let iter x = Traverse.Unoptimized.iter foldmap x
  let map x = Traverse.Unoptimized.map foldmap x
  let fold x = Traverse.Unoptimized.fold foldmap x
end

module Walk : TraverseInterface.TRAVERSE
  with type 'a t = IL.term constraint 'a = _ * _ * _
  and type 'a container = IL.term constraint 'a = _ * _ * _
      = Traverse.Make2 ( Subs )

(* TODO: replace this with the unionFind algorithm *)
module IdentWalk :
sig
  open IL
  val foldmap : ( 'acc -> ident -> 'acc * ident ) -> 'acc -> term -> 'acc * term
  val foldmap_up : ( 'acc -> ident -> 'acc * ident ) -> 'acc -> term -> 'acc * term
  val foldmap_down : ( 'acc -> ident -> 'acc * ident ) -> 'acc -> term -> 'acc * term
  val exists : ident -> value -> bool
end =
struct
  open IL

  let foldmap_vident tra acc ((Value (label, id)) as v) =
    let acc, fid = tra acc id in
    acc,
    if id == fid then v else
      Value (label, fid)

  let foldmap_cident tra acc ((Continuation id) as v) =
    let acc, fid = tra acc id in
    acc,
    if id == fid then v else
      Continuation fid

  let foldmap_skip tra acc e =
    QmlAstWalk.Expr.foldmap
      (fun acc e ->
         match e with
         | QmlAst.Ident (label, id) ->
             let acc, fid = tra acc id in
             acc,
             if id == fid then e else
               QmlAstCons.UntypedExprWithLabel.ident ~label fid
         | _  -> acc, e
      ) acc e

  let foldmap_cps_function tra acc cps =
    match cps with
    | CpsVident (Value (label, id)) ->
        let acc, fid = tra acc id in
        acc,
        if id == fid then cps else
          CpsVident (Value (label, fid))
    | _ -> acc, cps

  let foldmap_stack_info tra acc stack_info =
    match stack_info.caller_cont with
    | None -> acc, stack_info
    | Some cident ->
        let acc, fcident = foldmap_cident tra acc cident in
        acc,
        if cident == fcident then stack_info else {stack_info with caller_cont = Some fcident}

  let foldmap_ident_value tra acc t =
    match t with
    | Constant _ -> acc, t
    | Record list ->
        let foldmap acc ((fident, vident, annot) as t) =
          let acc, fvident = foldmap_vident tra acc vident in
          acc,
          if vident == fvident then t else
            (fident, fvident, annot) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        acc,
        if list == flist then t else
          Record flist
    | ExtendRecord (list, vident) ->
        let foldmap acc ((fident, vident (*, annot*)) as t) =
          let acc, fvident = foldmap_vident tra acc vident in
          acc,
          if vident == fvident then t else
            (fident, fvident(*, annot*)) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        let acc, fvident = foldmap_vident tra acc vident in
        acc,
        if list == flist && vident == fvident then t else
          ExtendRecord (flist, fvident)
    | LazyRecord (list, vident_option) ->
        let foldmap acc ((fident, vident, annot) as t) =
          let acc, fvident = foldmap_vident tra acc vident in
          acc,
          if vident == fvident then t else
            (fident, fvident, annot) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        let acc, fvident_option = Option.foldmap_stable (foldmap_vident tra) acc vident_option in
        acc,
        if list == flist && vident_option == fvident_option then t else
          LazyRecord (flist, fvident_option)
    | BasicBypass _ -> acc, t
    | ValueSkip e ->
        let acc, fe = foldmap_skip tra acc e in
        acc,
        if e == fe then t else
          ValueSkip fe

  let foldmap_ident tra acc t =
    match t with
    | LetVal (vident, value, term) ->
        let acc, fvident = foldmap_vident tra acc vident in
        let acc, fvalue = foldmap_ident_value tra acc value in
        acc,
        if vident == fvident && value == fvalue then t else
          LetVal (fvident, fvalue, term)

    | LetProj (vident, (vident', fident), term) ->
        let acc, fvident = foldmap_vident tra acc vident in
        let acc, fvident' = foldmap_vident tra acc vident' in
        acc,
        if vident == fvident && vident' == fvident' then t else
          LetProj (fvident, (fvident', fident), term)

    | LetCont ((cident, vident, term, term'), cident') ->
        let acc, fcident = foldmap_cident tra acc cident in
        let acc, fvident = foldmap_vident tra acc vident in
        let acc, fcident' = Option.foldmap_stable (foldmap_cident tra) acc cident' in
        acc,
        if cident == fcident && vident == fvident && cident' == fcident' then t else
          LetCont ((fcident, fvident, term, term'), fcident')

    | LetRecFun (list, term) ->
        let foldmap acc ((vident, vident_list, cident, term) as t)=
          let acc, fvident = foldmap_vident tra acc vident in
          let acc, fvident_list = List.fold_left_map_stable (foldmap_vident tra) acc vident_list in
          let acc, fcident = foldmap_cident tra acc cident in
          acc,
          if vident == fvident && vident_list == fvident_list && cident = fcident then t else
            (fvident, fvident_list, fcident, term) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        acc,
        if list == flist then t else
          LetRecFun (flist, term)

    | LetFun (list, term) ->
        let foldmap acc ((vident, vident_list, cident, term) as t) =
          let acc, fvident = foldmap_vident tra acc vident in
          let acc, fvident_list = List.fold_left_map_stable (foldmap_vident tra) acc vident_list in
          let acc, fcident = foldmap_cident tra acc cident in
          acc,
          if vident == fvident && vident_list == fvident_list && cident == fcident then t else
            (fvident, fvident_list, fcident, term) in
        let acc, flist = List.fold_left_map_stable foldmap acc list in
        acc,
        if list == flist then t else
          LetFun (flist, term)

    | ApplyCont (cident, vident) ->
        let acc, fcident = foldmap_cident tra acc cident in
        let acc, fvident = foldmap_vident tra acc vident in
        acc,
        if cident == fcident && vident == fvident then t else
          ApplyCont (fcident, fvident)

    | ApplyExpr (cps_function, vident, cident) ->
        let acc, fcps_function = foldmap_cps_function tra acc cps_function in
        let acc, fvident = foldmap_vident tra acc vident in
        let acc, fcident = foldmap_cident tra acc cident in
        acc,
        if cps_function == fcps_function && vident == fvident && cident == fcident then t else
          ApplyExpr (fcps_function, fvident, fcident)

    | ApplyNary (cps_function, vident_list, cident, stack_info_opt) ->
        let acc, fcps_function = foldmap_cps_function tra acc cps_function in
        let acc, fvident_list = List.fold_left_map_stable (foldmap_vident tra) acc vident_list in
        let acc, fcident = foldmap_cident tra acc cident in
        let acc, fstack_info_opt = Option.foldmap (foldmap_stack_info tra) acc stack_info_opt in
        acc,
        if cps_function == fcps_function && vident_list == fvident_list && cident == fcident && stack_info_opt == fstack_info_opt then t else
          ApplyNary (fcps_function, fvident_list, fcident, fstack_info_opt)

    | ApplyBypass (bypass, vident_list, cident) ->
        let acc, fvident_list = List.fold_left_map_stable (foldmap_vident tra) acc vident_list in
        let acc, fcident = foldmap_cident tra acc cident in
        acc,
        if vident_list == fvident_list && cident == fcident then t else
          ApplyBypass (bypass, fvident_list, fcident)

    | Match (vident, pat_term_list) ->
        let acc, fvident = foldmap_vident tra acc vident in
        acc,
        if vident == fvident then t else
          Match (fvident, pat_term_list)

    | Done (vident, string) ->
        let acc, fvident = foldmap_vident tra acc vident in
        acc,
        if vident == fvident then t else
          Done (fvident, string)

    | Directive (_qml_directive, _terms, _tys) -> acc, t

    | Skip e ->
        let acc, fe = foldmap_skip tra acc e in
        acc,
        if e == fe then t else
          Skip fe

  let foldmap tra = Walk.foldmap (foldmap_ident tra)
  let foldmap_up tra = Walk.foldmap_up (foldmap_ident tra)
  let foldmap_down tra = Walk.foldmap_down (foldmap_ident tra)

  let exists ident value =
    try
      ignore
        (foldmap_ident_value
           (fun () v ->
              if Ident.equal v ident then raise Exit;
              (), v) () value);
      false
    with Exit -> true

end


(** {6 Utils on IL} *)

#<Debugvar:CPS_VERBOSE>

let debug fmt =
  Printf.fprintf stderr ("[34m[Cps][0m "^^fmt^^"\n%!")

module Substitution :
sig
  open IL
  val ident : ident -> ident -> term -> term
end =
struct
  let ident ident ident' term =
    let subst v = if Ident.equal v ident then ident' else v in
    let foldmap () ident = (), subst ident in
    let _, term = IdentWalk.foldmap foldmap () term in
    term
end

module Factorize :
sig
  open IL

  (** letcont : factorize all occurrence of the form
      [LetCont (k, (x -> term), [ ApplyCont (k, y) ])]
      rewriting it so :
      [ <term> {x <- y} ]

      <!> The Optimized implementation uses a side-effect with the
      union find structure.
      assert : x is fresh and appears only in term.
  *)
  val letcont : term -> term

  (** debug : give the number of letcont simplified until now *)
  val count : unit -> int

  (** profile : give the global time taken doing substitution *)
  val chrono_subst : unit -> float
end =
struct
  open IL

  let counter = ref 0
  let count () = !counter

  let _chrono_subst = Chrono.make ()
  let chrono_subst () = Chrono.read _chrono_subst

  let letcont term =
    let rec map t =
      match t with
(* This case is unused, but is should be done somewhere. Keep this code until we find the correct way to do it

      | LetCont ((Continuation cident, Value vident, ApplyCont (Continuation cident', Value vident'), term), cident_option)
          when Ident.compare vident vident' = 0 ->

          (* we could eventually loose some thread_context, be sure that not before simplifying *)
          let authorized_simplification =
            match cident_option with
            | None -> true
            | Some (Continuation cident'') -> Ident.compare cident' cident'' = 0
          in
          if authorized_simplification
          then
            let _ =
              incr(counter);
              #<If:CPS_VERBOSE $minlevel 2>
                debug "#%d LetCont %s (%s -> ApplyCont %s %s) in <term>" (!counter)
                (Ident.stident cident) (Ident.stident vident)
                (Ident.stident cident') (Ident.stident vident')
                #<End> in

            let _ = #<If:CPS_VERBOSE $minlevel 1> Chrono.start _chrono_subst #<End> in
            let term = Substitution.ident cident cident' term in
            let _ = #<If:CPS_VERBOSE $minlevel 1> Chrono.stop _chrono_subst #<End> in
            map term
          else
            term
*)

      | LetCont ((Continuation cident, Value (_, vident), term, applycont), _) ->
          begin
            (* traversing local LetVal *)
            let rec find_applycont acc = function
              | ApplyCont (Continuation cident', (Value (_, vident'))) ->
                  if Ident.compare cident cident' <> 0
                  then None
                  else Some (acc, vident')
              | LetVal (vident, value, term) when not (IdentWalk.exists cident value) -> find_applycont ((vident, value)::acc) term
              | _ -> None
            in
            match find_applycont [] applycont with
            | None -> t
            | Some (letvals, vident') ->
                let _ =
                  incr(counter);
                  #<If:CPS_VERBOSE $minlevel 2>
                    debug "#%d LetCont %s (%s -> <term>) in ApplyCont %s %s" (!counter)
                    (Ident.stident cident) (Ident.stident vident)
                    (Ident.stident cident) (Ident.stident vident')
                  #<End> in

                (* this can be factorized <term> { vident <- vident' } *)
                (* Optimisation: once Vident are (vident, ExprIdent.t) UnionFind.t,
                   we can use : UnionFind.replace vident vident', but we need to be
                   sure that [term] is the only one containing occurrences of [vident] *)

                let _ = #<If:CPS_VERBOSE $minlevel 1> Chrono.start _chrono_subst #<End> in
                let term = Substitution.ident vident vident' term in
                let _ = #<If:CPS_VERBOSE $minlevel 1> Chrono.stop _chrono_subst #<End> in

                (* putting back local LetVal in the same order *)
                let term = List.fold_left (fun term (vident, const) -> LetVal (vident, const, term)) term letvals in
                map term
          end
      | _ -> t in
    Walk.map_down map term

end
