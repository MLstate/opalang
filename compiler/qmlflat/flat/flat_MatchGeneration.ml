(*
    Copyright © 2011 MLstate

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
(* CF mli *)

(* depends *)
module List = Base.List

(* refactoring in progress *)

(* alias *)
module ServerLib = Flat_Common.ServerLib
module FCons = Flat_Common.FCons

(* shorthand *)
module Q = QmlAst

let nopos = FilePos.nopos "MatchGeneration"
let next_label () = Annot.next_label nopos
type label = Annot.label

(**
   A private AST for trivial patterns.
*)
type trivial_pat =
  | T_PatConst of label * QmlAst.const_expr
  | T_PatVar of label * Ident.t
  | T_PatAny of label
  | T_PatAs of label * trivial_pat * Ident.t
  | T_PatOr of label * trivial_pat list

(**
   A private AST for closed pattern.
   PatRecord is for closed pattern matching, ie with None rowvar.
*)
type closed_pat =
  | PatRecord of label * (string * closed_pat) list
      (**
         Gathering all fields, and a final info for taging closed pattern.
         `open pattern are those with a Some rowvar, e.g. { a ; ... }
         Invariant: the list is sorted by fields. (lexicographic order)
      *)

  | PatConst of label * QmlAst.const_expr
  | PatVar of label * Ident.t
  | PatAny of label
  | PatAs of label * closed_pat * Ident.t

(**
   A flat for complex PatRecord.
*)
type row_flag = [`closed | `open_]

(**
   The complete private AST for representing patterns.
   The last list in M_PatRecord are PatAs.
*)
type mixed_pat =
  | M_PatRecord of label * (string * mixed_pat) list * row_flag * Ident.t list
  | M_ClosedPat of closed_pat

let assert_closed_fst (p, e) =
  match p with
  | M_ClosedPat p -> (p, e)
  | _ -> assert false

let assert_closed_snd (f, p) =
  match p with
  | M_ClosedPat p -> (f, p)
  | _ -> assert false

module Projection :
sig

  (**
     Project a qml pattern into a trivial pattern.
     As this projection may fail, the returned type is an option.
  *)
  val trivial : QmlAst.pat -> trivial_pat option

  (**
     The projection into complex pat cannot fail.
     We add a boolean for specifying if the tag `open_ was generated.
     If not, the complex_pat can magically be projected into a closed_pat
  *)
  val pat : QmlAst.pat -> mixed_pat
end =
struct
  exception Not_trivial
  let trivial p =
    let rec aux p =
      match p with
      | Q.PatRecord _ -> raise Not_trivial
      | Q.PatConst (label, const) ->
          T_PatConst (label, const)
      | Q.PatVar (label, ident) ->
          T_PatVar (label, ident)
      | Q.PatAs (label, pat, ident) ->
          let pat = aux pat in
          T_PatAs (label, pat, ident)
      | Q.PatAny label ->
          T_PatAny label
      | Q.PatCoerce (_, pat, _) -> aux pat
    in
    try
      Some (aux p)
    with
    | Not_trivial -> None

  let cmp (fa, _) (fb, _) = String.compare fa fb
  let sort tl = List.sort cmp tl

  let pat p =
    let rec aux p =
      match p with
      | Q.PatRecord (label, fields, rowvar) ->
          aux_record label fields rowvar
      | Q.PatConst (label, const) ->
          M_ClosedPat (PatConst (label, const))
      | Q.PatVar (label, ident) ->
          M_ClosedPat (PatVar (label, ident))
      | Q.PatAny label ->
          M_ClosedPat (PatAny label)
      | Q.PatCoerce (_, pat, _) -> aux pat
      | Q.PatAs (label, pat, alias) -> (
          match aux pat with
          | M_PatRecord (label, fields, rowvar, patas) ->
              M_PatRecord (label, fields, rowvar, alias :: patas)

          | M_ClosedPat pat ->
              let pat = PatAs (label, pat, alias) in
              M_ClosedPat pat
        )

    and aux_record label fields rowvar =
      let closed = ref true in
      let map (field, pat) =
        let pat = aux pat in
        let () =
          match pat with
          | M_PatRecord _ ->
              (*
                This one, or a sub pattern is not closed
              *)
              closed := false
          | _ -> ()
        in
        field, pat
      in
      let fields = List.map map fields in
      let fields = sort fields in
      match rowvar with
      | `open_ ->
          M_PatRecord (label, fields, `open_, [])
      | `closed ->
          if !closed
          then
            let fields = List.map assert_closed_snd fields in
            M_ClosedPat (PatRecord (label, fields))
          else
            M_PatRecord (label, fields, `closed, [])
    in
    aux p
end

module PatternAnalysis =
(* cf mli for sig *)
struct
  type 'pat matching = Ocaml.expr * ('pat * Ocaml.expr) list

  type trivial = trivial_pat matching
  type closed = closed_pat matching
  type mixed = mixed_pat matching

  type t =
    | Trivial of trivial
    | Closed of closed
    | Mixed of mixed

  (*
    - A matching is trivial only if all this patterns are trivial
    - A matching is closed only if all this patterns are closed
    - In any other case, the matching is complex.

    We deal with magic for efficiency (assert_closed)
  *)
  let analysis e pat_expr =
    let is_trivial =
      let rec aux acc = function
        | [] -> Some ( List.rev acc )
        | (pat, e) :: tl -> (
            match Projection.trivial pat with
            | Some trivial -> aux ((trivial, e) :: acc) tl
            | None -> None
          )
      in
      aux [] pat_expr
    in
    match is_trivial with
    | Some trivial ->
        Trivial (e, trivial)
    | None ->
        let is_closed = ref true in
        let map (pat, expr) =
          let mixed_pat = Projection.pat pat in
          let () =
            match mixed_pat with
            | M_PatRecord _ ->
                is_closed := false
            | M_ClosedPat _ ->
                ()
          in
          mixed_pat, expr
        in
        let list = List.map map pat_expr in
        if !is_closed
        then
          Closed (e, List.map assert_closed_fst list)
        else
          Mixed (e, list)
end

module TrivialGeneration :
sig
  (**
     With trival patterns, we do not need to modify the right-side production.
  *)
  val compile_pat : trivial_pat -> Ocaml.pattern
  val compile : PatternAnalysis.trivial -> Ocaml.expr
end =
struct
  let rec compile_pat = function
    | T_PatConst (_, const) -> (
        match const with
        | Q.Int i -> Ocaml.Cons.pat_int i
        | Q.Float i -> Ocaml.Cons.pat_float i
        | Q.String i -> Ocaml.Cons.pat_string i
      )

    | T_PatVar (_, ident) ->
        FCons.pat ident

    | T_PatAny _ ->
        Ocaml.PatAny

    | T_PatAs (_, pat, ident) ->
        let pat = compile_pat pat in
        FCons.patas pat ident

    | T_PatOr (_, pl)->
        let pl = List.map compile_pat pl in
        Ocaml.PatOr pl

  let compile (e, pat_expr) =
    let map (pat, e) = compile_pat pat, None, e in
    let pat_expr = List.map map pat_expr in
    Ocaml.Cons.make_match e pat_expr
end


module Guard :
sig
  (**
     Utils for guard.
     <!> We should systematically add magic for avoiding unification problems,
     because we use guard in the context of the matching of heterogenous arrays.
     <!> For field constant comparaison, there is a need to Lazy.force the variable,
     because the record may be a lazy record.

     e.g:
     {[
     | [| vt ; _ ; ma ; mb |] when
         Obj.magic vt == shared_vtable_35
      && Lazy.force (Obj.magic ma) = 5
     |}
  *)

  (**
     A condition, with a priority assignment, for sorting at end,
     and computing effecient test first (&& is lazy in Ocaml, so that
     makes an importance.)
  *)
  type t = int * Ocaml.expr

  (**
     The variable we want to test is in the left.
     That's where the Obj.magic is inserted.
  *)

  val equal : Ocaml.expr -> Ocaml.expr -> t
  val physical_equal : Ocaml.expr -> Ocaml.expr -> t

  val empty_record : Ocaml.expr -> t

  val conjonction : t list -> Ocaml.expr option
  val conjonction_opt : t option list -> Ocaml.expr option

  val expr : t -> Ocaml.expr
  val cond : Ocaml.expr -> t

  val band : t -> t -> t
  val band_opt : t -> t option -> t

end =
struct

  type t = int * Ocaml.expr

  let equal_priority = 1
  let physical_equal_priority = 0

  let equal a b =
    let cond = Ocaml.make_equals a b in
    equal_priority, cond

  let physical_equal a b =
    let cond = Ocaml.physical_equality a b in
    physical_equal_priority, cond

  let empty_record a =
    physical_equal a ServerLib.empty

  let cmp ((p : int), _) (p', _) = Pervasives.compare p p'
  let conjonction list =
    let list = List.sort cmp list in
    let fold cond (_, cond2) = Ocaml.Cons.band cond cond2 in
    match list with
    | [] -> None
    | (_, cond)::tl ->
        let cond = List.fold_left fold cond tl in
        Some cond

  let conjonction_opt list = conjonction (List.filter_map (fun e -> e) list)

  let expr = snd
  let cond e = 0, e

  let band (i, g) (j, h) = max i j, Ocaml.Cons.band g h
  let band_opt a b =
    match b with
    | Some b -> band a b
    | None -> a
end


module ClosedGeneration :
sig
  (**
     Aux function, for producing a single pattern, introducing all the correct
     variables, and making all checks about vtable
  *)

  (**
     Because of the inhomogenity of matched values, the generated code would not type if
     we do not re-introduce variables with a call to [Obj.magic] before the right-side production.

     For a couple [( v, mv )], where [v] is potentially used in the right-side production,
     and [mv] is introduced by the pattern, we will add at the end the following binding
     before the right-side production :
     {[
     let v = Lazy.force_eval (Obj.magic mv) in
     <right-side production>
     ]}

     Bindings can depends on binding previously introduced, so the order of returned binding is important.
     The returned list is in the order of introduction.
  *)
  type bindings = ( Ocaml.param_formel * Ocaml.expr ) list

  val deep_guard : Ocaml.expr -> closed_pat -> Guard.t option
  val deep_binds : Ocaml.expr -> closed_pat -> bindings

  val aux_compile_pat : closed_pat -> (Guard.t option * bindings) * Ocaml.pattern

  (**
     With closed patterns, we should modifiy the right-side production,
     by adding some let-binding for removing some type constraint introduced
     by the magic utilisation of arrays for representing flat-record.
     This is just for the ocaml typer, and should have no impact on the native
     code generated in fine.
  *)
  val compile_pat : (closed_pat * Ocaml.expr) -> (Ocaml.pattern * Ocaml.expr option * Ocaml.expr)

  val compile : PatternAnalysis.closed -> Ocaml.expr
end =
struct
  type bindings = ( Ocaml.param_formel * Ocaml.expr ) list

  (*
    Guard to be generated in a 'when' clause, for checking sub-patterns.
  *)
  let rec deep_guard var = function

    (* empty record *)
    | PatRecord (_, []) ->
        let var = Ocaml.make_magic var in
        let guard = Guard.empty_record var in
        Some guard

    (* simple record *)
    | PatRecord (_, [field, PatRecord (_, [])]) ->
        let shared_simple = Flat_Shared.simple field in
        let var = Ocaml.make_magic var in
        let guard = Guard.physical_equal var shared_simple in
        Some guard

    (* complex record *)
    | PatRecord (_, fields) -> (
        let labels, _ = List.split fields in
        let shared_vtable = Flat_Shared.vtable labels in
        let var = Ocaml.make_magic var in
        let vtable = Ocaml.Cons.app2 ServerLib.get_vtable var in
        let vtable_guard = Guard.physical_equal vtable shared_vtable in
        let guards, binds = List.fold_left_i (
          fun (guards, binds) (field, pat) i ->
            let f = Ident.next field in
            let f_param, f_var = FCons.param_var f in
            let expr = Ocaml.Cons.app3 ServerLib.unsafe_get (Ocaml.Cons.int i) var in
            let binds, guards =
              match deep_guard f_var pat with
              | Some guard ->
                  let binds = (f_param, expr) :: binds in
                  let guards = guard :: guards in
                  binds, guards
              | None ->
                  binds, guards
            in
            (guards, binds)
        ) ( ([] : Guard.t list ),
            ([] : (Ocaml.param_formel * Ocaml.expr) list)
          ) fields
        in
        match Guard.conjonction guards with
        | None -> Some vtable_guard
        | Some ( guard : Ocaml.expr ) ->
            let guard = List.fold_left (fun acc (id, expr) -> Ocaml.Letin ([id, expr], acc)) guard binds in
            let guard = Guard.band vtable_guard (Guard.cond guard) in
            Some guard
      )

    (* constant *)
    | PatConst (_, const) ->
        let var = Ocaml.make_magic var in
        let guard = Guard.equal var (Ocaml.Const (FCons.const const)) in
        Some guard

    (* var *)
    | PatVar _ ->
        None

    (* any *)
    | PatAny _ ->
        None

    (* as *)
    | PatAs (_, pat, _) ->
        deep_guard var pat

  (*
    unsafe get for rebinding variables from nested PatVar patterns.
  *)
  let rec deep_binds var = function

    (* empty record *)
    | PatRecord (_, []) ->
        []

    (* simple record *)
    | PatRecord (_, [_, PatRecord (_, [])]) ->
        []

    (* other closed record *)
    | PatRecord (_, fields) ->
        let binds = List.mapi (
          fun i (field, pat) ->
            let var = Ocaml.make_magic var in
            let expr = Ocaml.Cons.app3 ServerLib.unsafe_get (Ocaml.Cons.int i) var in
            let binds =
              match pat with
              | PatVar (_, ident) ->
                  let f_param = FCons.param ident in
                  let binds = [f_param, expr] in
                  binds
              | _ -> (
                  let f = Ident.next field in
                  let f_param, f_var = FCons.param_var f in
                  match deep_binds f_var pat with
                  | [] -> []
                  | binds ->
                      let binds = (f_param, expr) :: binds in
                      binds
                )
            in
            binds
        )
          fields
        in
        let binds = List.tail_concat binds in
        binds

    | PatConst _ ->
        []

    | PatAs (_, pat, ident) ->
        let bind =
          let param = FCons.param ident in
          let magic = Ocaml.make_magic var in
          param, magic
        in
        let binds = deep_binds var pat in
        bind :: binds

    | PatVar (_, ident) ->
        let bind =
          let param = FCons.param ident in
          let magic = Ocaml.make_magic var in
          param, magic
        in
        [bind]

    | PatAny _ ->
        []

  (*
    aux_compile_pat
    let (guard, rev_bindings), pat = aux_compile_pat pat in
  *)
  let aux_compile_pat pat =
    let deep ident =
      let f_pat, f_var = FCons.pat_var ident in
      let guard = deep_guard f_var pat in
      let binds = deep_binds f_var pat in
      (guard, binds), f_pat
    in

    match pat with
    | PatRecord (_, []) -> deep (Ident.next "empty")

    | PatRecord (_, [ field, PatRecord (_, [])]) -> deep (Ident.next field)

    | PatConst _ -> deep (Ident.next "const")

    | PatAs (_, _, ident)
    | PatVar (_, ident) ->
        let fident = Ident.refresh ident in
        deep fident

    | PatAny _ ->
        (None, []), Ocaml.PatAny

    | PatRecord (_, fields) ->
        let labels, _ = List.split fields in
        let shared_vtable = Flat_Shared.vtable labels in
        let vt = Ident.next "vt" in
        let pat, var = FCons.pat_var vt in
        let var = Ocaml.make_magic var in
        let guard = Guard.physical_equal var shared_vtable in
        let (guard, binds), pats =
          let foldmap (guard, binds) (field, pat) =
            let f = Ident.next field in
            let f_pat, f_var = FCons.pat_var f in
            let any = ref true in
            let f_pat = Ocaml.PatLazy f_pat in
            let guard =
              let d_guard = deep_guard f_var pat in
              let () = if Option.is_some d_guard then any := false in
              Guard.band_opt guard d_guard
            in
            let binds =
              let d_binds = deep_binds f_var pat in
              let () = if d_binds <> [] then any := false in
              List.append binds d_binds
            in
            let f_pat = if !any then Ocaml.PatAny else f_pat in
            (guard, binds), f_pat
          in
          List.fold_left_map foldmap (guard, []) fields
        in
        let patarray = pat :: Ocaml.PatAny :: pats in
        let final_pat = Ocaml.PatArray patarray in
        let guard = Some guard in
        (guard, binds), final_pat

  let compile_pat (pat, e) =
    let (guard, bindings), pat = aux_compile_pat pat in
    let fold acc (t, magic) = Ocaml.Letin ([t, magic], acc) in
    let right_side = List.fold_left fold e (List.rev bindings) in
    let guard = Option.map Guard.expr guard in
    pat, guard, right_side

  let rec is_last_pattern = function
    | Ocaml.PatAny
    | Ocaml.PatVar _ -> true
    | Ocaml.PatAs (pat, _) ->
        is_last_pattern pat
    | _ -> false

  let compile (e, pat_expr) =
    (* we will match a record against an array *)
    let e = Ocaml.Cons.app ServerLib.unwrap_record e in
    (* clean-up : filter unused patterns after the last one. *)
    let pat_expr = List.map compile_pat pat_expr in
    let pat_expr =
      let rec aux acc = function
        | [] -> List.rev acc
        | ((pat, guard, _) as hd) :: tl ->
            if Option.is_some guard
            then aux (hd::acc) tl
            else (
              if is_last_pattern pat
              then
                (* skip tl *)
                List.rev (hd::acc)
              else
                aux (hd::acc) tl
            )
      in
      aux [] pat_expr
    in
    Ocaml.Cons.make_match e pat_expr
end

module MixedGeneration :
sig

  (**
     Yet subclasses of patterns, even more precise than complex_pat.
  *)
  type mixed_analysed_pat_expr

  (**
     Sub analysis.
  *)
  val analysis : ( mixed_pat * Ocaml.expr ) list -> mixed_analysed_pat_expr list

  (**
     Compilation of a list of analysed_pat_expr.
  *)
  val compile_aux :
    matched:Ocaml.expr ->
    mixed_analysed_pat_expr list ->
    Ocaml.expr

  (**
     Main mixed compilation.
  *)
  val compile : PatternAnalysis.mixed -> Ocaml.expr
end =
struct

  type singlefail_pat = mixed_pat

  (**
     This type can be extended for some more optimizations.
  *)
  type open_analysed_pat_expr = [

  (**
     Not a pattern matching.
     Patterns generated without closure introduction for sharing fail case,
     because failure code is produced only once.
  *)
  | `singlefail of singlefail_pat * Ocaml.expr

  (**
     Not a pattern matching.
     Patterns generated with a closure introduction for sharing fail case,
     because failure code is produced several time.
  *)
  | `multiplefail of mixed_pat * Ocaml.expr
  ]

  type mixed_analysed_pat_expr =
    (**
       Regrouping closed cases, because we generate a part of a match for that
    *)
    | Closed of ( closed_pat * Ocaml.expr ) list (* HdList.t if needed *)

    (**
       Complex cases. Not compiled as pattern matching.
    *)
    | Complex of open_analysed_pat_expr

  let rec take_closed acc = function
    | ((M_ClosedPat closed_pat), e)::tl ->
        take_closed ((closed_pat, e)::acc) tl
    | ( tl : ( mixed_pat * Ocaml.expr ) list ) ->
        List.rev acc, tl

  let analysis pat_expr =
    let rec aux acc = function
      | [] ->
          List.rev acc

      | ((M_ClosedPat closed_pat), e)::tl ->
          let closed, tl = take_closed [closed_pat, e] tl in
          let closed = Closed closed in
          aux (closed :: acc) tl

      | ((M_PatRecord (_, fields, row_var, _)) as complex_pat, e)::tl ->
          let complex =
            let complex =
              (* a singlefail patttern is a open record with 1 closed pat field *)
              match fields, row_var with
              | [ _, M_ClosedPat _ ], `open_ ->
                  `singlefail (complex_pat, e)
              | _ ->
                  `multiplefail (complex_pat, e)
            in
            Complex complex
          in
          aux (complex :: acc) tl
    in
    aux [] pat_expr

  (* shared cases between singlefail and multiplefail *)

  (*
     Not a pattern matching.
     Compiling an sub-analysed open pattern.
  *)

  let compile_pat ~matched ~failure (pat, expr) =
    let rec aux matched success = function
      | M_PatRecord (_, fields, `open_, patas) ->
          let fold (f, pat) success =
            let id = Ident.next f in
            let pat_id, var_id = FCons.pat_var id in
            (* possible optim: if the subpat pat is closed *)
            let subpat, guard, right =
              match pat with
              | M_ClosedPat closed_pat -> (
                  match closed_pat with
                  | PatVar (_, ident) ->
                      (* since we use dot_opt which returns 'a, no need for more magic *)
                      FCons.pat ident, None, success
                  | _ ->
                      ClosedGeneration.compile_pat (closed_pat, success)
                )
              | _ ->
                  let right = aux var_id success pat in
                  pat_id, None, right
            in
            let pat_some = Ocaml.Cons.pat_some subpat in
            let patterns = [
              pat_some, guard, right ;
              Ocaml.PatAny, None, failure ;
            ] in
            (* FIXME: replace by DotGeneration, for cache optimizations *)
            let dot = Ocaml.Cons.app3 ServerLib.dot_opt (Flat_Shared.field f) matched in
            Ocaml.Cons.make_match dot patterns
          in
          let return = List.fold_right fold fields success in
          let fold return ident =
            let param = FCons.param ident in
            Ocaml.Letin ([param, matched], return)
          in
          List.fold_left fold return patas

      | M_PatRecord (_, fields, `closed, patas) -> (
          (*
            No need to match fields = [], because this case would have been
            marked as closed pattern.
            We can add here an assert (fields <> []) during dev period. (remove later)
          *)
          assert (fields <> []) ;
          (* generation of identifiers *)
          let idents = List.map (fun ((f, _) as p) -> Ident.next f, p) fields in
          (* submatching *)
          let fold (id, (_, pat)) success =
            let var_id = FCons.var id in
            aux var_id success pat
          in
          let success = List.fold_right fold idents success in
          (* generation of the pattern, reusing code of closed_record *)
          let varfields = List.map (
            fun (id, (f, pat)) ->
              let pat =
                match pat with
                | M_ClosedPat (PatAny _ as patany) ->
                    (* lighter: with patany, do not introduce any var *)
                    patany
                | _ ->
                    PatVar (next_label(), id)
              in
              (f, pat)
          ) idents in
          let pat_success = PatRecord (next_label(), varfields) in
          let else_ = PatAny (next_label()) in
          let return =
            ClosedGeneration.compile
              (matched, [
                 pat_success, success ;
                 else_, failure ;
               ])
          in
          let fold return ident =
            let param = FCons.param ident in
            Ocaml.Letin ([param, matched], return)
          in
          List.fold_left fold return patas
        )

      | M_ClosedPat closed_pat -> (
          (* some simplification cases, lighter than a match *)
          match closed_pat with
          | PatRecord (_, []) ->
              let _, cond = Guard.empty_record (Ocaml.make_magic matched) in
              Ocaml.Cond (cond, success, failure)
          | PatConst (_, const) ->
              let cond = Ocaml.make_equals matched (Ocaml.Const (FCons.const const)) in
              Ocaml.Cond (cond, success, failure)

          | PatVar (_, ident) ->
              let param = FCons.param ident in
              Ocaml.Letin ([param, matched], success)

          | PatAny _ ->
              success

          (* generic case, rematch *)
          | closed_pat ->
              let else_ = PatAny (next_label()) in
              ClosedGeneration.compile (
                matched, [
                  closed_pat, success ;
                  else_, failure ;
                ])
        )
    in
    aux matched expr pat

  let compile_aux ~matched analysed =
    (* duplication of matched value with a let *)
    let rec aux = function
      | [] -> Ocaml.Cons.app ServerLib.runtime_error (Ocaml.Cons.string "todo match failure with position")
      | hd::tl -> (
          match hd with
          | Closed pat_expr_list ->
              let final_e = aux tl in
              let final_pat = PatAny (next_label()) in
              ClosedGeneration.compile (matched, (pat_expr_list @ [(final_pat, final_e)]))

          | Complex complex -> (
              match complex with
              | `singlefail pat_expr ->
                  let failure = aux tl in
                  compile_pat ~matched ~failure pat_expr

              | `multiplefail pat_expr ->
                  (* we need to share failure because it is generated more than once *)
                  let failure = Ident.next "failure" in
                  let fail_param, failure_var = FCons.param_var failure in
                  let failure = Ocaml.Cons.app failure_var Ocaml.Cons.unit in
                  let fail_case = aux tl in
                  let fail_abs = Ocaml.Abs ( [Ocaml.Pat (Ocaml.PatAny)], fail_case ) in
                  let rest = compile_pat ~matched ~failure pat_expr in
                  Ocaml.Letin ([ fail_param, fail_abs], rest)
            )
        )
    in
    aux analysed

  let compile (matched, pat_expr) =
    let analysed = analysis pat_expr in
    match matched with
    | Ocaml.Var (Ocaml.Pated _) ->
        compile_aux ~matched analysed
    | _ ->
        let ident = Ident.next "matched" in
        let param, var = FCons.param_var ident in
        let value = compile_aux ~matched:var analysed in
        Ocaml.Letin ([ param, matched ], value)
end

(* sugar *)
let compile matched pat_expr =
  match PatternAnalysis.analysis matched pat_expr with
  | PatternAnalysis.Trivial trivial -> TrivialGeneration.compile trivial
  | PatternAnalysis.Closed closed -> ClosedGeneration.compile closed
  | PatternAnalysis.Mixed mixed -> MixedGeneration.compile mixed
