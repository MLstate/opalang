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

(* depends *)
module Format = Base.Format
module Hashtbl = Base.Hashtbl
module List = Base.List

(* alias *)
module Array = Base.Array
module Common = Imp_Common
module FieldSet = StringSet
module FieldMap = StringMap
module PatternAnalysis = Imp_PatternAnalysis
module TypeVar = QmlTypeVars.TypeVar

(* shorthands *)
module Q = QmlAst

(* -- *)

(* type alias *)
type field = string
type path = field list
type rev_path = field list
type rev_prefix_path = field list
type to_dot = int StringMap.t
type sum_ident = int
type sum_index = int

let compare_path a b = List.make_compare String.compare a b

module SumCondition =
struct
  (**
     A type for controling field addition into a tree.
     The standard life of the tag for a tree is the following:
     -it starts [Open 0]
     -as long as fields are added to an open tree, the tree remains open,
     and grows up [Open x++]
     -a [size] guard may be added to a tree taged as [Open n], where [n <= size],
     is this case, the tag becomes :
     [Partial (n, size)] if [n < size], or [Closed size] if [n = size].
     Any other case leads to an inconsistency.
     -as long as fields are added to a partial tree, the tree remains partial,
     and grows up [Partial (current++, size)] until the limit of [size] is reached,
     in this last case, the tree becomes closed, [Closed size].

     An invariant is that the size of the tree (the FieldMap) is equal to the [current]
     value stored in the tag.
     [Node ((Open n | Partial (n, _) | Closed n), tree) => size(tree) = n]
  *)
  type tree_tag =
    | Open of int
        (**
           [Open i] means that the tree has currently [i] present fields,
           and there is yet no guard about the size of the tree.
        *)

    | Partial of int * int
        (**
           [Partial (current, size)] means that the tree has currently [current]
           present fields, and there is a guard about the [size].
           In this case, an invariant is [0 <= current < size]
        *)

    | Closed of int
        (**
           [Closed size] means that the tree contains already all its field.
           Not any further fields may be added to the tree (inconsistency).
        *)

  module NodeTag =
  struct
    let is_open = function
      | Open _ -> true
      | _ -> false
  end

  type node =
    | Node of tree
    | Abs
    | PConst of QmlAst.const_expr
  and tree = {
    tag : tree_tag ;
    map : node FieldMap.t ;
  }

  type check =
    | Field of bool
    | Const of QmlAst.const_expr
    | Size of int

  type decision =
    | True
    | Check of check * rev_path
    | And of decision list

  type negation_item =
    | Decision of decision
    | NotSumCase of sum_ident * sum_index

  type negation = negation_item list

  let empty_tree = { tag = Open 0 ; map = FieldMap.empty }
  let is_empty_tree t = t == empty_tree

  let present rev_path = Check (Field true, rev_path)
  let absent rev_path = Check (Field false, rev_path)
  let check_field present rev_path = Check (Field present, rev_path)
  let const rev_path const = Check (Const const, rev_path)
  let size rev_path size = Check (Size size, rev_path)

  (*
    A type for representing negation of indexed sum.
    The index of the map is the sum ident,
    the values in the set are the index already invalided.
  *)
  type sum_case_negation = IntSet.t IntMap.t

  let add_sum_case_negation (ident : sum_ident) (index : sum_index) map =
    let set = Option.default IntSet.empty (IntMap.find_opt ident map) in
    let set = IntSet.add index set in
    IntMap.add ident set map

  (*
    so that t is private for optimized is_empty test,
    ensuring with a typing check
  *)
  module T :
  sig
    type t = private {
      tree : tree ;
      (**
         The tree representing the condition
      *)

      sum_case_negation : sum_case_negation ;
      (**
         Enriched only by the negation function.
         This are extra informations for keeping in mide invalided patterns.
      *)

      implications : (tree * negation) list ;
      (**
         Implication algebra, list of pending implications.
         Invariant:
         for any [(_, d)] in [implications], we have [not (tree => d)]
         The operation of normalization will add in the tree all decisions
         associated to a condition implied by the main tree
      *)
    }
    val empty : t
    val cons : tree -> (tree * negation) list -> t
    val full_cons : tree -> sum_case_negation -> (tree * negation) list -> t
    val tree : tree -> t
  end =
  struct
    type t = {
      tree : tree ;
      sum_case_negation : sum_case_negation ;
      implications : (tree * negation) list ;
    }
    let empty = {
      tree = empty_tree ;
      sum_case_negation = IntMap.empty ;
      implications = [] ;
    }
    let cons tree implications =
      if is_empty_tree tree && implications = []
      then
        empty
      else {
        tree ;
        sum_case_negation = IntMap.empty ;
        implications ;
      }
    let full_cons tree sum_case_negation implications =
      if is_empty_tree tree && implications = [] && IntMap.is_empty sum_case_negation
      then
        empty
      else {
        tree ;
        sum_case_negation ;
        implications ;
      }
    let tree tree =
      if is_empty_tree tree
      then
        empty
      else {
        tree ;
        sum_case_negation = IntMap.empty ;
        implications = [] ;
      }
  end

  let empty = T.empty
  let is_empty e = e == empty

  type t = T.t
  type implication = t * negation

  let pp_tree_tag fmt = function
    | Open i -> Format.fprintf fmt "<open:%d>" i
    | Partial (i, j) -> Format.fprintf fmt "<partial:%d/%d>" i j
    | Closed i -> Format.fprintf fmt "<closed:%d>" i

  let rec pp_tree_map fmt t =
    if FieldMap.is_empty t then Format.pp_print_string fmt "<empty>" else
      let sep = ref false in
      FieldMap.iter (
        fun key node ->
          (if !sep then Format.fprintf fmt "@\n" ; sep := true) ;
          Format.fprintf fmt "@[<2>%S:%a@]" key pp_node node
      ) t
  and pp_tree fmt t = pp_node fmt (Node t)
  and pp_node fmt = function
    | Node t ->
        Format.fprintf fmt "Node %a@\n%a" pp_tree_tag t.tag pp_tree_map t.map
    | Abs ->
        Format.pp_print_string fmt "Abs"
    | PConst c ->
        Format.fprintf fmt "PConst(%a)" QmlPrint.pp#const c

  let pp_check fmt = function
    | Field b -> Format.pp_print_string fmt (if b then "present" else "absent")
    | Const c -> QmlPrint.pp#const fmt c
    | Size i -> Format.fprintf fmt "size:%d" i

  let rec pp_decision fmt = function
    | True -> Format.pp_print_string fmt "True"
    | Check (check, rev_path) ->
        Format.fprintf fmt "Check (%a, [ %a ])" pp_check check Common.pp_path rev_path
    | And decs ->
        Format.fprintf fmt "And [ %a ]" (Format.pp_list " ; " pp_decision) decs

  let pp_negation_item fmt = function
    | Decision decision -> pp_decision fmt decision
    | NotSumCase (ident, index) -> Format.fprintf fmt "NotSumCase(%d[%d])" ident index

  let pp_negation fmt negation = Format.pp_list " / " pp_negation_item fmt negation

  let pp_implication_tree fmt (tree, neg) =
    Format.fprintf fmt "@[<2>(@\n%a@\n =>@\n%a@]@\n)" pp_tree tree pp_negation neg

  let pp_implication fmt (t, d) = pp_implication_tree fmt (t.T.tree, d)

  let pp fmt t =
    if t.T.implications = [] && IntMap.is_empty t.T.sum_case_negation then pp_tree fmt t.T.tree else
    Format.fprintf fmt (
      "{@[<2>@\n@[<2>@{<bright>tree@}:@\n%a@]@\n"^^
      "@[<2>@{<bright>implications@}:@\n%a@]@\n"^^
      "@[<2>@{<bright>sum_case_neg@}:@\n%a@]@\n@]}"
    )
      pp_tree t.T.tree
      (Format.pp_list ";@\n" pp_implication_tree) t.T.implications
      (IntMap.pp ", " (
         fun fmt key set ->
           Format.fprintf fmt "%d[%a]" key (IntSet.pp "," Format.pp_print_int) set
       )
      ) t.T.sum_case_negation

  exception Inconsistency

  let is_conjonction = function
    | And _ -> true
    | _ -> false

  let flatten list =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl -> (
          let acc =
            match hd with
            | And decs ->
                List.rev_append decs acc
            | True -> acc
            | _ -> hd :: acc
          in
          aux acc tl
        )
    in
    aux [] list

  let conjonction list =
    let conj =
      match list with
      | [] -> True
      | [ hd ] -> hd
      | _ -> (
          match flatten list with
          | [] -> True
          | list -> And list
        )
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumCondition.conjonction">
        OManager.printf "@[<2>@{<bright>conjonction@} [ %a ] ==>@\n%a@]@\n@." (Format.pp_list " ; " pp_decision) list pp_decision conj
      #<End>
    in
    conj

  let lief check =
    match check with
    | Field present ->
        if present
        then
          Node empty_tree
        else
          Abs
    | Const c ->
        PConst c
    | Size size ->
        Node { tag = Partial (0, size) ; map = FieldMap.empty }

  let add_path ~check t path =
    let inconsistency () =
      let () =
        #<If:JS_MATCH_COMPILATION $contains "inconsistency">
          OManager.printf (
            "@{<bright>INCONSISTENCY@} ~check:%a t %a@\n@[<2>where t is:@\n%a@]@\n@[<2>Inconsistency@]@\n@."
          )
          pp_check check Common.pp_path path
          pp_tree t
          #<End>
      in
      raise Inconsistency
    in
    let incr_tag = function
      | Open i -> Open (succ i)
      | Partial (i, size) ->
          let i = succ i in
          if i > size
          then
            inconsistency ()
          else
            if i = size
            then
              Closed size
            else
              Partial (i, size)
      | Closed _ ->
          inconsistency ()
    in
    let already_in = ref false in
    let rec aux rev_path t = function
      | [] -> (
          match check with
          | Size size -> (

              match t.tag with
              | Open i ->
                  if i > size
                  then
                    inconsistency ()
                  else
                    let tag =
                      if i = size
                      then
                        Closed i
                      else
                        Partial (i, size)
                    in
                    { t with
                        tag ;
                    }

              | Partial (_, closed) | Closed closed ->
                  if size <> closed
                  then
                    inconsistency ()
                  else (
                    already_in := true ;
                    t
                  )
            )

          | _ ->
              (*
                This is an internal error, it correspond to a empty rev_path
                with a presence or const check:
               {[
                Check (present, [ ]), Check (Const "5", [ ])
               ]}
                This is not handled by this function, but would be
              *)
              assert false
        )

      | [ hd ] -> (
          match FieldMap.find_opt hd t.map with
          | Some node -> (
              match node with
              | Node tree -> (
                  let tag = tree.tag and map = tree.map in
                  match check with
                  | Field present ->
                      if present
                      then (
                        already_in := true ;
                        t
                      )
                      else
                        inconsistency ()
                  | Const c ->
                      if is_empty_tree tree
                      then
                        { t with
                            map = FieldMap.add hd (PConst c) t.map ;
                        }
                      else
                        inconsistency ()
                  | Size size -> (
                      match tag with
                      | Open i ->
                          if i > size
                          then
                            inconsistency ()
                          else
                            let hd_tag =
                              if i = size
                              then
                                Closed i
                              else
                                Partial (i, size)
                            in
                            { t with
                                map = FieldMap.add hd (Node { tag = hd_tag ; map }) t.map ;
                            }
                      | Partial (_, closed) | Closed closed ->
                          if size <> closed
                          then
                            inconsistency ()
                          else (
                            already_in := true ;
                            t
                          )
                    )
                )
              | Abs -> (
                  match check with
                  | Field false ->
                      already_in := true ;
                      t
                  | _ ->
                      inconsistency ()
                )
              | PConst c -> (
                  match check with
                  | Field true ->
                      already_in := true ;
                      t
                  | Const c2 ->
                      if QmlAstUtils.Const.equal c c2
                      then (
                        already_in := true ;
                        t
                      )
                      else
                        inconsistency ()
                  | _ ->
                      inconsistency ()
                )
            )
          | None -> (
              (*
                We are adding a totaly new hd-tree, we should be starting by building it.
              *)
              let hd_tree = lief check in
              (*
                Then, in any case (but the adding of the absence of hd), we should
                check the consistency of tag/
              *)
              let new_tag =
                match check with
                | Field false ->
                    (*
                      We are adding the absence of a field, the tag remains the same
                    *)
                    t.tag
                | _ -> incr_tag t.tag
              in
              {
                tag = new_tag ;
                map = FieldMap.add hd hd_tree t.map ;
              }
            )
        )
      | hd :: tl -> (
          let rev_path = hd :: rev_path in
          let hd_tree, do_incr_tag =
            match FieldMap.find_opt hd t.map with
            | Some (Node tree) ->
                tree, false
            | Some Abs | Some (PConst _) ->
                inconsistency ()
            | None ->
                empty_tree, true
          in
          let hd_tree = aux rev_path hd_tree tl in
          {
            tag = if do_incr_tag then incr_tag t.tag else t.tag ;
            map = FieldMap.add hd (Node hd_tree) t.map ;
          }
        )
    in
    let res = aux [] t path in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumCondition.add_path">
        OManager.printf (
          "@{<bright>add_path@} ~check:%a t %a@\n@[<2>where t is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        pp_check check Common.pp_path path
        pp_tree t
        pp_tree res
      #<End>
    in
    !already_in, res

  let add_tree d t =
    let rec aux t d =
      match d with
      | True -> t
      | And list ->
          List.fold_left aux t list
      | Check (check, rev_path) ->
          snd (add_path ~check t (List.rev rev_path))
    in
    let res = aux t d in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumCondition.add">
        OManager.printf (
          "@{<bright>add@} d t@\n@[<2>where @{<bright>d@} is:@\n%a@]@\n@[<2>and @{<bright>t@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        pp_decision d
        pp_tree t
        pp_tree res
      #<End>
    in
    res

  (*
    ( a => b ) <=> ( b included in a )
  *)
  let implies_tag a b =
    match a, b with
    | Open a, Open b -> a >= b
    | Open _, _ -> false
    | Partial (a, _), Open b -> a >= b
    | Partial (a, sa), Partial (b, sb) -> sa = sb && a >= b
    | Partial _, _ -> false
    | Closed a, Open b -> a >= b
    | Closed a, Partial (_, b) -> a = b
    | Closed a, Closed b -> a = b

  let implies_tree a b =
    let res =
      (is_empty_tree b) || not (is_empty_tree a) && (
        Return.set_checkpoint (
          fun label ->
            let not_included () = Return.return label false in
            let rec is_included b a =
              let iter_b field node_b =
                match FieldMap.find_opt field a.map with
                | None -> not_included ()
                | Some node_a -> (
                    match node_b, node_a with
                    | Abs, Abs -> ()
                    | Node b, Node a -> is_included b a
                    | PConst b, PConst a -> if not (QmlAstUtils.Const.equal b a) then not_included ()
                    | Node b, PConst _ -> if not (is_empty_tree b) then not_included ()
                    | _ -> not_included ()
                  )
              in
              if not (implies_tag a.tag b.tag)
              then
                not_included ()
              else
                FieldMap.iter iter_b b.map
            in
            is_included b a ;
            true
        )
      )
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumCondition.implies">
        OManager.printf (
          "@{<bright>implies@} a b@\n@[<2>where @{<bright>a@} is:@\n%a@]@\n@[<2>and @{<bright>b@} is:@\n%a@]@\n@[<2>returns: %b@]@\n@."
        )
        pp_tree a
        pp_tree b
        res
      #<End>
    in
    res

  let filter_tree t d =
    let rec aux d =
      match d with
      | True -> True
      | Check (check, rev_path) ->
          if fst (add_path ~check t (List.rev rev_path))
          then True
          else d

      | And list ->
          let filter_map d =
            let d = aux d in
            if d = True then None else Some d
          in
          let list = List.filter_map filter_map list in
          conjonction list
    in
    let res =
      if is_empty_tree t
      then
        d
      else
        aux d
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumCondition.filter">
        OManager.printf (
          "@{<bright>filter@} t d@\n@[<2>where @{<bright>t@} is:@\n%a@]@\n@[<2>and @{<bright>d@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        pp_tree t
        pp_decision d
        pp_decision res
      #<End>
    in
    res

  let implies_decision_tree t d = d = True || filter_tree t d = True

  let normalize tree sum_case_negation implications =
    let fold (((tree, _) as cpl), implications) ((cond, negation) as implication) =
      if implies_tree tree cond
      then (
        let fold (tree, sum_case_negation) item =
          match item with
          | Decision dec ->
              let tree = add_tree dec tree in
              tree, sum_case_negation
          | NotSumCase (ident, index) ->
              let sum_case_negation = add_sum_case_negation ident index sum_case_negation in
              tree, sum_case_negation
        in
        let cpl = List.fold_left fold cpl negation in
        cpl, implications
      )
      else
        let implications = implication :: implications in
        cpl, implications
    in
    let (tree, sum_case_negation), implications =
      List.fold_left fold ((tree, sum_case_negation), []) implications in
    T.full_cons tree sum_case_negation implications

  let decision d =
    let tree = add_tree d empty_tree in
    T.tree tree

  let add d t =
    let tree = add_tree d t.T.tree in
    normalize tree t.T.sum_case_negation t.T.implications

  let add_implication (cond, negation) t =
    assert (IntMap.is_empty cond.T.sum_case_negation) ;
    assert (cond.T.implications = []) ;
    normalize t.T.tree t.T.sum_case_negation ((cond.T.tree, negation) :: t.T.implications)
  let filter t d = filter_tree t.T.tree d
  let implies a b =
    assert (IntMap.is_empty b.T.sum_case_negation) ;
    assert (b.T.implications = []) ;
    implies_tree a.T.tree b.T.tree
  let implies_decision t d = implies_decision_tree t.T.tree d
end

module SumEnv =
struct
  module M = ListMap.Make ( Order.StringList )

  type 'ident t = ( SumCondition.t * 'ident ) list M.t

  let empty = M.empty

  let add_dot ~rev_path condition ident t =
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumEnv">
        OManager.printf (
          "@{<bright>SumEnv.add@} ~rev_path condition@\n"^^
          "@[<2>where @{<bright>rev_path@} is:@\n%a@]@\n"^^
          "@[<2>and @{<bright>condition@} is:@\n%a@]@\n@."
        )
        Common.pp_path rev_path
        SumCondition.pp condition
      #<End>
    in
    M.append rev_path (condition, ident) t

  let find_dot ~rev_path condition t =
    let ident =
      match M.find_opt rev_path t with
      | None -> None
      | Some list ->
          let find (cond, ident) = if SumCondition.implies condition cond then Some ident else None in
          List.find_map find list
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumEnv">
        OManager.printf (
          "@{<bright>SumEnv.find@} ~rev_path condition@\n"^^
          "@[<2>where @{<bright>rev_path@} is:@\n%a@]@\n"^^
          "@[<2>and @{<bright>condition@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        Common.pp_path rev_path
        SumCondition.pp condition
        (Option.pp_meta (DebugPrint.pp ~depth:max_int)) ident
      #<End>
    in
    ident
end

(*
  assert or skip the prefix of the sum prefix path
*)
let assert_sum_prefix sum_rev_prefix_path rev_prefix_path =
  match sum_rev_prefix_path with
  | [] -> ()
  | _ ->
      let rec aux a b =
        match a, b with
        | [], _ -> ()
        | hda :: tla, hdb :: tlb when hda = hdb -> aux tla tlb
        | _ ->
            OManager.printf "assert_rev_prefix_path: %a@\nrev_prefix_path: %a@."
              Common.pp_path sum_rev_prefix_path
              Common.pp_path rev_prefix_path
            ;
            assert false
      in
      aux (List.rev sum_rev_prefix_path) (List.rev rev_prefix_path)

let rev_skip_sum_prefix sum_rev_prefix_path rev_prefix_path =
  match sum_rev_prefix_path with
  | [] -> List.rev rev_prefix_path
  | _ ->
      let rec aux a b =
        match a, b with
        | [], _ -> b
        | hda :: tla, hdb :: tlb when hda = hdb -> aux tla tlb
        | _ ->
            OManager.printf "sum_rev_prefix_path: %a@\nrev_prefix_path: %a@."
              Common.pp_path sum_rev_prefix_path
              Common.pp_path rev_prefix_path
            ;
            assert false
      in
      aux (List.rev sum_rev_prefix_path) (List.rev rev_prefix_path)

module SumAnalysis =
struct
  module SC = SumCondition

  type sum_case = FieldSet.t

  type sum_content = {
    ty : QmlAst.ty ;
    sum_rev_prefix_path : rev_prefix_path ;
    colvar : PatternAnalysis.colvar ;
    fields : FieldSet.t ;
    cases : FieldSet.t array ;
    cache_ty : (field list, QmlAst.ty) Hashtbl.t ;
    cache_sum : (field list, sum) Hashtbl.t ;
    ident : sum_ident ;
  }
  and sum = sum_content option

  let pp_sum_content fmt sum =
    Format.fprintf fmt (
      "@[<2>{@\nty: %a@\nrev_prefix_path: %a@\ncolvar: %a@\nfields: [ %a ]@\ncases: %a@\nident: %d@\n}@]"
    )
      QmlPrint.pp#ty sum.ty
      Common.pp_path sum.sum_rev_prefix_path
      PatternAnalysis.pp_flag sum.colvar
      Common.pp_fieldset sum.fields
      (Format.pp_list " / " Common.pp_fieldset) (Array.to_list sum.cases)
      sum.ident

  let pp_sum fmt sum = Option.pp_none pp_sum_content fmt sum

  let ident =
    let i = ref (-1) in
    (fun () -> incr(i) ;  !i)

  type t = {
    sum : sum ;

    rev_prefix_path : rev_prefix_path ;
    (*
      The prefix path for this sum case, only if this sum was in a nested pattern.
      If the pattern is not nested, this is the empty list
    *)

    rowvar : PatternAnalysis.rowvar ;
    (*
      the rowvar is specific to each sum case.
      the colvar is the same for all cases, and is in the field [sum]
    *)

    index : sum_index option ;
    (*
      in case of a closed colvar, we know statically
      the index in sum.cases of this analysed case.
    *)

    absent : FieldSet.t ;
    (*
      In case of an open colvar, the set is empty.
      In other cases, it contains all field not present in any possible case
      containing at least the present field (and exactly them, if rowvar is closed)
    *)

    present : FieldSet.t ;
    (*
      The field constituting this case. May be incomplete in case of open rowvar,
      if we wasn't able to guess the complete case.
    *)
  }

  let pp fmt t =
    Format.fprintf fmt "@[<2>{@\nsum: %a@\nprefix: %a@\nrowvar: %a@\nindex: %a@\nabsent: %a@\npresent: %a@\n}@]"
      pp_sum t.sum
      (Format.pp_list "." Format.pp_print_string) (List.rev t.rev_prefix_path)
      PatternAnalysis.pp_flag t.rowvar
      (Option.pp_none Format.pp_print_int) t.index
      Common.pp_fieldset t.absent
      Common.pp_fieldset t.present

  let check_set ~rev_prefix_path check set =
    FieldSet.fold (fun field acc -> (SC.Check (SC.Field check, field :: rev_prefix_path))::acc) set []


  let cases = function
    | None -> None
    | Some sum -> Some sum.cases

  let add t cond =
    let rev_prefix_path = t.rev_prefix_path in
    let fold check set cond = FieldSet.fold
      (fun field cond -> SumCondition.add (SC.Check (SC.Field check, field :: rev_prefix_path)) cond) set cond
    in
    let cond = fold false t.absent cond in
    let cond = fold true t.present cond in
    cond

  let field_compare = String.compare
  let sort_field_list fields = List.sort field_compare fields
  let sort_cases a = List.sort (Array.compare String.compare) a

  let from_ty ~rev_path gamma ty =
    let sum =
      Return.set_checkpoint_none (
        fun label ->
          let cache_ty = Hashtbl.create 10 in
          let cache_sum = Hashtbl.create 10 in
          let ty = QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty in
          let () = Hashtbl.add cache_ty [] ty in
          let cases, colvar =
            match ty with
            | Q.TypeRecord (Q.TyRow (cases, _)) ->
                [ cases ], `closed
            | Q.TypeSum (Q.TyCol (cases, colvar)) ->
                let colvar = if Option.is_none colvar then `closed else `open_ in
                cases, colvar
            | Q.TypeVar _ ->
                Return.return label ()
            | _ ->
                OManager.printf "Imp_SumCase.SumAnalysis.from_ty: internal error on type %a@." QmlPrint.pp#ty ty ;
                assert false
          in
          let fields = List.fold_left
            (List.fold_left (fun acc (field, _) -> FieldSet.add field acc)) FieldSet.empty cases in
          let cases = Array.of_list
            (List.map (List.fold_left (fun acc (field, _) -> FieldSet.add field acc) FieldSet.empty) cases) in
          let ident = ident () in
          let sum =
          {
            ty ;
            sum_rev_prefix_path = rev_path ;
            colvar ;
            fields ;
            cases ;
            cache_ty ;
            cache_sum ;
            ident ;
          } in
          let () = Hashtbl.add cache_sum rev_path (Some sum) in
          sum
      )
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.from_ty">
        OManager.printf (
          "@{<bright>from_ty@} gamma ty@\n@[<2>where @{<bright>ty@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        QmlPrint.pp#ty ty
        pp_sum sum
      #<End>
    in
    sum

  let make sum ~rowvar ~colvar ~rev_prefix_path present =
    let t =
      match sum with
      | None ->
          {
            sum ;
            rev_prefix_path ;
            rowvar ;
            index = None ;
            absent = FieldSet.empty ;
            present ;
          }

      | Some sum -> (
          assert_sum_prefix sum.sum_rev_prefix_path rev_prefix_path ;

      if sum.colvar <> colvar
      then
        (*
          Internal inconsitency between the typer and PatternAnalysis.
          The pattern analysis should build pattern in which, for a given level,
          all colvar should be the same. It uses the typer for guessing the correct
          value of colvar. The sum was also optained using the typer. If the two
          disagree, we have a problem somewhere.
        *)
        assert false
      else
        (*
          Resolution:
          if we are in an closed colvar case, we may try to close the rowvar,
          by inspecting how many cases of the sum match the given fields
        *)
        let rowvar, present, absent, index =
          if colvar = `closed
          then
            let rowvar, present, overlap, index =
              if rowvar = `open_
              then
                (*
                  [good_cases] is the list (case * index) of all cases where we could be.
                  Example:
                  if we are in a sum (a|a;b|a;c|d) and the present fiels are [ a ],
                  the good_case is [ (|a|, 0) ; (|a;b|, 1) ; (|a,c|, 2) ]
                *)
                let good_cases =
                  Array.fold_left_i (
                    fun acc case index ->
                      if FieldSet.subset present case
                      then (case, index) :: acc
                      else acc
                  ) [] sum.cases
                in
                match good_cases with
                | [] ->
                    (*
                      inconsistency, this should have not type
                      this case means that we are trying to build an analysed sum case
                      with a pattern which is included in not any cases corresponding to
                      the type of the pattern
                    *)
                    OManager.printf (
                      "ASSERT FALSE:@\n@[<2>sum is:@\n%a@]@\n@[<2>present is:@\n%a@]@."
                    )
                      pp_sum_content sum
                      Common.pp_fieldset present
                    ;
                    assert false

                | [ hd, index ] ->
                    (*
                      We can complete the pattern, and close the rowvar.
                    *)
                    `closed, hd, hd, Some index

                | (hd, _) :: tl ->
                    (*
                      Several overlap choices, we cannot close the rowvar.
                    *)
                    let overlap = List.fold_left (fun acc (hd, _) -> FieldSet.union hd acc) hd tl in
                    `open_, present, overlap, None
              else
                (*
                  closed rowvar, and closed colvar.
                *)
                let indexes = Array.fold_left_i (
                  fun acc case index -> if FieldSet.equal case present then index::acc else acc
                ) [] sum.cases in
                match indexes with
                | [ index ] ->
                    `closed, present, present, Some index
                | _ ->
                    (*
                      In this case, the matched structure should correspond
                      exactly to one of the case of the matched expression
                      If this assert breaks, this means an inconsitency, this should have not type
                    *)
                    OManager.printf (
                      "ASSERT FALSE:@\n@[<2>sum is:@\n%a@]@\n@[<2>present is:@\n%a@]@\n"^^
                      "@[<2>indexes are: %a@]@."
                    )
                      pp_sum_content sum
                      Common.pp_fieldset present
                      (Format.pp_list ", " Format.pp_print_int) indexes
                    ;
                    assert false
            in
            let absent = FieldSet.diff sum.fields overlap in
            rowvar, present, absent, index
          else
            rowvar, present, FieldSet.empty, None
        in
        {
          sum = Some sum ;
          rev_prefix_path ;
          rowvar ;
          absent ;
          present ;
          index ;
        }
    )
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.make">
        OManager.printf (
          "@{<bright>make@} sum ~rowvar:%a ~colvar:%a present@\n"^^
          "@[<2>where @{<bright>sum@} is:@\n%a@]@\n@[<2>and @{<bright>present@} is:@\n%a@]@\n"^^
          "@[<2>returns:@\n%a@]@\n@."
        )
        PatternAnalysis.pp_flag rowvar
        PatternAnalysis.pp_flag colvar
        pp_sum sum
        Common.pp_fieldset present
        pp t
      #<End>
    in
    t

  let implies sum =
    let res = match sum with | None -> SC.True | Some sum ->
      if sum.colvar = `open_ then SC.True else
        let rev_prefix_path = sum.sum_rev_prefix_path in
        let cases = sum.cases in
        let len = Array.length cases in
        assert (len > 0) ;
        let rec aux i set =
          if i = len then set else
            let set = FieldSet.inter set cases.(i) in
            if FieldSet.is_empty set then set else
              aux (succ i) set
        in
        let inter = aux 1 cases.(0) in
        let present = check_set ~rev_prefix_path true inter in
        SC.conjonction present
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.implies">
        OManager.printf (
          "@{<bright>implies@} sum@\n@[<2>where @{<bright>sum@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        pp_sum sum
        SumCondition.pp_decision res
      #<End>
    in
    res

  let follow_dot gamma ty path =
    let rec aux ty = function
      | [] -> ty
      | field :: tl -> (
          let cases =
            match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
            | Q.TypeRecord (Q.TyRow (cases, _)) -> [ cases ]
            | Q.TypeSum (Q.TyCol (cases, _)) -> cases
            | ty ->
                OManager.printf "Imp_SumCase.SumAnalysis.follow_dot: internal error on type %a with path %a@\n"
                  QmlPrint.pp#ty ty
                  (Format.pp_list "." Format.pp_print_string) path
                ;
                assert false
          in
          let ty = Return.set_checkpoint (
            fun label ->
              List.iter (List.iter (fun (field2, ty) -> if field = field2 then Return.return label ty)) cases ;
              OManager.printf "Imp_SumCase.SumAnalysis.follow_dot: internal error on type %a with path %a looking for field %s@\n"
                QmlPrint.pp#ty ty
                (Format.pp_list "." Format.pp_print_string) path field
              ;
              (* DEBUG *)
              List.iter
                (fun case ->
                   OManager.printf "Case: " ;
                   List.iter
                     (fun (field, ty) ->
                        OManager.printf "%s : %a; " field QmlPrint.pp#ty ty)
                     case ;
                   OManager.printf "@.")
                cases ;
              (* END DEBUG *)
              assert false
          ) in
          aux ty tl
        )
    in aux ty path

  let ty gamma sum ~rev_path =
    let ty =
      match sum with
      | None -> Q.TypeVar (TypeVar.next ~descr:"SumAnalysis.unknown_sum" ())
      | Some sum -> (
          let cache_ty = sum.cache_ty in
          match Hashtbl.find_opt cache_ty rev_path with
          | Some ty -> ty
          | None ->
              let path = rev_skip_sum_prefix sum.sum_rev_prefix_path rev_path in
              let ty = follow_dot gamma sum.ty path in
              Hashtbl.add cache_ty rev_path ty;
              ty
        )
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.ty">
        OManager.printf (
          "@{<bright>ty@} gamma sum ~rev_path:[ %a ]@\n"^^
          "@[<2>where @{<bright>sum@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        Common.pp_path rev_path
        pp_sum sum
        QmlPrint.pp#ty ty
      #<End>
    in
    ty

  let from_sum gamma sum ~rev_path =
    let sum =
      match sum with
      | None -> None
      | Some sum_content -> (
          let cache_sum = sum_content.cache_sum in
          match Hashtbl.find_opt cache_sum rev_path with
          | Some sum -> sum
          | None ->
              let ty = ty gamma sum ~rev_path in
              let sum = from_ty ~rev_path gamma ty in
              Hashtbl.add cache_sum rev_path sum;
              sum
        )
    in
    sum

  let filter_other_cases ~condition ~rev_prefix_path ~fields ~sum_ident ~index ~cases =
    let negation_set =
      Option.default IntSet.empty
        (IntMap.find_opt sum_ident condition.SC.T.sum_case_negation)
    in
    let check_field_constency present field =
      let decision = SumCondition.check_field present (field :: rev_prefix_path) in
       try
         ignore(SumCondition.implies_decision condition decision);
         true
       with
       | SumCondition.Inconsistency _ -> false
    in
    (* the set of inconsistents field for this condition *)
    let inconsistent_absent_fields = FieldSet.filter (fun field -> not(check_field_constency false field)) fields in
    let filteri i case =
      not (i = index)
      && not (IntSet.mem i negation_set)
      && (
            (*
              If one of the present field of this case is inconsistent,
              the case should not be kept
            *)
            FieldSet.for_all (check_field_constency true) case
          &&
            (*
              If one of the absent field of this case is inconsistent,
              the case should not be kept
            *)
            FieldSet.is_empty (FieldSet.diff inconsistent_absent_fields case)
            (*
              Else, the case is kept
            *)
        )
    in
    Array.filteri filteri cases

  let decisions condition t =
    let decisions =
      let rev_prefix_path = t.rev_prefix_path in
      let default ?(size=true) () =
        let size = size && t.rowvar = `closed in
        let checks = check_set ~rev_prefix_path true t.present in
        let checks =
          if size
          then
            let size = SumCondition.size rev_prefix_path (FieldSet.size t.present) in
            (*
              for maximizing the sharing of dots; we would rather that the size check
              is done after the dots checks.
            *)
            checks @ [ size ]
          else
            checks
        in
        let conjonction = SumCondition.conjonction checks in
        if SumCondition.implies_decision condition conjonction
        then
          None
        else
          Some [ conjonction ]
      in
      let sum = t.sum in
      if Option.is_none sum then default () else let sum = Option.get sum in
      if sum.colvar = `open_ || t.index = None
      then
        (*
          In case of an [`open_] colvar, we do not care of rowvar, we have only one possible decision,
          this is the conjonction of the presence of all present fields.
          If we wasn't able to detect the index of the sum case, we do not optimize anything.
          The compilation of pattern matching should add a size check.
        *)
        default ()
      else
        let cases = sum.cases in
        let index = Option.get t.index in
        (*
          We will filter the cases statically invalidated by the condition.
          If there are not any case left, this is an inconsistency.
          If there is still only 1 case, we return None,
          else we use the standard disjonction according to presence/absence of fields
        *)
        let other_cases =
          let fields = sum.fields in
          let sum_ident = sum.ident in
          filter_other_cases ~condition ~rev_prefix_path ~fields ~sum_ident ~index ~cases
        in
        let other_size = Array.length other_cases in
        if other_size = 0
        then
          (*
            by elimination, if the pattern is not inconsistent,
            there is nothing to check.
          *)
          let _ =
            let decision =
              let checks = check_set ~rev_prefix_path true t.present in
              SumCondition.conjonction checks
            in
            SumCondition.implies_decision condition decision
          in
          None
        else
          let fold_other fold acc =
            Array.fold_left fold acc other_cases
          in
          (*
            We build the present of all field not present in any other case,
            and we add the absence of all field present in all other cases.
          *)
          let present = fold_other FieldSet.diff t.present in
          let absent = fold_other FieldSet.inter t.absent in
          let present = check_set ~rev_prefix_path true present in
          let absent = check_set ~rev_prefix_path false absent in
          match List.rev_append present absent with
          | [] ->
              (*
                If we end-up there, it can means that:
                1) we want to build decisions for the empty record, typed as void. In this case,
                we will return the default, with the size.
                2) the current implementation of D()() is quite dummy and imcomplete,
                we handle now only cases where there is not too much overlap of fields between
                sum cases, and we do not take benefits of And node (for decision of cardinal > 1)
                TODO: we can improve the algorithm for beeing smarter in case 2)
              *)
              (*
                Partial optimization: the size check should be added only if it exists
                an other case containing the current case (including all its fields)
              *)
              let this_present = t.present in
              let size =
                Return.set_checkpoint (
                  fun label ->
                    let fold _ case =
                      if FieldSet.subset this_present case
                      then
                        Return.return label true
                    in
                    fold_other fold () ;
                    false
                )
              in
              default ~size ()

          | decisions ->
              (*
                Check for consistency, and possible optimisation.
              *)
              if List.exists (SumCondition.implies_decision condition) decisions
              then
                None
              else
                Some decisions
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.decisions">
        OManager.printf (
          "@{<bright>decisions@} cond t@\n"^^
          "@[<2>where @{<bright>cond@} is:@\n%a@]@\n"^^
          "@[<2>where @{<bright>t@} is:@\n%a@]@\n"^^
          "@[<2>returns:@\n%a@]@\n@."
        )
        SumCondition.pp condition
        pp t
        (Option.pp_none (Format.pp_list " / " SumCondition.pp_decision)) decisions
      #<End>
    in
    decisions

  let negation condition t =
    let negation =
      let sum = t.sum in
      if Option.is_none sum then None else let sum = Option.get sum in
      (*
        unoptimized case
      *)
      if sum.colvar = `open_ || t.index = None then None
      else
        let rev_prefix_path = t.rev_prefix_path in
        let cases = sum.cases in
        let index = Option.get t.index in
        let not_sum_case = SC.NotSumCase (sum.ident, index) in
        let other_cases =
          let fields = sum.fields in
          let sum_ident = sum.ident in
          filter_other_cases ~condition ~rev_prefix_path ~fields ~sum_ident ~index ~cases
        in
        let fold_other fold acc =
          Array.fold_left fold acc other_cases
        in
        (*
          We build the absence of all present field not present in any other case,
          and we add the presence of all field present in all other cases.
        *)
        let absent = fold_other FieldSet.inter t.absent in
        let present = fold_other FieldSet.diff t.present in
        let rev_prefix_path = t.rev_prefix_path in
        let fold_set set check decisions =
          let fold field decisions =
            let decision = SC.Check (SC.Field check, field :: rev_prefix_path) in
            decision :: decisions
          in
          FieldSet.fold fold set decisions
        in
        let decisions = [] in
        let decisions = fold_set present false decisions in
        let decisions = fold_set absent  true  decisions in
        let negations =
          if List.is_empty decisions
          then
            [ not_sum_case ]
          else
            let decision = SumCondition.conjonction decisions in
            [ not_sum_case ; SC.Decision decision ]
        in
        Some negations
    in
    let () =
      #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.negation">
        OManager.printf (
          "@{<bright>negation@} t@\n@[<2>where @{<bright>t@} is:@\n%a@]@\n@[<2>returns:@\n%a@]@\n@."
        )
        pp t
        (Option.pp_none SumCondition.pp_negation) negation
        #<End>
    in
    negation


  module Filter =
  struct
    (*
      Use a tool in the implementation:
    *)
    let refinement filter start =
      match List.filter filter start with
      | [] -> start
      | filter -> filter

    let cmp (p, _) (q, _) = Pervasives.compare p q

    let insert p elt acc = List.insert_sorted ~cmp (p, elt) acc

    let patvar ~rev_prefix_path ~to_dot start =
      let rec aux acc = function
        | (SC.Check (_, checked_rev_path)) as elt -> (
            match checked_rev_path with
            | hd :: tl ->
                if compare_path tl rev_prefix_path = 0
                then (
                  match FieldMap.find_opt hd to_dot with
                  | None -> acc
                  | Some p ->
                      insert p elt acc
                )
                else
                  acc
            | [] ->
                (* empty path *)
                assert false
          )
        | SC.True -> acc
        | SC.And decs ->
            let sorted = List.fold_left aux [] decs in
            let pmax, decs = List.fold_left (fun (pmax, acc) (p, elt) -> max pmax p, elt :: acc) (0, []) sorted in
            if decs = [] then acc
            else
              insert pmax (SC.And decs) acc
      in
      let sorted = List.fold_left aux [] start in
      let filter = List.rev_map snd sorted in
      if filter = [] then start else filter

    let non_js_false ~gamma ~sum start =
      let rec filter = function
        | SC.Check (_, rev_path) ->
            let ty = ty gamma sum ~rev_path in
            not (Common.maybe_js_false gamma ty)

        | SC.True -> false
        | SC.And decs ->
            List.exists filter decs
      in
      refinement filter start

    let present start =
      let rec filter = function
        | SC.Check (SC.Field present, _) -> present
        | SC.Check (SC.Const _, _) -> true
        | SC.Check (SC.Size _, _) -> false
        | SC.True -> false
        | SC.And decs ->
            List.exists filter decs
      in
      refinement filter start

    let (|>) x f = f x

    let final_choice ~rev_prefix_path ~gamma ~sum ~to_dot decs =
      Return.set_checkpoint (
        fun label ->
          let (|+>) x f =
            match x with
            | [ x ] -> Return.return label x
            | _ -> (
                let x = f x in
                let () =
                  #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.Filter">
                    OManager.printf (
                      "@[<2>@{<bright>filtering decisions@}: %a@]@\n@."
                    )
                    (Format.pp_list " / " SumCondition.pp_decision) x
                  #<End>
                in
                x
              )
          in
          decs
        |+> patvar ~rev_prefix_path ~to_dot
        |+> non_js_false ~gamma ~sum
        |+> present
        |> List.hd
        )
  end
end
