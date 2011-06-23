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
    @author Esther Baruk
    @author Mathieu Barbin
**)

(* See documentation about this module in .mli *)

(* depends *)
let (|>) = InfixOperator.(|>)

(* shorthands *)
(* FIXME, replace by
module O = OcamlAst
*)
module O = Ocaml

(* -- *)

module App =
struct
  let nary_app a b =
    let rec aux acc a =
      match a with
      | O.App (c, d) ->
          aux (d::acc) c
      | _ -> a, acc
    in
    aux [b] a

  let app f args =
    List.fold_left (fun acc e -> O.App (acc, e)) f args
end

module Array =
struct
  let relax_make a =
    let l = match a with O.AnArray l -> l | _ -> assert false in
    let n = List.length l in
    let t = Ocaml.make_Var "t" in
    let array_creation =
      O.Cons.app3
        (O.make_Varl ["Array"; "make"])
        (O.Const (O.Int n))
        (O.make_magic (O.Const (O.Int 0))) in
    let array_filling =
      Base.List.mapi (fun i x -> O.make_array_unsafe_set_no_magic i t x) l
    in
    let sequence =
      let rec aux = function
        | [x] -> x
        | hd :: tl ->
            O.Sequence(hd, aux tl)
        | [] -> assert false
      in
      aux array_filling
    in
    let expr = O.make_Letin (O.Pat (O.PatVar (Ident.source "_"))) sequence t in
    O.make_Letin (O.Pat (O.PatVar (Ident.source "t"))) array_creation expr
end

type filename = string
type module_name = string

module Module =
struct
  type path = module_name list

  let of_filename filename =
    filename
    |> Filename.basename
    |> File.chop_extension
    |> String.capitalize


  let module_path ~full ~pwd =
    let rec link = function
      | [], _ -> []
      | l, [] -> l
      | (a::b) as pf, u::v ->
          if String.compare a u <> 0 then pf
          else link (b, v) in
    link (full, pwd)
end

  (* TODO organize !!! *)
(*
  (* Transcript the type to a qml-well-parsed type in a generation of qml-code *)

  let module_spliter = String.slice_chars "~/,.#"

  let ocaml_module_path_manager_from_string full pwd =
    let pfull = module_spliter full and ppwd = module_spliter pwd in
    let t = ocaml_module_path_manager pfull ppwd in
    String.concat "." t

  type type_path_map = (string * string list * BslTypes.t) BslKeyMap.t




  let to_ocaml_coercion ?(type_path_map=BslKeyMap.empty) ?(current_path=[]) =
    let ocaml_prefix n =
      match BslKeyMap.find_opt (BslKey.of_string n) type_path_map with
      | Some (_, full, _) ->
          begin
            let link = ocaml_module_path_manager full current_path in
            match List.rev link with
            | t::q -> String.concat "." ((List.rev_map String.capitalize q)@[t])
            | [] -> n
          end
      | None -> n in


*)

(* ===== Code optimization ===== *)

open Base
open Ocaml (* FIXME, when we switch to ocaml 3.12, use the local open construct *)

(*
FIXME, use dump-printing instead of this...
let pattern2str = function
  | PatVar _ -> "PatVar"
  | PatPair _ -> "PatPair"
  | PatList _ -> "PatList"
  | PatEmptyList -> "PatEmptyList"
  | PatRecord _ -> "PatRecord"
  | PatConstructor _ -> "PatConstructor"
  | PatVariant _ -> "PatVariant"
  | PatPVariant _ -> "PatPVariant"
  | PatConst _ -> "PatConst"
  | PatAny -> "PatAny"
  | PatGuard _ -> "PatGuard"
  | PatAnnot _ -> "PatAnnot"
  | PatAs _ -> "PatAs"
  | PatArray _ -> "PatArray"

let expr2str = function
  | Var _ -> "Var"
  | Pair _ -> "Pair"
  | Verbatim _ -> "Verbatim"
  | Constructor _ -> "Constructor"
  | _ -> "?"
*)

let normalize_expr = function
  | (Constructor (_, [])) as e -> e
  | (Constructor (_, [Tuple _])) as e -> e
  | Constructor (c, ps) ->
      Constructor (c, [Tuple ps])
  | e -> e

let normalize_pattern = function
  | (PatConstructor (_, [])) as e -> e
  | (PatConstructor (_, [PatTuple _])) as e -> e
  | PatConstructor (c, pps) ->
      PatConstructor (c, [PatTuple pps])
  | p -> p

let ident = BaseString.concat_map "." (fun i -> i)

let rec trivial_pattern (p, e) =
  let res =
  match p, e with
  | PatVar pv, Verbatim v ->
      (* FIXME, hackish; preferrably get rid of Verbatim & properly parse Ocaml snippets in TRX *)
      let vt = Base.String.trim v in
      let pv = OcamlPrint.ident pv in
      pv = vt || Printf.sprintf "( %s )" pv = vt
  | PatVar pv, Var (Pated ([v], _)) -> pv = v
  | PatEmptyList, EmptyList -> true
  | PatList (phd, ptl), Cons (hd, tl) -> trivial_pattern (phd, hd) && trivial_pattern (ptl, tl)
  | PatConstructor (pc, pps), Constructor (c, ps) ->
      List.make_compare Ident.compare pc c = 0 && List.for_all trivial_pattern (List.combine pps ps)
  | PatTuple ps, Tuple es -> List.length ps = List.length es && List.for_all trivial_pattern (List.combine ps es)
  (* FIXME, handle remaining cases *)
  | _ -> false
  in
(*
  Printf.eprintf "\n[%s | %s | " (pattern2str p) (expr2str e);
  OcamlPrint.Output.pattern stderr p;
  Printf.eprintf " VS ";
  OcamlPrint.Output.expr stderr e;
  Printf.eprintf "] -> %b " res;
*)
  res

let optimize_match ~only_trivial e ps =
  let analyze_pattern (pats, all_trivial) ((pat, guard, expr) as match_case) =
    let case, all_trivial' =
      if Option.is_none guard && trivial_pattern (pat, expr) then
        let pat_var = Ident.source "__pat_var" in
        let underscore_pattern =
          OcamlWalk.Pat.map (fun p ->
                                match p with
                                | PatVar s -> PatVar (Ident.source ("_" ^ (OcamlPrint.ident s)))
                                | _ -> p)
        in
        (PatAs (underscore_pattern pat, pat_var), None, Ocaml.Cons.var pat_var), all_trivial
      else
        match_case, false
    in
    case::pats, all_trivial'
  in
  let ps', all_trivial = List.fold_left analyze_pattern ([], true) ps in
  let optimized =
    if all_trivial then
      e
    else if only_trivial then
      Match (e, ps)
    else
      Match (e, List.rev ps')
  in
(*
  if v' <> None then begin
    Printf.eprintf "************** Optimizing\n";
    OcamlPrint.Output.expr stderr (Match (e, ps));
    Printf.eprintf "\ninto:\n";
    OcamlPrint.Output.expr stderr optimized;
    Printf.eprintf "\n\n"
  end;
*)
  optimized

let corresponds p e =
  let rec map_args = function
    | [], [] -> Some IdentMap.empty
    | p::ps, e::es ->
        begin match map_args (ps, es) with
        | None -> None
        | Some m ->
            match p, e with
            | PatTuple ps, Tuple es ->
                map_args (ps, es)
            | PatVar pv, e ->
                if IdentMap.mem pv m then
                  None
                else
                  Some (IdentMap.add pv e m)
            | _ -> None
        end
    | _ -> None
  in
  let res =
    match p, e with
    | PatConstructor (pc, pps), Constructor (c, ps) ->
        if List.make_compare Ident.compare c pc = 0 then
          map_args (pps, ps)
        else
          None
    | _ -> None
  in
(*
  Printf.eprintf "Corresponds: ";
  OcamlPrint.Output.pattern stderr p;
  Printf.eprintf " with ";
  OcamlPrint.Output.expr stderr e;
  Printf.eprintf " => %s\n" (if res <> None then "YES" else "NO");
*)
  res

let inst exp m =
  let add_mapping v e expr =
    Ocaml.make_Letin (Ocaml.Cons.param v) e expr
  in
  IdentMap.fold add_mapping m exp

let rec optimize_expr ~phase e =
  let show _str _res = ()
    (*
    Printf.eprintf "***** %s *****\n" _str;
    OcamlPrint.Output.expr stderr e;
    Printf.eprintf " --> ";
    OcamlPrint.Output.expr stderr _res;
    Printf.eprintf "\n%!"
    *)
  in
  let simplify1 = function
  | Match (me, ps) -> optimize_match ~only_trivial:true me ps
  | _ -> e
  in
  let simplify2 = function
  | Match (Match (me, [(p1, None, r1); (p2, None, r2)]), [(p3, None, r3); (p4, None, r4)]) ->
      begin match corresponds p3 r1, corresponds p4 r2 with
      | Some m1, Some m2 ->
          let res = Match (me, [(p1, None, inst r3 m1); (p2, None, inst r4 m2)]) in
          res
      | _ ->
          match corresponds p4 r1, corresponds p3 r2 with
          | Some m1, Some m2 ->
              let res = Match (me, [(p1, None, inst r4 m1); (p2, None, inst r3 m2)]) in
              res
          | _ -> e
      end
  | e -> e
  in
  let simplify3 = function
  | Match (me, ps) -> optimize_match ~only_trivial:false me ps
  | _ -> e
  in
  match phase with
  | `P1 ->
      let e2 = simplify1 e in
      if e <> e2 then show "simplify1" e2;
      e2
  | `P2 ->
      let e2 = simplify2 e in
      if e <> e2 then show "simplify2" e2;
      let e3 = simplify3 e2 in
      if e2 <> e3 then show "simplify3" e3;
      e3

let optimize c =
  let c = OcamlWalk.PatExpr.map_code normalize_expr normalize_pattern c in
  let rec fix f a =
    let res = f a in
    if res = a then
      a
    else
      fix f res
  in
  let optimize_with f c = fix (List.map (OcamlWalk.Expr.map f)) c in
  let c = optimize_with (optimize_expr ~phase:`P1) c in
  let c = optimize_with (optimize_expr ~phase:`P2) c in
  c

module Misc =
struct
  let size e =
    OcamlWalk.Expr.fold (fun acc _ -> acc + 1) 0 e
end

module Deps =
struct
  let deps add_ident acc e =
    OcamlWalk.Expr.fold
      (fun acc -> function
         | Var (Labeled (_, Some _)) -> acc
         | Var s -> add_ident s acc
         | _ -> acc) acc e
end

(*
  Keep this module at end, because it hides the module Ident from compiler lib
*)
module Ident =
struct

  let is_operator = function
    | "or" | "mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
    | "" -> false
    | s -> (
        match s.[0] with
        | '_' | 'a'..'z' -> false
        | 'A'..'Z' | '0'..'9' -> false
        | _ -> true
      )
end
