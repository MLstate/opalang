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

(**
   Runtime for generated files by trx.

   TODO:(Adam) documentation of this module
   @author Adam Koprowski
*)

let pr = Printf.sprintf

type pos = int

(* =========================================================================================================== *)
(* ============================================= Error handling ============================================== *)
(* =========================================================================================================== *)

exception SyntaxError of pos * string

type errorDesc =
  | Expected of string

type parseError =
    { errorPos : pos
    ; expected : errorDesc list
    ; mainConstruct : (pos * errorDesc) option
    }

type 'a result =
  | Ok of 'a * parseError
  | Fail of parseError

let rec errorDescList2str = function
  | [Expected e] -> e
  | Expected e::es -> pr "%s or %s" e (errorDescList2str es)
  | _ -> "[???]"

let joinErrors e1 e2 =
  let rec joinExp = function
    | [], e -> e
    | e, [] -> e
    | x::xs, y::ys when x = y -> joinExp (x::xs, ys)
    | x::xs, y::ys when x < y -> x::joinExp (xs, y::ys)
    | x::xs, y::ys -> y::joinExp (x::xs, ys)
  in
  let joinMC = function
    | None, e -> e
    | e, None -> e
    | (Some (e1p, e1d) as e1), (Some (e2p, e2d) as e2) ->
        if e1p > e2p then
          e1
        else if e2p > e1p then
          e2
        else if e1d = e2d then
          e1
        else
          None
  in
  if e1.errorPos > e2.errorPos || e2.expected = [] then
    e1
  else if e2.errorPos > e1.errorPos || e1.expected = [] then
    e2
  else
    { errorPos = e1.errorPos
    ; expected = joinExp (e1.expected, e2.expected)
    ; mainConstruct = joinMC (e1.mainConstruct, e2.mainConstruct)
    }

let res_err_fun f = function
  | Ok (res, e) -> Ok (res, f e)
  | Fail e -> Fail (f e)

let decorateConstruct res pos err =
  let fix e =
    if e.errorPos > pos then
      e
    else
      { e with errorPos = pos; expected = err }
  in
  res_err_fun fix res

let emptyError pos =
  { errorPos = pos
  ; expected = []
  ; mainConstruct = None
  }

let option_to_res_err opt pos err =
  let res =
    match opt with
    | Some res -> Ok (res, emptyError pos)
    | None -> Fail (emptyError pos)
  in
  decorateConstruct res pos err

let option_to_res_msg opt pos msg =
  option_to_res_err opt pos [Expected msg]

let addErrorInfo err = res_err_fun (joinErrors err)

let setMainConstruct res pos err =
  let setMC e =
    let newMC =
      match e.mainConstruct with
      | None -> Some (pos, err)
      | Some (p, _) when pos > p -> Some (pos, err)
      | Some (p, d) when pos = p && d = err -> Some (pos, err)
      | Some (p, _) when pos < p -> e.mainConstruct
      | Some (p, _) when pos = p -> None
      | Some _ -> assert false
    in
    { e with mainConstruct = newMC }
  in
  decorateConstruct (res_err_fun setMC res) pos [err]

let error2str pos2loc e =
  let extraInfo =
    match e.mainConstruct with
    | None -> ""
    | Some (pos, (Expected msg)) ->
        pr "\n(while parsing %s starting at %s)" msg (FilePos.get_pos_string (pos2loc pos))
  in
  pr "expected %s %s" (errorDescList2str e.expected) extraInfo

let gen_syntax_error pos2loc err =
  raise (SyntaxError (err.errorPos, error2str pos2loc err))

exception Final of errorDesc list
let range_to_error r =
  let rec aux = function
    | [] -> []
    | `Any :: _ -> raise (Final [Expected "any character"])
    | `One c::cs -> Expected (pr "'%c'" c)::aux cs
    | `Range (c1, c2)::cs -> Expected (pr "['%c'-'%c']" c1 c2)::aux cs
  in
  try
    List.sort Pervasives.compare (aux r)
  with
    Final err -> err

let print_error pos err = pr "At %s: %s" pos err

let show_error_aux pos2loc pos err =
  print_error (FilePos.get_pos_string (pos2loc pos)) err

let show_error content pos err =
  print_error (FilePos.get_pos_string (FilePos.get_pos_no_cache content pos)) err

let show_parse_error pos2loc err =
  show_error_aux pos2loc err.errorPos (error2str pos2loc err)

(* was used by trx_ocaml.ml; temporarily suspended suspending backtrace recording ;)
let suspend_backtrace_recording f arg =
  let backtrace_stat = Printexc.backtrace_status () in
  Printexc.record_backtrace false;
  let result = f arg in
  Printexc.record_backtrace backtrace_stat;
  result
*)

(* =========================================================================================================== *)
(* ============================================= Parsing support ============================================= *)
(* =========================================================================================================== *)

let process_range _get_char _len pos cl =
  if pos < _len then begin
    let c = _get_char pos in
    let rec aux = function
      | [] -> false
      | `Any::_ -> true
      | `One c'::cs -> c = c' || aux cs
      | `Range (c1, c2)::cs -> (c >= c1 && c <= c2) || aux cs
    in
    if aux cl then
      Some (pos + 1, c)
    else
      None
  end else
    None

let process_literal _get_char _len pos literal case =
  let literal_len = String.length literal in
  let equal_insensitive c1 c2 = Char.lowercase c1 = Char.lowercase c2 in
  let eq = if case then (=) else equal_insensitive in
  let rec aux i =
    if i = literal_len then
      true
    else
      eq (_get_char (pos + i)) (String.unsafe_get literal i) && aux (i + 1)
  in
  if pos + literal_len <= _len && aux 0 then
    Some (pos + literal_len, literal)
  else
    None

let while_primary plus f pos =
  let rec aux res parse_errors pos =
    match f pos with
    | Ok ((pos', r), e) ->
        aux (r::res) (e :: parse_errors) pos'
    | Fail e ->
        let e =
          List.fold_left (fun e parse_error -> joinErrors parse_error e)
            e parse_errors in
        if plus && res = [] then
          Fail e
        else
          Ok ((pos, List.rev res), e)
  in
  aux [] [] pos

let while_primary_noerr plus f pos =
  let rec aux res pos =
    match f pos with
    | Some (pos', r) -> aux (r::res) pos'
    | None ->
        if plus && res = [] then
          None
        else
          Some (pos, List.rev res)
  in
  aux [] pos

(* FIXME This function and the following one are essentially a hack.
   They assume that we don't care about the result of parsing so
   give an empty list as said result. This has the benefit that we
   have the same type as for the above, un-optimized functions
   and as long as we don't inspect this result everything is fine.
   Of course much better would be to do it in a type-safe way, i.e.
   either switch the result to option type or to lazy. But that
   has far gone consequences for TRX infrastructure and is difficult
   to do without LOTS of changes to it (I already tried and gave up)
   and without a small runtime penalty as well. So for now I'm just
   leaving this hack. If anyone has a better idea of how to address
   it, I'm all ears... Adam *)
let while_primary_nores plus f pos =
  let rec aux first parse_errors pos =
    match f pos with
    | Ok ((pos', _), e) ->
        aux false (e :: parse_errors) pos'
    | Fail e ->
        let e =
          List.fold_left (fun e parse_error -> joinErrors parse_error e)
            e parse_errors in
        if plus && first then
          Fail e
        else
          Ok ((pos, []), e)
  in
  aux true [] pos

let while_primary_noerr_nores plus f pos =
  let rec aux first pos =
    match f pos with
    | Some (pos', _) -> aux false pos'
    | None ->
        if plus && first then
          None
        else
          Some (pos, [])
  in
  aux true pos

let err_stack = ref []

let push_errInfo (err : parseError) =
  err_stack := err :: !err_stack

let gatherErrors res =
  let rec aux res = function
    | [] -> res
    | x::xs -> aux (addErrorInfo x res) xs
  in
  let res = aux res !err_stack in
  err_stack := [];
  res

(* Needed for Netweb *)
let get_pos :pos->int = fun p -> p

let update_memoization_cache cache update_pos =
  (* FIXME, this is a bit ugly/inefficient, but the point is to update [cache]
            in place; any ideas how to do it better? *)
  Hashtbl.clear cache;
  let update_entry (pos, result) =
    match update_pos ~pos:pos with
    | None -> ()
    | Some pos' -> Hashtbl.add cache pos' result
  in
  let content = Hashtbl.fold (fun k v l -> (k, v)::l) cache [] in
  List.iter update_entry content;
  cache
