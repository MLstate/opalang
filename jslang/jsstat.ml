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
module J = JsAst
module StringMap = StringMap
module String = BaseString
module List = BaseList
module Format = BaseFormat
module Arg = BaseArg

type category = string
module CategoryMap = StringMap

type kind =
  | Var of string * J.expr
  | Function of string * string list * J.statement list
  | Other of J.statement
let native_of_ident = function
  | J.Native (_, i) -> i
  | J.ExprIdent _ -> assert false
let kind_of_statement = function
  | J.Js_var (_,i,Some e) ->
      Var (native_of_ident i,e)
  | J.Js_function (_,i,params,body) ->
      Function (native_of_ident i,
                List.map native_of_ident params,
                body
               )
  | stm -> Other stm

type state = {
  has_seen_closure_or_ei : bool;
  map : (kind * J.statement) list CategoryMap.t;
}
let empty_state = {
  has_seen_closure_or_ei = false;
  map = CategoryMap.empty;
}
let category_of_kind has_seen_closure_or_ei = function
  | Other (J.Js_expr (_, J.Je_call (_, J.Je_ident (_, J.Native (_, s)), _, _))) when String.is_contained "set_distant" s ->
      "closure_creation"
  | Other _ -> "bsl_static"
  | Function (s,_,_) when String.is_contained "clos_env" s
                 || String.is_contained "clos_args" s -> "closure_dyn"
  | Function (s,_,_) when String.is_contained "_bsl_" s -> "bsl_proj"
  | Var (s,_) when String.is_prefix "_sei" s
                || String.is_contained "type_def_map" s -> "ei"
  | Function (s,_,_) when String.is_contained "_stub_" s -> "rpc_stub"
  | Function (s,_,_) when String.is_contained "_skeleton_" s -> "rpc_skeleton"
  | Var (_,e) when
      JsWalk.Expr.exists
        (function
         | J.Je_ident (_,i) when String.is_contained "BslClosure_create" (native_of_ident i)
                              || String.is_contained "BslClosure_define" (native_of_ident i) -> true
         | _ -> false) e -> "closure_creation"
  | Function (s,_,body)
      when String.is_contained "_choice_" s ||
        List.exists
        (JsWalk.ExprInStatement.exists
           (function
            | J.Je_ident (_,i) when
                String.is_contained "check_partial" (native_of_ident i)
                || String.is_contained "primary_list" (native_of_ident i)
                || String.is_contained "itsub" (native_of_ident i)
                -> true
            | _ -> false)) body
        -> "usercode_parser"
  | Function _
  | Var _ -> if has_seen_closure_or_ei then "usercode" else "bsl_static"

let rec flatten_block stm =
  match stm with
  | J.Js_block (_,stms) -> flatten_blocks stms
  | _ -> [stm]
and flatten_blocks stms =
  List.concat_map flatten_block stms

let analyse ~code_to_analyse ~code_to_display =
  let state =
    List.fold_left2
      (fun state stm_to_display stm_to_analyse ->
         let kind_to_analyse = kind_of_statement stm_to_analyse in
         let kind_to_display = kind_of_statement stm_to_display in
         let category = category_of_kind state.has_seen_closure_or_ei kind_to_analyse in
         let state =
           if category = "closure_dyn" || category = "ei"
           then {state with has_seen_closure_or_ei = true}
           else state in
         let prev = try CategoryMap.find category state.map with Not_found -> [] in
         let map = CategoryMap.add category ((kind_to_display,stm_to_display) :: prev) state.map in
         {state with map}
      ) empty_state code_to_display code_to_analyse in
  {state with map = CategoryMap.map List.rev state.map}

let sizeof_stm acc stm =
  JsWalk.TStatement.fold
    (fun acc _ -> acc + 1)
    (fun acc _ -> acc + 1)
    acc stm

let repartition_of_fields acc stm =
  JsWalk.ExprInStatement.fold
    (fun acc e ->
       match e with
       | J.Je_dot (_, _, field)
       | J.Je_binop (_, J.Jb_hashref _, _, J.Je_string (_, field, _)) ->
           StringMap.add field ((try StringMap.find field acc with Not_found -> 0) + 1) acc
       | _ -> acc)
    acc stm

let display ?experiment filename state code =
  (match experiment with
  | None ->
      Printf.printf "### Stats for %s\n" filename;
      let total_number = List.length code in
      Printf.printf "Total number of statements: %d\n" total_number;
      CategoryMap.iter
        (fun category kinds ->
           let number = List.length kinds in
           Printf.printf "  %s: %d statements, %.2f%%\n" category number (100. *. (float) number /. (float) total_number)
        ) state.map;
      Printf.printf "\n%!";
      CategoryMap.iter
        (fun category kinds ->
           let filename = filename ^ "_" ^ category in
           let out = open_out filename in
           let f = Format.formatter_of_out_channel out in
           List.iteri
             (fun (_,stm) i ->
                Format.fprintf f "// statement n°%d@\n%a@\n" i JsPrint.pp#code [stm]
             ) kinds;
           Format.fprintf f "@.";
           close_out out
        ) state.map;
      Printf.printf "(files dumped)\n\n";
      let total_size = List.fold_left sizeof_stm 0 code in
      Printf.printf "Size in ast nodes:\n";
      Printf.printf "Total is %d nodes\n" total_size;
      CategoryMap.iter
        (fun category kinds ->
           let size = List.fold_left (fun acc (_,stm) -> sizeof_stm acc stm) 0 kinds in
           Printf.printf "  %s: %d nodes, %.2f%%\n" category size (100. *. (float) size /. (float) total_size)
        ) state.map;
      Printf.printf "\n%!";
      let total_size = String.length (Format.to_string JsPrint.pp#code code) in
      Printf.printf "Size in source length:\n";
      Printf.printf "Total size is %d chars\n%!" total_size;
      CategoryMap.iter
        (fun category kinds ->
           let size = String.length (Format.to_string JsPrint.pp#code (List.map snd kinds)) in
           Printf.printf "  %s: %d chars, %.2f%%\n" category size (100. *. (float) size /. (float) total_size)
        ) state.map;
      Printf.printf "\n%!";
      Printf.printf "Length of the fields:\n";
      let map =
        CategoryMap.fold
          (fun category kinds map ->
             let new_map = List.fold_left (fun acc (_, stm) -> repartition_of_fields acc stm) StringMap.empty kinds in
             let full_map = StringMap.merge (+) map new_map in
             let full_length = StringMap.fold (fun k v acc -> v * String.length k + acc) new_map 0 in
             let renamed_length = StringMap.fold (fun _k v acc -> v * 2 + acc) new_map 0 in
             Printf.printf "  %s: normal: %d, renamed: %d\n%!" category full_length renamed_length;
             full_map
          ) state.map StringMap.empty in
      let full_length = StringMap.fold (fun k v acc -> v * String.length k + acc) map 0 in
      let renamed_length = StringMap.fold (fun _k v acc -> v * 2 + acc) map 0 in
      Printf.printf "Total: normal: %d, renamed: %d\n%!" full_length renamed_length;
      Printf.printf "\n%!";
  | Some _ -> ());
  let total_size = String.length (JsMinify.minify (Format.to_string JsPrint.pp#code code)) in
  if experiment = None then Printf.printf "Size in minified source length:\n";
  (match experiment with
   | None -> Printf.printf "Total minified size is %d chars\n%!" total_size
   | Some experiment_name -> Printf.printf "%s.total_size %d\n" experiment_name total_size);
  CategoryMap.iter
    (fun category kinds ->
       let str = JsMinify.minify (Format.to_string JsPrint.pp#code (List.map snd kinds)) in
       let size = String.length str in
       match experiment with
       | None ->
           let filename = filename ^ "_min_" ^ category in
           let out = open_out filename in
           Printf.fprintf out "%s\n%!" str;
           close_out out;
           Printf.printf "  %s: %d chars, %.2f%%\n" category size (100. *. (float) size /. (float) total_size)
       | Some experiment_name ->
           Printf.printf "%s.%s %d\n" experiment_name category size
    ) state.map;
  Printf.printf "\n%!"

let parse filename =
  let code = JsParse.File.code filename in
  flatten_blocks code

let experiment_name = ref None
let file_for_analysis = ref None
let file_for_display = ref None
let arguments = [
  "--experiment", Arg.String (fun s -> experiment_name := Some s), "";
  "--alpha", Arg.String (fun s -> file_for_display := Some s), "";
  "--no-alpha", Arg.String (fun s -> file_for_analysis := Some s), "";
]
let anon_fun = fun s -> file_for_analysis := Some s

let () =
  Arg.parse arguments anon_fun "You are brave, you don't need help!";
  let file1, o =
    match !file_for_analysis, !file_for_display with
    | None, None -> exit 0
    | Some f, o -> f, o
    | None, Some _ ->
        Printf.eprintf "You must set the file that should be analysed\n%!";
        exit 1 in
  let code_to_analyse = parse file1 in
  let code_to_display =
    match o with
    | None -> code_to_analyse
    | Some filename -> parse filename in
  let filename_to_display =
    match o with
    | None -> file1
    | Some file -> file in
  let state = analyse ~code_to_analyse ~code_to_display in
  display ?experiment:!experiment_name filename_to_display state code_to_display
