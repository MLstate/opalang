(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
module Top = QmlGenTop.Top

let _start = ref None
let _table = Hashtbl.create 10
let _last_id = ref 1

let load_libs = ref true

type env_id = int

let initial () =
  QmlTopLevel.Properties.fatal_mode := false;
  match !_start with 
  | None -> 
      let loaders = BslLib.LoaderTable.finalize ~fatal:true () in
      let libs = List.map (
	fun loader ->
	  load_libs, Printf.sprintf "customlib-%s" loader.BslLib.module_name, Some loader.BslLib.dynloader, List.map snd loader.BslLib.splitqmlinit
      ) loaders
      in
      let env = Top.dynload_and_init_env libs in
      _start := Some env;
      env
  | Some env -> env

let get_env id =
  try
    Hashtbl.find _table id
  with
  | Not_found -> 
      let env = initial () in
      let res = env, "" in
      Hashtbl.add _table id res;
      res

let set_env (id:env_id) = Hashtbl.add _table id

let webtop = "webtop:input"
let fold_map_topexpr = Top.fold_map_topexpr ~dbgen:true ~dump:true ~loc:webtop

let fold_map env src =
  try
    let src =
      match QmlAstParser.TopLevel.of_string src with
      | QmlAstParser.ParsedExpr expr -> [QmlTopLevel.Top_eval_expr expr]
      | QmlAstParser.ParsedCode code -> List.map (fun elt -> QmlTopLevel.Top_code_elt elt) code
    in
    let env, out = Base.List.fold_left_map fold_map_topexpr env src in
    let out = Top.to_string out in
    env, out
  with
  | QmlAstParser.Exception e ->
      env, QmlAstParser.short_parse_error_message ~extra:webtop e
  | e -> 
      env, Top.string_of_exception e

##register init : void -> void
let init () = ignore (initial ())

##register eval : int -> string -> int
let eval id src =
  let env, _ = get_env id in
  let res = fold_map env src in
  let id = !_last_id in
  incr _last_id;
  set_env id res;
  id

##register get_result : int -> string
let get_result id =
  let _, out = get_env id in
  out

##register delete_env : int -> unit
let delete_env id = Hashtbl.remove _table id
