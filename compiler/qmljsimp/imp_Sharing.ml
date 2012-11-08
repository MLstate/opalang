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
   @author Quentin Bourgerie
*)

module Map = BaseMap
module List = BaseList

module J = JsAst
module U = JsUtils

module JsMap =
  Map.Make (struct
             type t = J.statement
             let compare = U.compare_statement
            end)

let rec collect jsmap code =
  List.fold_left
    (fun jsmap -> function
     | J.Js_function (l, i, p, b) ->
         let noident = J.Js_function (l, J.Native (`global false, ""), p, b) in
         JsMap.replace noident
           (function
            | None -> [i]
            | Some [] -> assert false
            | Some (id::ids) -> id::i::ids)
           jsmap
     | J.Js_block (_, block) -> collect jsmap block
     | _ -> jsmap)
    jsmap
    code

let rewrite_expr jsmap = JsWalk.Expr.map
  (function
   | J.Je_ident (l, i) as e ->
       begin match JsIdentMap.find_opt i jsmap with
       | None -> e
       | Some i -> J.Je_ident (l, i)
       end
   | e -> e
  )


let rec rewrite jsmap code =
  List.filter_map
    (function
     | J.Js_function (l, i, p, e) ->
         if JsIdentMap.mem i jsmap then None
         else Some (J.Js_function (l, i, p, rewrite jsmap e))
     | J.Js_block (l, s) -> Some (J.Js_block (l, rewrite jsmap s))
     | s -> Some (JsWalk.ExprInStatement.map (rewrite_expr jsmap) s))
    code

type env = JsAst.ident JsIdentMap.t

let process_code ~pass code =
  let module S =
      struct
        type t = env
        let pass = Printf.sprintf "imp_Sharing_%s" pass
        let pp fmt m =
          JsIdentMap.pp ",@ "
            (fun fmt i0 i1 ->
               Format.fprintf fmt "%a => %a"
                 JsPrint.pp#ident i0
                 JsPrint.pp#ident i1)
            fmt m
      end
  in
  let module R = ObjectFiles.Make(S)
  in
  let jsmap = collect JsMap.empty code in
  let env =
    JsMap.fold
      (fun _e ids acc ->
         match ids with
         | id::((_::_) as tail) ->
             List.fold_left
               (fun acc i ->
                  try
                    JsIdentMap.safe_add i id acc
                  with _ -> acc
               ) acc tail
         | _ -> acc
      ) jsmap JsIdentMap.empty
  in
  R.save env;
  let env = R.fold ~deep:true (JsIdentMap.merge (fun _ _ -> assert false)) env in
  #<If:JS_IMP>
  let _outputer oc tosave =
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a%!" S.pp tosave
  in
  ignore(PassTracker.file ~filename:"js_imp_sharing" _outputer env);
  #<End>;
  let code = rewrite env code in
  code, env


let get_substitute env i = JsIdentMap.find_opt i env
