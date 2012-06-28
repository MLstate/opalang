(*
    Copyright © 2011, 2012 MLstate

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
module Q = QmlAst
module List = BaseList
module Format = BaseFormat

type env = IdentSet.t

type runtime_type_key = string
type runtime_rpc_key = string
type runtime_ident = string
type runtime_code_elt = {
  ident : runtime_ident option;
  client_equivalent : runtime_ident option;
  defines : [ `rpc of runtime_rpc_key | `type_ of runtime_type_key | `nothing ];
  ident_deps : runtime_ident list;
  rpc_deps: runtime_rpc_key list;
  type_deps : runtime_type_key list;
  root : bool;
}
type runtime_code = runtime_code_elt list

module S =
struct
  type t = env
  let pass = "pass_GenerateServerAst"
  let pp _ _ = ()
end
module R =
struct
  include ObjectFiles.Make(S)
  let load () =
    let env = IdentSet.empty in
    (* can't be a safe union because of fakesources *)
    fold IdentSet.union env
  let save ~loaded_env ~env =
    let small_env = IdentSet.diff env loaded_env in
    save small_env
end

let pp_ident_field f = function
  | None -> Format.fprintf f "None"
  | Some i -> Format.fprintf f "Some %s" i
let pp_defines f = function
  | `nothing -> Format.fprintf f "`nothing"
  | `rpc s -> Format.fprintf f "`rpc %s" s
  | `type_ s -> Format.fprintf f "`type_ %s" s
let pp_ident_deps =
  Format.pp_list ",@ " Format.pp_print_string
let pp_rpc_deps = pp_ident_deps
let pp_type_deps = pp_ident_deps
let pp_root = Format.pp_print_bool
let pp_code_elt f {ident; client_equivalent; defines; ident_deps; rpc_deps; type_deps; root} =
  Format.fprintf f
    "@[@[<v2>{@\n@[<2>ident: %a@]@\n[<2>client_equivalent: %a@]@\n@[<2>defines: %a@]@\n@[<2>ident_deps: %a@]@\n@[<2>type_deps: %a@]@\n@[<2>rpc_deps: %a@]@\n@[<2>root: %a@]@]@\n}@]"
    pp_ident_field ident
    pp_ident_field client_equivalent
    pp_defines defines
    pp_ident_deps ident_deps
    pp_type_deps type_deps
    pp_rpc_deps rpc_deps
    pp_root root
let pp_code =
  Format.pp_list "@\n" pp_code_elt

let is_root e =
  QmlAstWalk.Expr.traverse_exists
    (fun tra -> function
     | Q.Lambda _ -> false
     | Q.Apply _ -> true
     | e -> tra e) e

type acc = {
  mutable acc_rpc_deps : runtime_rpc_key list;
  mutable acc_type_deps : runtime_type_key list;
  mutable acc_rpc_def : runtime_rpc_key list;
  mutable acc_type_def : runtime_type_key list;
  mutable acc_ident_deps : IdentSet.t;
  mutable acc_rec_ident_deps : IdentSet.t;
  mutable acc_fun_action_deps : runtime_ident list;
}

let useless = function
  | {acc_rpc_deps = [];
     acc_type_deps = [];
     acc_rpc_def = [];
     acc_type_def = [];
     acc_ident_deps = x;
     acc_rec_ident_deps = _;
     acc_fun_action_deps = []} when IdentSet.is_empty x -> true
  | _ -> false
let useful acc = not (useless acc)

let process_toplevel_binding env rec_idents e =
  let acc = {
    acc_rpc_deps = [];
    acc_type_deps = [];
    acc_rpc_def = [];
    acc_type_def = [];
    acc_ident_deps = IdentSet.empty;
    acc_rec_ident_deps = IdentSet.empty;
    acc_fun_action_deps = [];
  } in
  let e =
    QmlAstWalk.Expr.map
      (fun e ->
         match e with
         | Q.Ident (_, i) ->
             if IdentSet.mem i env then
               (* dependencies only on toplevel useful idents (env)
                * or rec_idents, because for recursive bindings, this is tricky *)
               acc.acc_ident_deps <- IdentSet.add i acc.acc_ident_deps
             else if IdentSet.mem i rec_idents then
               acc.acc_rec_ident_deps <- IdentSet.add i acc.acc_rec_ident_deps;
             e
         | Q.Directive (label, `tagged_string (s, kind), _, _) ->
             (match kind with
              | Q.Type_def -> acc.acc_type_def <- s :: acc.acc_type_def
              | Q.Type_use -> acc.acc_type_deps <- s :: acc.acc_type_deps
              | Q.Rpc_def -> acc.acc_rpc_def <- s :: acc.acc_rpc_def
              | Q.Rpc_use -> acc.acc_rpc_deps <- s :: acc.acc_rpc_deps
              | Q.Client_closure_use -> acc.acc_fun_action_deps <- s :: acc.acc_fun_action_deps);
             Q.Const (label, Q.String s)
         | _ -> e
      ) e in
  acc, e

let process_code_elt (rev_qml,code,env) = function
  | Q.NewVal (label, iel)
  | Q.NewValRec (label, iel) as code_elt ->
      let idents = List.fold_left (fun acc (i,_) -> IdentSet.add i acc) IdentSet.empty iel in
      let ieal =
        List.map
          (fun (i,e) ->
             let acc, e = process_toplevel_binding env idents e in
             (i, e, acc)
          ) iel in
      let rev_qml =
        let l = List.map (fun (i,e,_) -> (i,e)) ieal in
        let code_elt =
          match code_elt with
          | Q.NewVal _ -> Q.NewVal (label, l)
          | Q.NewValRec _ -> Q.NewValRec (label, l)
          | _ -> assert false in
        code_elt :: rev_qml in
      if List.exists (fun (_, _, acc) -> useful acc) ieal then
        let env = IdentSet.union env idents in
        let code = List.rev_append ieal code in
        (rev_qml, code, env)
      else
        (rev_qml, code, env)
  | _ ->
      assert false

let process_code acc code =
  List.fold_left process_code_elt acc code

let ident_to_string = Ident.to_string
 (* don't care about that serialization fonction, i think *)

let runtime_code_of_accs ~server_renaming ~client_renaming accs =
  List.map
    (fun (i,e,acc) ->
       let {
         acc_rpc_deps;
         acc_type_deps;
         acc_rpc_def;
         acc_type_def;
         acc_ident_deps;
         acc_rec_ident_deps;
         acc_fun_action_deps
       } = acc in
       let defines =
         (match acc_rpc_def, acc_type_def with
          | [], [] -> `nothing
          | [s], [] -> `rpc s
          | [], [s] -> `type_ s
          | _ ->
              Printf.printf "ident: %s, rpc:%d, type:%d\n%!"
                (Ident.to_string i)
                (List.length acc_rpc_def) (List.length acc_type_def);
              assert false
         ) in {
         ident = Some (ident_to_string i);
         client_equivalent =
           (try
              let i = QmlRenamingMap.new_from_original client_renaming (QmlRenamingMap.original_from_new server_renaming i) in
              let s = JsPrint.string_of_ident (JsAst.ExprIdent i) in
              Some s
            with Not_found -> None);
         defines = defines;
         ident_deps = acc_fun_action_deps @ List.map ident_to_string (IdentSet.elements (IdentSet.union acc_ident_deps acc_rec_ident_deps));
         rpc_deps = acc_rpc_deps;
         type_deps = acc_type_deps;
         root = (defines = `nothing && acc_fun_action_deps = [] && is_root e)
       }
    ) accs

module Binary = struct

  let ser_int b i =
    (* we need to make sure that the length of an integer is fixed (or predictable at least) *)
    (* big bytes first *)
    for j = 3 downto 0 do
      Buffer.add_char b (Char.chr ((i lsr (j*8)) mod 256));
    done
  let ser_string b s =
    ser_int b (String.length s);
    Buffer.add_string b s

  let ser_option ser_a b = function
    | None -> Buffer.add_char b '\000'
    | Some a -> Buffer.add_char b '\001'; ser_a b a
  let ser_list ser_a b l =
    ser_int b (List.length l);
    List.iter (fun a -> ser_a b a) l
  let ser_bool b bool =
    Buffer.add_char b (if bool then '\001' else '\000')

  let ser_rpc b s =
    (* same escaping as in qmljs_serializer! *)
    ser_string b (JsPrint.escape_string s)
  let ser_type b s =
    (* same escaping as in qmljs_serializer! *)
    ser_string b (JsPrint.escape_string s)
  let ser_defines b = function
    | `nothing -> Buffer.add_char b '\000'
    | `rpc s -> Buffer.add_char b '\001'; ser_rpc b s
    | `type_ s -> Buffer.add_char b '\002'; ser_type b s
  let ser_ident b o =
    ser_option ser_string b o
  let ser_ident_deps b l =
    ser_list ser_string b l
  let ser_rpc_deps b l =
    ser_list ser_rpc b l
  let ser_type_deps b l =
    ser_list ser_type b l
  let ser_root = ser_bool
  let ser_code_elt b {ident; client_equivalent; defines; ident_deps; rpc_deps; type_deps; root} =
    (* alphabetic order of fields *)
    ser_ident b client_equivalent;
    ser_defines b defines;
    ser_ident b ident;
    ser_ident_deps b ident_deps;
    ser_root b root;
    ser_rpc_deps b rpc_deps;
    ser_type_deps b type_deps
  let ser_code b code =
    ser_list ser_code_elt b code
end

module Code =
struct

  type env = {
    gamma : QmlTypes.gamma;
    stdlib_gamma : QmlTypes.gamma;
    annotmap : QmlAst.annotmap;
    val_ : string -> Ident.t;
  }

  let cons = ref QmlAstCons.untyped_cons

  let empty_env = {
    gamma =  QmlTypes.Env.empty;
    stdlib_gamma = QmlTypes.Env.empty;
    annotmap = QmlAnnotMap.empty;
    val_ = (function _ -> assert false);
  }

  let env = ref empty_env

  let gen_list gen_a l =
    !cons#directive `llarray (List.map gen_a l) []

  let gen_string s =
    !cons#string s

  let gen_bool = !cons#bool

  let gen_option gen_a = function
    | None -> !cons#none ()
    | Some e -> !cons#some (gen_a e)

  let gen_ident oident =
    gen_option gen_string oident

  let gen_ident_deps idents =
    gen_list gen_string idents

  let gen_root root =
    let create = !env.val_ Opacapi.ServerReference.create in
    let tsc = QmlTypes.Env.Ident.find create !env.stdlib_gamma in
    let ty = QmlTypes.Scheme.instantiate tsc in
    let create = !cons#ident create ty in
    !cons#apply create [gen_bool root]

  let gen_rpc rpc =
    gen_string (JsPrint.escape_string rpc)

  let gen_type type_ =
    gen_string (JsPrint.escape_string type_)

  let gen_defines = function
    | `nothing -> !cons#record ["nothing", !cons#cheap_void]
    | `rpc s ->   !cons#record ["rpc",     gen_rpc s]
    | `type_ s -> !cons#record ["type",    gen_type s]

  let gen_rpc_deps rpc_deps =
    gen_list gen_rpc rpc_deps

  let gen_type_deps type_deps =
    gen_list gen_type type_deps

  let gen_code_elt {ident; client_equivalent; defines; ident_deps; rpc_deps; type_deps; root} =
    !cons#record [
      ("client_equivalent", gen_ident client_equivalent);
      ("defines"          , gen_defines defines);
      ("ident"            , gen_ident ident);
      ("ident_deps"       , gen_ident_deps ident_deps);
      ("root"             , gen_root root);
      ("rpc_deps"         , gen_rpc_deps rpc_deps);
      ("type_deps"        , gen_type_deps type_deps);
    ]

  let generate env_ code =
    let c, e =  QmlAstCons.make_typed_cons env_.gamma env_.annotmap in
    cons := c;
    env := env_;
    let expr = gen_list gen_code_elt code in
    let _gamma, annotmap = e () in
    `code expr, annotmap

end

let generate_register ~gamma ~stdlib_gamma ~annotmap ~val_ ~server_code =
  let ident = val_ Opacapi.Core_server_code.register_server_code in
  let tsc = QmlTypes.Env.Ident.find ident stdlib_gamma in
  let ty = QmlTypes.Scheme.instantiate tsc in
  let annotmap, fun_ = QmlAstCons.TypedExpr.ident annotmap ident ty in
  let annotmap, to_register =
    match server_code with
    | `string s ->
        let annotmap, string = QmlAstCons.TypedExpr.string annotmap s in
        let annotmap, package =
          QmlAstCons.TypedExpr.string annotmap (ObjectFiles.get_current_package_name ())
        in
        QmlAstCons.TypedExpr.record annotmap ["adhoc", string; "package_", package]
    | `code e ->
        let annotmap, package =
          QmlAstCons.TypedExpr.string annotmap (ObjectFiles.get_current_package_name ())
        in
        QmlAstCons.TypedExpr.record annotmap ["adhoc_e", e; "package_", package]
  in
  let annotmap, app = QmlAstCons.TypedExpr.apply gamma annotmap fun_ [to_register] in
  let dummy_ident = Ident.next "server_ast" in
  let tyvoid = Q.TypeRecord (Q.TyRow ([], None)) in
  let tsc_void = QmlTypes.Scheme.quantify tyvoid in
  let gamma = QmlTypes.Env.Ident.add dummy_ident tsc_void gamma in
  IdentSet.singleton ident, gamma, annotmap, Q.NewVal (Annot.nolabel "pass_GenerateServerAst", [dummy_ident, app])

let _outputer oc ast =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a%!" pp_code ast

let generate_binary rc =
  let b = Buffer.create 1000 in
  Binary.ser_code b rc;
  let string = Buffer.contents b in
  #<If:SERVER_SERIALIZE$contains "overhead">
    Printf.printf "length: %d\n%!" (String.length string);
    let r = ref 0 in
      for i = 0 to String.length string - 1 do
        if string.[i] < '\005' then incr r
      done;
      Printf.printf "overhead: %d, %.2f%%\n%!" !r (100. *. float !r /. float (String.length string))
    #<End>;
  `string string

let generate_code ~annotmap ~gamma ~stdlib_gamma ~val_ rc =
  Code.generate {Code. val_; annotmap; stdlib_gamma; gamma} rc

let process ~kind ~annotmap ~stdlib_gamma ~gamma ~val_ ~generate ~server_renaming ~client_renaming ~code =
  match ObjectFiles.compilation_mode () with
  | `init -> gamma, annotmap, code
  | `prelude | `linking | `compilation ->
  let loaded_env = R.load () in
  let (rev_qml, acc, env) = process_code ([], [], loaded_env) code in
  R.save ~loaded_env ~env;
  let qml = List.rev rev_qml in
  if generate &&
     (try ignore (val_ Opacapi.Core_server_code.register_server_code); true
      with Not_found -> false) then (
    let rc = runtime_code_of_accs ~server_renaming ~client_renaming acc in
    #<If:SERVER_SERIALIZE>
      ignore (PassTracker.file ~filename:"serverast" _outputer rc);
    #<End>;
    let server_code, annotmap =
      match kind with
      | `adhoc -> generate_binary rc, annotmap
      | `ast   -> generate_code ~annotmap ~gamma ~stdlib_gamma ~val_ rc
    in
    let deps, gamma, annotmap, code_elt =
      generate_register ~gamma ~stdlib_gamma ~annotmap ~val_ ~server_code
    in
    (* for the first package, we need to insert the code_elt at the end :/ *)
    gamma, annotmap, QmlAstUtils.Code.insert ~deps ~insert:[code_elt] qml
     )
  else gamma, annotmap, qml


(* FIXME:
   the computation of roots could be finer:
     f(x)(y) = ...
     a = f(1) // a is considered as being a root
*)
