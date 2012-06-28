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
    @author Sebastien Briais
*)

module List = Base.List
module Q = QmlAst
module ConsT = QmlAstCons.TypedExpr
module ConsU = QmlAstCons.UntypedExpr

let bslclosure = "BslClosure"
let closure_runtime = "CR"
type env_apply_kind = EAK_std | EAK_with_ty
let eak_str = function EAK_std -> "" | EAK_with_ty -> "_with_ty"

let make_closure_name ?(safe=false) ~side ~renaming_server ~renaming_client ident =
  let client_ident =
    match side with
    | `client -> ident
    | `server ->
        try
          let common_ident = QmlRenamingMap.original_from_new renaming_server ident in
          (try QmlRenamingMap.new_from_original renaming_client common_ident
           with Not_found -> ident)
        with Not_found when safe ->
          ident in
  JsPrint.string_of_ident (JsAst.ExprIdent client_ident)

let is_other_side ~side ~renaming_server ~renaming_client ident =
  let renaming, renaming_other_side =
    match side with
    | `client -> renaming_client, renaming_server
    | `server -> renaming_server, renaming_client in
  match QmlRenamingMap.original_from_new_opt renaming ident with
  | None -> false (* No original ident perhaps a local function
                     (lambda lifting is after slicing)*)
  | Some oident ->
      match QmlRenamingMap.new_from_original_opt renaming_other_side oident with
      | None -> false
      | Some _ -> true

let mk_identifier ~typed ~side ~renaming_server ~renaming_client gamma annotmap ident =
  (* presumably, any string that contains the filename should be ok
   * (the filename will certainly be needed for separate compilation) *)
  let name = make_closure_name ~safe:true ~side ~renaming_server ~renaming_client ident in
  let is_other_side = is_other_side ~side ~renaming_server ~renaming_client ident in
  let annotmap, s = ConsT.string annotmap name in
  let annotmap, b =
    if typed then
      ConsT.bool_no_named_type (annotmap, gamma) is_other_side
    else
      annotmap, ConsU.bool is_other_side in
  annotmap, s, b

let initial_needed_env_apply = -1
let initial_needed_args_apply = -1
let needed_env_apply = ref initial_needed_env_apply
let needed_env_apply_with_ty = ref initial_needed_env_apply
let needed_args_apply = ref initial_needed_args_apply
let reset () =
  needed_env_apply := initial_needed_env_apply;
  needed_env_apply_with_ty := initial_needed_env_apply;
  needed_args_apply := initial_needed_args_apply

let env_apply_ident ~eak = Printf.sprintf "clos_env_%s%d" (eak_str eak)
let env_apply_exprident ~eak d = Ident.fake_source (env_apply_ident ~eak d)

let args_apply_ident = Printf.sprintf "clos_args_%d"
let args_apply_exprident d = Ident.fake_source (args_apply_ident d)
let export_ident = Printf.sprintf "clos_export_%d"

let type_of_env_apply gamma annotmap e env_size =
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
  match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
  | Q.TypeArrow (params,return) ->
      let env, args = List.split_at env_size params in
      Q.TypeArrow (ty :: env, Q.TypeArrow (args,return))
  | _ -> assert false

let type_of_args_apply gamma annotmap e =
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
  match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
  | Q.TypeArrow (params,return) ->
      Q.TypeArrow (ty :: params, return)
  | _ ->
      OManager.i_error "@[<v2>QmlClosure: applying an expression with a non arrow type:@ @[<v2>type:@ %a@]@ @[<v2>expr:@ %a@]@]@." QmlPrint.pp#ty ty QmlPrint.pp#expr e

let env_apply ~env_size ~env_ty_size ~eak ~typed (gamma,annotmap) e es =
  needed_env_apply := max env_size !needed_env_apply;
  needed_env_apply_with_ty := max env_ty_size !needed_env_apply;
  let annot, annotmap =
    if typed then
      let annot = Annot.next () in
      let ty = type_of_env_apply gamma annotmap e env_size in
      let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
      annot, annotmap
    else
      Annot.next (), annotmap in
  let label = Annot.make_label annot (Q.Pos.expr e) in
  let ident = Q.Directive (label, `backend_ident (env_apply_ident ~eak env_size), [ConsU.ident (env_apply_exprident ~eak env_size)], []) in
  if typed then
    ConsT.apply gamma annotmap ident (e :: es)
  else
    annotmap, ConsU.apply ident (e :: es)

let args_apply ~typed (gamma,annotmap) e es =
  let args_size = List.length es in
  needed_args_apply := max args_size !needed_args_apply;
  let annot, annotmap =
    if typed then
      let annot = Annot.next () in
      let ty = type_of_args_apply gamma annotmap e in
      let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
      annot, annotmap
    else
      Annot.next (), annotmap in
  let label = Annot.make_label annot (Q.Pos.expr e) in
  let ident = Q.Directive (label, `backend_ident (args_apply_ident args_size), [ConsU.ident (args_apply_exprident args_size)], []) in
  if typed then
    ConsT.apply gamma annotmap ident (e :: es)
  else
    annotmap, ConsU.apply ident (e :: es)

let rewrite_expr ~typed bp_typer ~side ~renaming_client ~renaming_server gamma annotmap e =
  QmlAstWalk.Expr.self_traverse_foldmap
    (fun self tra annotmap e ->
       match e with
       | Q.Directive (label, (`closure_create (f, _arity, _tsc) as d), _, _)
       | Q.Directive (label, (`closure_create_no_function (f, _arity, _tsc) as d), _, _) ->
           (* FIXME: this part generates untyped code, but this part of the code is
            * so simple that who needs type anyway ? *)
           let bp =
             match d with
             | `closure_create _ -> Opacapi.Opabsl.BslClosure.create_and_register
             | `closure_create_no_function _ -> Opacapi.Opabsl.BslClosure.create_no_function_and_register in
           let pos = Annot.pos label in
           let annotmap, ccreate = QmlAstCons.TypedExpr.bypass ~pos annotmap bp (bp_typer bp) in
           let annotmap, identifier, is_other_side = mk_identifier ~typed ~side ~renaming_client ~renaming_server gamma annotmap f in
           let f = QmlAstCons.UntypedExpr.ident f in
           let arity = QmlAstCons.UntypedExpr.int _arity in
           let args =
             let common_args = [arity;identifier;is_other_side] in
             match d with
             | `closure_create _ -> f :: common_args
             | `closure_create_no_function _ -> common_args in
           let e = QmlAstCons.UntypedExpr.apply ccreate args in
           tra annotmap e
       | Q.Directive (label, `closure_define_function (clos, code, _tsc), _, _) ->
           (* FIXME: same as above *)
           let bp = Opacapi.Opabsl.BslClosure.define_function in
           let pos = Annot.pos label in
           let annotmap, define = QmlAstCons.TypedExpr.bypass ~pos annotmap bp (bp_typer bp) in
           let e_clos = QmlAstCons.UntypedExpr.ident clos in
           let e_code = QmlAstCons.UntypedExpr.ident code in
           let e = QmlAstCons.UntypedExpr.apply define [e_clos; e_code] in
           annotmap, e
      | Q.Directive (label, `partial_apply missing, Q.Directive (_, `closure_apply, e :: es, _) :: tys , _) ->
          let env_size = List.length es in
          let env_ty_size = List.length tys in
          let eak,es = if tys == [] then EAK_std,es else EAK_with_ty,es@tys in
          let annotmap, e = self annotmap e in
          let annotmap, es = List.fold_left_map self annotmap es in
          let annotmap, e = env_apply ~env_size ~env_ty_size ~eak ~typed (gamma,annotmap) e es in
          let e = Q.Directive (label,`partial_apply missing, [e], []) in
          annotmap,e
      | Q.Directive (_, `partial_apply _, _, _) -> assert false
      | Q.Directive(_, `closure_apply, e :: es, _) ->
          let annotmap, e = self annotmap e in
          let annotmap, es = List.fold_left_map self annotmap es in
          let annotmap, e = args_apply ~typed (gamma,annotmap) e es in
          annotmap,e
      | _ -> tra annotmap e
    ) annotmap e

let rewrite_code ~typed bymap ~side ~renaming_client ~renaming_server (gamma,annotmap) code =
  QmlAstWalk.CodeExpr.fold_map (rewrite_expr ~typed bymap ~side ~renaming_client ~renaming_server gamma) annotmap code

let populate_gamma gamma =
  gamma

let process_code ~typed ~side ~renaming_client ~renaming_server bymap gamma annotmap code =
  let bp_typer key = Option.get (bymap key) in
  let gamma = populate_gamma gamma in
  let annotmap,code = rewrite_code ~typed bp_typer ~side ~renaming_client ~renaming_server (gamma,annotmap) code in
  (gamma,annotmap),code

(*----------------------------------------*)
(*------- dynamic generation of code -----*)
(*----------------------------------------*)

(* Example of generated code
function clos_args_1(f,x0) {
    if (f.takes_array_arg) return f.func([x0])
    var args = f.args
    switch (args.length) {
    case 0: return f.func(x0)
    case 1: return f.func(args[0],x0)
    case 2: return f.func(args[0],args[1],x0)
    case 3: return f.func(args[0],args[1],args[2],x0)
    default: return args_apply(f,[x0]);
   }
*)
let js_size_of_clos_args = 8 (* arbitrary *)
let js_clos_args n =
  let clos_args = args_apply_exprident n in
  let clos_args = JsCons.Ident.ident clos_args in
  let fun_ = JsCons.Ident.native "f" in
  let args = JsCons.Ident.native "args" in
  let params = List.init n (fun i -> JsCons.Ident.native (Printf.sprintf "x%d" i)) in
  let params_expr () = List.map JsCons.Expr.ident params in
  JsCons.Statement.function_ clos_args (fun_ :: params) [
    JsCons.Statement.if_no_else
      (JsCons.Expr.dot (JsCons.Expr.ident fun_) "takes_array_arg")
      (JsCons.Statement.return (JsCons.Expr.call ~pure:false (JsCons.Expr.dot (JsCons.Expr.ident fun_) "func") [JsCons.Expr.list (params_expr ())]));
    JsCons.Statement.var args ~expr:(JsCons.Expr.dot (JsCons.Expr.ident fun_) "args");
    JsCons.Statement.switch (JsCons.Expr.dot (JsCons.Expr.ident args) "length")
      (List.init js_size_of_clos_args
         (fun i ->
            let test = JsCons.Expr.int i in
            let env = List.init i (fun j -> JsCons.Expr.hashref (JsCons.Expr.ident args) (JsCons.Expr.int j)) in
            let all_params = env @ params_expr () in
            let call = JsCons.Expr.call ~pure:false (JsCons.Expr.dot (JsCons.Expr.ident fun_) "func") all_params in
            let rhs = JsCons.Statement.return call in
            test, rhs
         )
      )
      ~default:(JsCons.Statement.return (JsCons.Expr.call ~pure:false (JsCons.Expr.native "args_apply") [JsCons.Expr.ident fun_; JsCons.Expr.list (params_expr ())]))
  ]

let js_clos_env n =
  let clos_env = env_apply_exprident ~eak:EAK_std n in
  let clos_env = JsCons.Ident.ident clos_env in
  let fun_ = JsCons.Ident.native "f" in
  let params = List.init n (fun i -> JsCons.Ident.native (Printf.sprintf "x%d" i)) in
  let mkfield field = field, JsCons.Expr.dot (JsCons.Expr.ident fun_) field in
  JsCons.Statement.function_ clos_env (fun_ :: params) [
    JsCons.Statement.return (
      JsCons.Expr.obj [
        mkfield "func";
        mkfield "arity";
        mkfield "identifier";
        mkfield "takes_array_arg";
        "args", JsCons.Expr.list (List.map JsCons.Expr.ident params);
      ]
    )
  ]

module Gen =
struct
  let rec repeat_aux f i n =
    if i = n then () else (f i; repeat_aux f (i+1) n)
  let repeat f n =
    repeat_aux f 0 n

  let array ~lang buffer make n =
    let pp fmt = Printf.bprintf buffer fmt in
    match lang,n with
    | `caml, 0 -> pp "[||]"
    | `caml, 1 -> pp "(Obj.magic {tuple1 = %s} : Obj.t array)" (make 0)
    | `caml, _ -> pp "(Obj.magic (%s" (make 0); repeat_aux (fun d -> pp ", %s" (make d)) 1 n; pp ") : Obj.t array)"
    | `js, 0 -> pp "[]"
    | `js, _ -> pp "[%s" (make 0); repeat_aux (fun d -> pp ", %s" (make d)) 1 n; pp "]"

  (* caml only *)
  let pat_array buffer make n =
    let pp fmt = Printf.bprintf buffer fmt in
    match n with
    | 0 -> pp "[||]"
    | _ -> pp "[|%s" (make 0); repeat_aux (fun d -> pp "; %s" (make d)) 1 n; pp "|]"
end

(* [caml_generate_apply buffer N] generates
   [let clos_applyN f x0 x1 ... xN-1 =
      BslClosure.apply f (Obj.magic (x0,x1,...,xN-1) : Obj.t array)
      (* using a tuple to avoid the float array optimization *)
   ]
*)
let init_code ~lang buffer =
  let pp fmt = Printf.bprintf buffer fmt in
  match lang with
  | `caml -> pp "type 'a tuple1 = { tuple1 : 'a }\n"
  | `js -> ()

(* could generated more but then the code can increase dramatically *)
let max_opt = 9
let caml_apply buffer n =
  let pp fmt = Printf.bprintf buffer fmt in
  match n with
  | 1 ->
    (* apply1 is already written in the runtime library *)
    pp "let %s = %s.args_apply1\n\n" (args_apply_ident n) closure_runtime
  | _ ->
    let arg = Printf.sprintf "a%d" in
    let env = Printf.sprintf "e%d" in
    pp "let %s closure %s =\n" (args_apply_ident n) (String.concat " " (List.init n arg));
    if n > max_opt then (
      pp " %s.args_apply closure " closure_runtime;
      Gen.array ~lang:`caml buffer arg n;
      pp "\n\n"
    ) else (
      pp "match closure.%s.args with\n" closure_runtime;
      for i = 0 to max_opt do
        pp "    | ";
        Gen.pat_array buffer env i;
        pp " -> (Obj.magic closure.%s.func) " closure_runtime;
        if i = 0 && n = 0 then (
          pp "()"
        ) else (
          pp "%s%s%s" (String.concat " " (List.init i env)) (if n = 0 then "" else " ") (String.concat " " (List.init n arg))
        );
        pp "\n"
      done;
      pp "    | _ -> %s.args_apply closure" closure_runtime;
      Gen.array ~lang:`caml buffer arg n;
      pp "\n\n"
    )

let generate_env_apply ~eak ~lang buffer n =
  let pp fmt = Printf.bprintf buffer fmt in
  let x = Printf.sprintf "x%d" in
  let ty = Printf.sprintf "ty%d" in
  match lang with
  | `caml ->
    let x_params = String.concat " " (List.init n x) in
    let opt_ty_params = if eak==EAK_with_ty then String.concat " " (List.init n ty) else "" in
    pp "let %s f %s %s =\n" (env_apply_ident ~eak n) x_params opt_ty_params;
    pp " %s.env_apply%s f " closure_runtime (eak_str eak);
    Gen.array ~lang buffer x n;
    begin match eak with
    | EAK_std -> ()
    | EAK_with_ty -> Gen.array ~lang buffer ty n (* do not scale to scheme type completion easily *)
    end;
    pp "\n"

  | `js ->
      pp "function %s(f%s%s) {\n" (env_apply_ident ~eak:EAK_std n)  (if n = 0 then "" else ",") (String.concat "," (List.init n x));
      pp "  return env_apply(f,";
      Gen.array ~lang buffer x n;
      pp ");\n}\n"

let generate_args_apply ~lang buffer n =
  let pp fmt = Printf.bprintf buffer fmt in
  let x = Printf.sprintf "x%d" in
  match lang with
  | `caml ->
      #<If:CLOSURE_OPT$maxlevel (-1)>
        pp "let %s f %s =\n" (args_apply_ident n) (String.concat " " (List.init n x));
        pp " %s.args_apply f " closure_runtime;
        Gen.array ~lang buffer (Printf.sprintf "x%d") n;
        pp "\n"
      #<Else>
        caml_apply buffer n
      #<End>
  | `js ->
      pp "function %s(f%s%s) {\n" (args_apply_ident n) (if n = 0 then "" else ",") (String.concat "," (List.init n x));
      pp "  return args_apply(f,";
      Gen.array ~lang buffer x n;
      pp ");\n}\n"

let generate_export ~lang buffer i =
  let pp fmt = Printf.bprintf buffer fmt in
  match lang with
  | `caml ->
      let arg = Printf.sprintf "a%d" in
      pp "let %s clos =\n" (export_ident i);
      let args = (String.concat " " (List.init i arg)) in
      pp "  fun %s -> %s clos%s\n" (if args = "" then "()" else args) (args_apply_ident i) (if args = "" then "" else " "^args)
  | `js -> ()

let generate_applys ?at_least lang =
  let n = !needed_args_apply in
  let n =
    match at_least with
    | None -> n
    | Some n' -> max n n' in
  let buffer = Buffer.create 100 in
  init_code ~lang buffer;
  for i = 0 to n do
    generate_args_apply ~lang buffer i
  done;
  for i = 0 to n do
    generate_export ~lang buffer i
  done;
  let n = !needed_env_apply in
  for i = 0 to n do
    generate_env_apply ~eak:EAK_std ~lang buffer i
  done;
  for i = 1 to !needed_env_apply_with_ty do
    generate_env_apply ~eak:EAK_with_ty ~lang buffer i
  done;
  reset ();
  Buffer.contents buffer

let generate_applys_js ?at_least () =
  let n_args = !needed_args_apply in
  let n_args =
    match at_least with
    | None -> n_args
    | Some n -> max n_args n in
  let n_env = !needed_env_apply in
  List.init (n_args+1) (fun n -> args_apply_ident n, js_clos_args n) @
  List.init (n_env+1) (fun n -> env_apply_ident ~eak:EAK_std  n, js_clos_env n) @
  List.init (n_env+1) (fun n -> env_apply_ident ~eak:EAK_with_ty  n, js_clos_env n)
