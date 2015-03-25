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
(* @author Rudy Sicard *)
(*
  Can be before renaming,

  For internationalisation, when generating template
  replace @i18n(@string(["computed_",@magic_to_string{"string"),"_yo"]) by
    generated_fun(@magic_to_string{"string")) and collect generated_fun
    or remove @i18n when no internationalisation package is known
  replace @i18n(something) by something(@i18_lang)


  Should be after renaming
  After renaming
  For computed String, when not generating template
  replace @string(["computed_",@magic_to_string{"string"),"_yo"]) by sc_list(["computed_",@magic_to_string{"string"),"_yo"])

  replace @i18n_lang by the relevant library call


  For performance reason we group both @i18n and @string process, so everything is with uid

*)
let (|>) a f = f a

let i18n_pkg_name = "stdlib.core.i18n"
let default_pkg_ext = ".translation"
let linking_pkg_name = "linking"

let i18n_warn =
  WarningClass.create
    ~public:true
    ~name:"i18n"
    ~doc:"All i18n related warnings"
    ~err:false
    ~enable:true
    ()

let i18n_warn_existing =
  WarningClass.create
    ~parent:i18n_warn
    ~public:true
    ~name:"existing-package"
    ~doc:"Warn if the i18n-template is generated while an old version of the template is still there"
    ~err:false
    ~enable:true
    ()

let i18n_warn_missing =
  WarningClass.create
    ~parent:i18n_warn
    ~public:true
    ~name:"missing"
    ~doc:"Warn for missing translation"
    ~err:false
    ~enable:true
    ()

let i18n_warn_missing_template =
  WarningClass.create
    ~parent:i18n_warn_missing
    ~public:true
    ~name:"template"
    ~doc:"Warn if a i18n-template is missing"
    ~err:false
    ~enable:true
    ()

let i18n_warn_missing_translation_package =
  WarningClass.create
    ~parent:i18n_warn_missing
    ~public:true
    ~name:"package"
    ~doc:"Warn if a translation package is missing"
    ~err:true
    ~enable:true
    ()



let warning_set =
  WarningClass.Set.create_from_list [
    i18n_warn;
    i18n_warn_existing;
    i18n_warn_missing_template;
  ]

module SA = SurfaceAst
module SAC = SurfaceAstCons.ExprIdentCons

(* generate a call to directive i18n_lang (or equivalent form *)
(* when cleaning of js code must be done, shoud really use the directive to simplify code analysis *)
let i18n_lang () =
  let i18n_lang = SAC.E.ident  (OpaMapToIdent.val_ Opacapi.I18n.lang) in
  SAC.E.applys i18n_lang []
  (* SAC.D.i18n_lang() *)
  (* SAC.E.string "WTF" *)

(* generate a call to directive intl_locale (or equivalent form *)
(* when cleaning of js code must be done, shoud really use the directive to simplify code analysis *)
(* let intl_locale () =
  let intl_locale = SAC.E.ident  (OpaMapToIdent.val_ Opacapi.Intl.locale) in
  SAC.E.applys intl_locale []
 *)
(** Generate a call to String.flatten with the given expr list as parameter, need with_label. *)
let flattened_string expr_list =
  match expr_list with
  | hd :: [] -> hd
  | hd :: _ ->
    SurfaceAstCons.with_same_pos hd (fun () ->
      let flatten   = SAC.E.ident  (OpaMapToIdent.val_ Opacapi.String.flatten) in
      let expr_list = SAC.E.list   expr_list in
      let flattened = SAC.E.applys flatten [expr_list] in
      flattened
    )
  | _ -> failwith "Empty flattened_string list"

(** Separate constant part and parameter part (inserts) of a string *)
let consts_and_expressions expr_list =
  List.partition (function
  | SA.Const SA.CString _,_ -> true
  | SA.Directive _,_ -> false
  | _ -> assert false
  ) expr_list

(* generate an ident for a computed string that depends on constant part of the string and on inserts positions *)
let translation_fun_ident consts =
  let digest = List.fold_left (fun acc e->
    match e with
    |SA.Const SA.CString s,_ -> Digest.string (acc ^ s)
    |_ -> assert false
  ) "" consts
  in Printf.sprintf "__i18n_%s" (Digest.to_hex digest)

(* adding a source ident here is ok when it is used to generate template file *)
let template_ident_cons = Ident.source

let translation_fun_body_default_case default_expr =
  let default_ident = template_ident_cons "_default" in
  let pat = SAC.P.var default_ident in
  (pat,default_expr)

(* substitute non const string expression by parameters *)
let subst_parameters expr_list parameters =
  let _, expr_list = Base.List.fold_left_map (fun acc e->
    match e with
    |SA.Const SA.CString _,_ -> acc,e
    | _ -> List.tl acc,(List.hd acc)
  ) parameters expr_list
  in expr_list

type ('a,'b) translation_fun = {
  id : string;
  parameters : string list; (* string or something else *)
  initial_expr : ('a,'b) SurfaceAst.expr;
  default_expr : ('a,'b) SurfaceAst.expr;
  body : ('a,'b) SurfaceAst.expr;
}

(* generate a translation body given the number of parameter and initial computed string, need with_label *)
let translation_fun_body initial_expr expr_list ()=
  let (consts,expressions) = consts_and_expressions expr_list in
  let nb_parameters = List.length expressions in
  let parameters = Base.List.init nb_parameters (fun i-> Printf.sprintf "p%d" (i+1)) in
  let args = List.map template_ident_cons parameters in
  let lambda_args    = List.map SAC.P.var   args in
  let string_inserts = List.map SAC.E.ident args in
  let default_expr = SAC.D.string (subst_parameters expr_list string_inserts) in
  let default_pattern_case = translation_fun_body_default_case default_expr in
  let match_ = SAC.E.match_ (i18n_lang()) [default_pattern_case] in
  let body = SAC.E.lambda lambda_args match_ in
  {
    id = translation_fun_ident consts;
    parameters;
    body ; default_expr ; initial_expr
  }

let collect_directives__i18n acc code =
  let fold (acc,v) f = OpaWalk.Code.fold f acc v in
  let process e expr_list = SurfaceAstCons.with_label (snd e)
    (translation_fun_body e expr_list)
  in
  let collect acc code =
    fold(acc,code)(fun acc (e,_) ->
      match e with
      (* @i18n("...")  ==> add template translation package *)
      | SA.Directive (`i18n , [SA.Directive (`string, expr_list, _),_ as e], _) -> (process e expr_list)::acc
      | SA.Directive (`i18n , [SA.Const (SA.CString _)             ,_ as e], _) -> (process e [e]      )::acc
      | _ ->  acc
    )
  in
  collect acc code

let warn_missing_template e i18n_dir i18n_pkg str_ident =
  QmlError.warning ~wclass:i18n_warn_missing_template (QmlError.Context.pos (QmlLoc.pos (snd e)))
    "A translation function is missing (%s),\nplease regenerate translation template packages:\n %a {%a}/*.opa"
    str_ident
    (Base.Format.pp_list "@," Base.Format.pp_print_string) i18n_pkg
    (Base.Format.pp_list "@," Base.Format.pp_print_string) i18n_dir

(* replace @i18n and @string directives by calls to the proper functions (String.flatten or translation) *)
let replace_directives__i18n__string  ~i18n_dir ~i18n_pkg code =
  let enable_i18n = i18n_dir <> [] || i18n_pkg <> [] in
  let map v f = OpaWalk.Code.map_down f v in
  let translate expr_list = SurfaceAstCons.with_same_pos (List.hd expr_list) (fun()->
    let (consts,expressions) = consts_and_expressions expr_list in
    let str_ident = translation_fun_ident consts in
    try
      let ident = OpaMapToIdent.val_no_opacapi_check str_ident in
      let translation_fun_ident = SAC.E.ident  ident in
      let translated = SAC.E.applys translation_fun_ident expressions in
      translated
    with Not_found -> (
      (* template not available, ignore @i18n mode *)
      warn_missing_template (List.hd expr_list) i18n_dir i18n_pkg str_ident;
      flattened_string expr_list
    )
  )
  in
  let process = if enable_i18n then translate else flattened_string in
  let rewrite code =
    map(code)(fun ((e,label) as v) ->
      match e with
      (* "..." ==> String_flatten(...) *)
      | SA.Directive (`string, []       , _) -> SAC.E.string ~label ""
      | SA.Directive (`string, expr_list, _) -> flattened_string expr_list

      (* @i18n("...")  ==> String_flatten(...) OR translate_fun(parameters) *)
      | SA.Directive (`i18n , [SA.Directive (`string, expr_list, _),_], _) -> process expr_list
      | SA.Directive (`i18n , [SA.Const (SA.CString _),_ as string   ], _) -> process [string]

      (* @i18n(expr) => expr(@i18n_lang) *)
      | SA.Directive (`i18n , [expr], _ ) -> SurfaceAstCons.with_label label (fun()->
        SAC.E.applys expr [i18n_lang ()]
      )

      | SA.Directive (`i18n , _ , _ ) -> assert false
      | _ ->  v
    )
  in
  rewrite code

let pos_get_line pos = snd (FilePos.get_one_loc pos)

(* header of one translation entry *)
let generate_template_header f tf =
  let pos = QmlLoc.pos (snd tf.initial_expr) in
  Format.fprintf f "// Template for %s\n" (FilePos.get_file pos);
  Format.fprintf f "// %a\n"  OpaPrint.readable_ident#expr tf.default_expr; (* printing e here can have some drawbacks, need to be solved => use string extracts ? *)
  Format.fprintf f "// string, %d\n" (pos_get_line  pos)


let pp_parameter f s = Base.Format.fprintf f "%s:string" s

(* a translation function in opa, pure string *)
let generate_declaration_custom f tf =
  Format.fprintf f "%s(%a)= match I18n.lang()\n" tf.id (Base.Format.pp_list "," pp_parameter) tf.parameters;
  (*"{123} ->"*)
  Format.fprintf f "  _   -> %a" OpaPrint.readable_ident#expr tf.default_expr

(* same from an AST *)
(*let generate_declaration f tf =
  let pat_id = SurfaceAstCons.with_same_pos tf.initial_expr (fun()-> SAC.P.var (template_ident_cons tf.id)) in
  Format.fprintf f "%a\n\n" OpaPrint.readable_ident#code_elt_node (SA.NewVal ([pat_id,tf.body],false))*)

(* generate the source of a translation package *)
let generate_opa_file name collection target =
  let f = Format.formatter_of_out_channel target in
  Format.fprintf f "package %s\n" name;
  Format.fprintf f "import %s\n" i18n_pkg_name;
  List.iter (fun tf ->
    (* here should check for an already present definition in old file *)
    Format.fprintf f "\n";
    generate_template_header f tf;
    generate_declaration_custom f tf;
    Format.fprintf f "\n\n";
  ) collection

(* TODO, if an Opa file already exists, then parse it and use it to extract code verbatim or make a merge thing *)
(* let _merge_opa_file name collection target old = assert false *)

let generate_po_file collection =
  let f = Format.formatter_of_out_channel stdout in
  List.iter (fun tf ->
    generate_template_header f tf;
    Format.printf "\nmsgid \"%a\"\n" OpaPrint.readable_ident#expr tf.default_expr;
    Format.printf "msgstr \"\"\n\n";
  ) collection

let current_package ?package () =
  let package = match package with
    | Some package -> package
    | None -> ObjectFiles.get_current_package_name()
  in if package="" then linking_pkg_name else package

let default_i18n_package ?package () = (current_package ?package ())^default_pkg_ext

module I18n = OpaEnv.I18n

let need_to_import_package ~options =
  let open I18n.Type in (* for I18n fields *)
  let opt = options.OpaEnv.i18n in
  (opt.pkg<> [] || opt.dir<>[]) && not(opt.template_opa || opt.template_po)

let warn_missing_package initial_package i18n_package =
  QmlError.warning ~wclass:i18n_warn_missing_translation_package
    (QmlError.Context.package  initial_package)
    "Translation package %s is needed (by %s) but not provided"
    i18n_package
    initial_package

let import_package ?package ~exists ~options =
  let open I18n.Type in (* for I18n fields *)
  let should_import =
    match options.OpaEnv.i18n.pkg with
    | [] -> [default_i18n_package ?package ()]
    | v  -> v
  in List.filter (fun i18n_package ->
    if exists i18n_package then true
    else (
      warn_missing_package (current_package ?package()) i18n_package;
      false
    )
  ) should_import

let may_import_package ?package  ~exists ~options =
  if need_to_import_package ~options then (
    let ps = import_package ?package ~exists ~options in
    (*if ps <> [] then Base.Format.printf "I18n: importing %a\n" (Base.Format.pp_list "," Base.Format.pp_print_string) ps;*)
    ps
  ) else []

let backup_existing name =
  try
    let stat = Unix.stat name in
    let nname = Printf.sprintf "%s.%d" name (int_of_float stat.Unix.st_atime) in
    Unix.rename name nname;
    Some(nname)
  with Unix.Unix_error(Unix.ENOENT,_,_) -> None

let with_file conflict name f =
  let name = PathTransform.string_to_mysys name in
  let backup = backup_existing name in
  let c = open_out name in
  (try f c with _ -> ());
  close_out c;
  match backup with
  | None -> ()
  | Some b -> conflict name b


module SAP = SurfaceAstPassesTypes
let replace_directives__i18n__string ~i18n_dir ~i18n_pkg env =
  let lcodeNotUser = replace_directives__i18n__string  ~i18n_dir ~i18n_pkg env.SAP.lcodeNotUser in
  let lcodeUser    = replace_directives__i18n__string  ~i18n_dir ~i18n_pkg env.SAP.lcodeUser    in
  { env with SAP.
    lcodeNotUser = lcodeNotUser;
    lcodeUser    = lcodeUser
  }

let collect_directives__i18n env =
  let acc = collect_directives__i18n []  env.SAP.lcodeNotUser in
  let acc = collect_directives__i18n acc env.SAP.lcodeUser    in
  let cmp_id tf1 tf2 = Pervasives.compare tf1.id tf2.id in
  let cmp_pos =
    let pos tf = FilePos.get_one_loc (QmlLoc.pos (snd tf.initial_expr)) in
    (fun tf1 tf2 -> Pervasives.compare (pos tf1) (pos tf2))
  in
  acc
  |> List.stable_sort cmp_id (* need determist order (wrt initial relative order) for equal elements *)
  |> Base.List.uniq ~cmp:cmp_id (* TODO merge position *)
  |> List.sort cmp_pos

let process_directives__i18n__string ~options env =
  let open I18n.Type in (* for I18n fields *)
  let i18n = options.OpaEnv.i18n in
  if OpaEnv.i18n_template options then (
    begin match collect_directives__i18n env with
    | [] -> ()
    | tf :: _ as collection ->
      let target_dir = match i18n.dir with
        | dir :: _ -> dir
        | [] -> "."
      in
      let target_package = match i18n.pkg with
        | pkg :: _ -> pkg
        | [] -> default_i18n_package ()
      in
(*      Printf.printf "I18n : Translation package name %s\n" target_package;*)
(*      Printf.printf "I18n : Number of translation template %d\n" (List.length collection); flush stdout;*)
      let conflict target diff new_ old_ = (* TODO use merge function here *)
        QmlError.warning ~wclass:i18n_warn_existing (QmlError.Context.pos (QmlLoc.pos (snd tf.initial_expr)))
          "Old version of package %s was backup in %s\nYou should merge with the new version :\n%s %s %s %s | tee %s\n"
          target old_
          diff new_ new_ old_ new_
      in
      let dest_file_opa = Printf.sprintf "%s/%s" target_dir target_package in
      if i18n.template_opa then
        with_file (conflict target_package "meld") (dest_file_opa^".opa")  (generate_opa_file target_package collection);
      if i18n.template_po  then generate_po_file collection;
    end;
    exit 0
  ) else replace_directives__i18n__string ~i18n_dir:i18n.dir ~i18n_pkg:i18n.pkg env
