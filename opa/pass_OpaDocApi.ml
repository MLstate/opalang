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
(* depends in base *)
module Char = BaseChar
module Format = BaseFormat
module List = BaseList
module String = BaseString

let (|>) = InfixOperator.(|>)

(* refactoring in progress *)

(* shorthands *)
module E = OpaEnv
module P = Passes
module SA = SurfaceAst
module Q = QmlAst

(* type alias for clarty *)
type filename = string
type pos = int
type path = string list

type short_coll = path * Annot.label
type collection = path * filename * pos * QmlAst.ty


(*
  traversed directive for finding fields of a module from a toplevel value
  to add doc type api directives
*)
type ('a, 'b, 'c) traversed_directive = [
| `coerce
| `deprecated
| `local of 'a
| Q.slicer_directive
]

(**
   Merge access directive
*)
let merge_access (acc : SurfaceAst.access_directive) ( dir : SurfaceAst.access_directive ) : SurfaceAst.access_directive =
  match acc, dir with
  | `private_, _ -> acc
  | _, `private_ -> dir
  | `package, _ -> acc
  | _, `package -> dir
  | _ -> acc

(**
   extract annotation type of top-level values and type of their eventual fields
   (if the final expression of the top level value is a record)
*)
let add_code_doctype sa_code =
  let keep_local modify e =
    match fst e with
      | SA.Directive(`local _ as l,[e],c) -> SA.Directive(l, [modify e], c), snd e
      | _ -> assert false
  in
  let final_expr_sub_1 main_e sube =
    let main_e, annot = main_e in
    (match main_e with
     | SA.Lambda(z,_)             -> SA.Lambda(z,sube)
     | SA.Directive(z0,[_],z2)    -> SA.Directive(z0,[sube],z2)
     | SA.LetIn(b, z, _)          -> SA.LetIn(b, z, sube)
     | _ -> assert false
    ), annot
  in
  (**
     add doctype directives to sub modules fields,
     and returns the accessibility of the toplevel expression
  *)
  let rec add_e_doctype access path e : SA.access_directive * (_, _) SA.expr =
    match fst e with

    (* access *)
    | SA.Directive ((#SA.access_directive as sub_access), [ sube ], _) ->
        let access = merge_access access sub_access in
        let access, sube = add_e_doctype access path sube in
        access, final_expr_sub_1 e sube

    (* go through for finding fields of modules *)
    | SA.Lambda(_, sube)
    | SA.Directive (#traversed_directive, [ sube ], _)
      ->
        let access, sube = add_e_doctype access path sube in
        access, final_expr_sub_1 e sube

    (* collect here and recurse *)
    | SA.LetIn(x, l, sube) ->
        let access, sube = add_e_doctype access path sube in
        access, (SA.LetIn(x, l, sube), (snd e))

    | SA.Directive(`module_ , [SA.Record( fields ), annot],ty)
        when SurfaceAstHelper.Record.is_module e ->
        (* since module header can be rewritten , we collect the annot here *)
        let new_record = SA.Record (
          List.map (
            fun (f,e) ->
              let npath = path @ [f] in
              #<If:OPADOC> OManager.printf "Adding %s@." (String.concat "." npath) #<End>;
              let modify e =
                let access, sube = add_e_doctype access npath e in
                keep_local
                  (SurfaceAstCons.ExprIdentCons.D.doctype ~label:(snd e) ~access npath)
                  sube
              in
              (f, modify e)
          ) fields )
        in
        access, (SA.Directive(`module_, [new_record, annot], ty), snd e)

    (* ignore *)
    | _ -> access, e
  in
  let rec add_patt_doctype ((name,e) as decl) =
    let rec getname name =
      match fst name with
      | SA.PatCoerce (n,_) -> getname n
      | SA.PatVar n
      | SA.PatAs (_,n) -> Some n.SA.ident
      | _ -> None
    in
    match getname name with
    |Some n ->
       let path = [Ident.original_name n] in
       #<If:OPADOC> OManager.printf "Fold %s@." (Ident.original_name n) #<End>;
       let access, sube = add_e_doctype `public path e in
       name, (SurfaceAstCons.ExprIdentCons.D.doctype ~label:(snd e) ~access path) sube
    | _ -> decl
  in
  List.tail_map
    (function
     | SA.NewVal (l, b), annot ->
         SA.NewVal (List.map add_patt_doctype l, b), annot
     | t -> t) sa_code

let collect_type_doctype sa_code =
  List.rev (List.fold_left (fun acc -> function (SA.NewType tds, _) -> tds @ acc | _ -> acc) [] sa_code)


(**
   output top-level value type description for opa-doc
*)
let process_opa ~(options : E.opa_options) env =
  if options.E.generate_interface || options.E.generate_interface_and_compile then
    { env with Passes.
        sa_lcode = add_code_doctype env.P.sa_lcode ;
        sa_doc_types = collect_type_doctype env.P.sa_lcode }
  else env


(* =========================================================== *)
(* SECOND PART; OpaDocApiGeneration *)
(* =========================================================== *)


(**
   This function filter the code, by removing [`doctype] diretives
   introduced by the first part (pass_AddDocApiDirecitves).

   It returns the filtered annotmap, and collect all the decorated path,
   in the form of an assoc list, binding pathes of decorated elements
   with there label (so that we can find their types and position)
*)
let remove_code_doctype annotmap (qmlAst : QmlAst.code) :
    (QmlAst.annotmap * (string list * Annot.label * QmlAst.doctype_access_directive) list) * QmlAst.code
    =
  let rec remove_expr_doctype (annotmap, acc) e =
    match e with
    | Q.Directive (label, `doctype (path, access), [sube], []) ->
        let annot_e = Annot.annot label in
        let tsc_opt =
          QmlAnnotMap.find_tsc_opt annot_e annotmap in
        let tsc_inst_opt =
          QmlAnnotMap.find_tsc_inst_opt annot_e annotmap in
        let annotmap =
          QmlAnnotMap.remove_tsc annot_e annotmap in
        let annotmap =
          QmlAnnotMap.remove_tsc_inst annot_e annotmap in
        let sube_tsc_opt =
          QmlAnnotMap.find_tsc_opt annot_e annotmap in
        let sube_tsc_inst_opt =
          QmlAnnotMap.find_tsc_inst_opt annot_e annotmap in
        let tsc_opt =
          Option.merge (fun _ _ -> assert false) tsc_opt sube_tsc_opt in
        let tsc_inst_opt =
          Option.merge
            (fun _ _ -> assert false) tsc_inst_opt sube_tsc_inst_opt in
        let annot_sube = QmlAst.QAnnot.expr sube in
        let annotmap =
          QmlAnnotMap.add_tsc_opt annot_sube tsc_opt annotmap in
        let annotmap =
          QmlAnnotMap.add_tsc_inst_opt annot_sube tsc_inst_opt annotmap in
        ((annotmap, (path, (QmlAst.Label.expr sube), access) :: acc), sube)
    | _ -> ((annotmap, acc), e) in
  let remove_patt_doctype acc e =
    QmlAstWalk.Expr.foldmap_down remove_expr_doctype acc e
  in
  QmlAstWalk.CodeExpr.fold_map remove_patt_doctype (annotmap, []) qmlAst


module Api =
struct

  (**
     This module defines the ocaml structures corresponding to the opa structures
     defined in [opaDocTy.opa].

     This is not a code duplication, but an easyer way to ensure than this pass
     generates serialized values corresponding to the value defined in opa.

     The documentation of these types is in the opa code.
  *)

  type ty = QmlAst.ty

  type pkg = string

  type path = string list

  (**
     <!> The fields are there prefixed by ["value_"] but not in opa
  *)
  type value = {
    value_ty : ty ;
    value_visibility : QmlAst.doctype_access_directive ;
  }

  (**
     <!> In opa, the fields contained in the type defs are flattened.
     We keep it like this to possibly export for opadoc more infos
     than infos contained in the QmlAst.typedef
  *)
  type type_def = {
    type_def : QmlAst.typedef
  }

  type code_elt =
    | Value of value
    | Type of type_def

  type file = string
  type pos = int

  type entry = {
    pkg : pkg ;
    path : path ;
    code_elt : code_elt ;
    fname : file ;
    pos : pos ;
  }

  module Entry :
  sig

    (**
       Build a value type from collected informations
    *)

    (**
       Values
    *)
    val value :
      gamma:QmlTypes.gamma ->
      annotmap:QmlAst.annotmap ->
      (string list * Annot.label * QmlAst.doctype_access_directive -> entry)

    (**
       Types definitions
    *)
    val type_def :
      gamma:QmlTypes.gamma ->
      annotmap:QmlAst.annotmap ->
      (Ident.t SA.typedef -> entry)

    (**
       Pretty printing api-txt (for debuging)
    *)
    val pp : Format.formatter -> entry -> unit
  end =
  struct

    let make_entry () =
      let pkg = ObjectFiles.get_current_package_name () in
      let make ~path ~filepos ~code_elt =
        let fname = FilePos.get_file filepos in
        let pos = FilePos.get_first_char filepos in
        let entry = {
          pkg ;
          path ;
          code_elt ;
          fname ;
          pos ;
        } in
        entry
      in
      make

    let value ~gamma:_ ~annotmap =
      let make_entry = make_entry () in
      let value (path, label, visibility) =
        let filepos = Annot.pos label in
        let annot = Annot.annot label in
        let ty = QmlAnnotMap.find_ty annot annotmap in
        let code_elt = Value {
          value_ty = ty ;
          value_visibility = visibility ;
        } in
        make_entry ~path ~filepos ~code_elt
      in
      value

    let type_def ~gamma:_ ~annotmap:_ =
      let make_entry = make_entry () in
      let type_ typedef =
        let typedef, loc = typedef in
        let filepos = loc.QmlLoc.pos in
        let SA.Typeident ident = typedef.SA.ty_def_name in
        let ident = Ident.original_name ident in
        let path = [ ident ] in
        let typedef = OpaToQml.UidsOpaToQml.typedef typedef in
        let code_elt = Type {
          type_def = typedef ;
        } in
        make_entry ~path ~filepos ~code_elt
      in
      type_

    let pp_path_elt fmt elt =
      let elt =
        if String.length elt > 0 && Char.is_alpha elt.[0] && String.is_word elt
        then elt
        else "`" ^ elt ^ "`"
      in
      Format.pp_print_string fmt elt

    let pp_light_ident = new QmlPrint.light_ident_printer

    let pp_value_visibility fmt = function
      | `public -> ()
      | `private_ -> Format.pp_print_string fmt "@private "
      | `package -> Format.pp_print_string fmt "@package "

    let pp_value fmt path value =
      let visibility = value.value_visibility in
      let ty = value.value_ty in
      Format.fprintf fmt
        "%a%a : %a@\n@\n"
        pp_value_visibility visibility
        (Format.pp_list "." pp_path_elt) path
        pp_light_ident#ty_new_scope ty

    let pp_type fmt type_def =
      pp_light_ident#reset_typevars ;
      let typedef = type_def.type_def in
      Format.fprintf fmt
        "%a@\n@\n"
        pp_light_ident#typedef typedef

    let pp fmt entry =
      match entry.code_elt with
      | Value value -> pp_value fmt entry.path value
      | Type type_def -> pp_type fmt type_def
  end
end


(**
   Json serialization, from Api structure (module above) to Json structures
   ready to be loaded by opa.
*)
module Serialize :
sig
  val entry :
    gamma:QmlTypes.gamma ->
    annotmap:QmlAst.annotmap ->
    (Api.entry -> JsonTypes.json)
end =
struct
  module J = JsonTypes

  let string s = J.String s
  let pkg pkg = J.String pkg
  let file file = J.String file
  let pos pos = J.Int pos
  let path path = J.Array (List.map string path)

  (**
     Given a Qml Expression representing an OpaTy.ty (runtime type ast),
     returns its serialized version (json)
  *)
  let rec opaty_to_json expr =
    match expr with
    | Q.Coerce (_, expr, _ty) -> opaty_to_json expr
    | Q.Const (_, (Q.String s)) -> string s
    | Q.Directive (_, `tagged_string (tyname, _kind), [], []) -> string tyname
    | Q.Record (_, fields) ->
        let fold acc (field, expr) =
          (field, opaty_to_json expr) :: acc
        in
        let fields = List.sort (fun (s1, _) (s2, _) -> String.compare s2 s1) fields in
        J.Record (List.fold_left fold [] (List.rev fields))
    | _ -> OManager.i_error "Unexpected expr in opaty_to_json: %a@." QmlPrint.pp#expr expr


  class serializer ~gamma ~annotmap =
    let ty_to_opaty_for_opadoc =
      Pass_ExplicitInstantiation.ty_to_opaty_for_opadoc
        ~val_:OpaMapToIdent.val_
        ~gamma
        ~annotmap
    in
  object(self)

    val gamma = gamma
    val annotmap = annotmap

    (** Variables scope for type variables *)
    val typevar_scope = QmlTypeVars.TypeVarPrint.new_scope ()
    val rowvar_scope = QmlTypeVars.RowVarPrint.new_scope ()
    val colvar_scope = QmlTypeVars.ColVarPrint.new_scope ()

    method reset_typevars =
      QmlTypeVars.TypeVarPrint.reset typevar_scope ;
      QmlTypeVars.RowVarPrint.reset rowvar_scope ;
      QmlTypeVars.ColVarPrint.reset colvar_scope ;
      ()

    method typevar var = QmlTypeVars.TypeVarPrint.get typevar_scope var
    method rowvar var = QmlTypeVars.RowVarPrint.get rowvar_scope var
    method colvar var = QmlTypeVars.ColVarPrint.get colvar_scope var

    (**
       Given a Qml Types, and typer environment, transform it into a runtime type expression.

       This use a normalization specialized for the documentation, preserving original names when
       there are some provided, and generated pretty names ('a, 'b, 'c, etc.) when there
       is no name provided.
       This is a normalization at compile time.
    *)
    method ty ty =
      let ty = ty_to_opaty_for_opadoc typevar_scope rowvar_scope colvar_scope ty in
      opaty_to_json ty

    method visibility (vis : QmlAst.doctype_access_directive) =
      (*
        <!> keep synchronized with opa names, cf OpaDocTy
      *)
      let field =
        match vis with
        | `private_ -> "private"
        | `public -> "public"
        | `package -> "package_"
      in
      J.Record [
        field, J.Void ;
      ]

    method value v =
      (*
        <!> OPA magic serialize, reverse of alphabetic order between fields
      *)
      J.Record [
        "visibility", self#visibility v.Api.value_visibility ;
        "ty", self#ty v.Api.value_ty ;
      ]

    (*
      FIXME:
      currently, the representation of type def is a tuple Api.ty * Api.ty
      in opadoc, which is not extensible enough to cover directives, and visibility.
      This will change for a record containing visibility informations.
    *)
    method type_def type_def =
      let type_def = type_def.Api.type_def in
      let name =
        string
          (Q.TypeIdent.to_string type_def.QmlAst.ty_def_name)
      in
      let params =
        let param tyvar = string (self#typevar tyvar) in
        let params = List.map param type_def.QmlAst.ty_def_params in
        JsonTypes.Array params
      in
      let visibility =
        match type_def.QmlAst.ty_def_visibility with
        | Q.TDV_public ->
            J.Record [
              "TDV_public", J.Void ;
            ]
        | Q.TDV_abstract package_name ->
            J.Record [
              "TDV_abstract", string package_name ;
            ]
        | Q.TDV_private package_name ->
            J.Record [
              "TDV_private", string package_name ;
            ]
      in
      let body = self#ty type_def.QmlAst.ty_def_body in
      (*
        <!> OPA magic serialize, reverse of alphabetic order between fields
      *)
      let tuple = JsonTypes.Record [
        "ty_def_visibility", visibility ;
        "ty_def_params", params ;
        "ty_def_name", name ;
        "ty_def_body", body ;
      ] in
      tuple

    method code_elt api =
      match api with
      | Api.Value value ->
          J.Record [
            "value", self#value value ;
          ]

      | Api.Type type_def ->
          J.Record [
            "type_def", self#type_def type_def ;
          ]

    method entry e =
      (*
        <!> OPA magic serialize, reverse of alphabetic order between fields
      *)
      J.Record [
        "pos", pos e.Api.pos ;
        "pkg", pkg e.Api.pkg ;
        "path", path e.Api.path ;
        "fname", file e.Api.fname ;
        "code_elt", self#code_elt e.Api.code_elt ;
      ]
  end

  let entry ~gamma ~annotmap =
    let serializer = new serializer ~gamma ~annotmap in
    let map entry =
      (*
        The scope of type variables is reset between each new entry
      *)
      serializer#reset_typevars ;
      serializer#entry entry
    in
    map

end

(**
   Print an error if a filename cannot be created
*)
let on_error filename error =
  match error with
  | None -> ()
  | Some msg ->
      OManager.error (
        "cannot output file %S@\n"^^
          "@[<2>@{<bright>Hint@}:@\n"^^
          "%s@]"
      ) filename msg

(**
   FileMap:
   A polymorphic map for storing a list of 'a associated to a filename
*)
module FileMap = ListMap.Make(Order.String)

(**
   Process the qml code, meaning remove the previously inserted doctypes directives,
   and generate api files (opadoc), as well as humain readable api-txt files (for debug)
*)
let process_qml ~(options : E.opa_options)
    (env : 'tmp_env Passes.env_Gen) : 'tmp_env Passes.env_Gen =
  let annotmap = env.P.typerEnv.QmlTypes.annotmap in
  let gamma = env.P.typerEnv.QmlTypes.gamma in

  let make_value = Api.Entry.value ~gamma ~annotmap in
  let make_type_def = Api.Entry.type_def ~gamma ~annotmap in

  (*
    Remove `doctype directives, filter annotmap, and collect doc values
  *)
  let ((annotmap, coll), qmlAst) = remove_code_doctype annotmap env.P.qmlAst in

  (* OUTPUTS *)
  if options.E.generate_interface || options.E.generate_interface_and_compile then (

    (*
      Construct the map filenames --> data
    *)
    let byfile = FileMap.empty in

    (*
      1. add Value data, collected by [remove_code_doctype]
    *)
    let byfile =
      List.fold_left
        (fun byfile ((_, label, _) as value) ->
           let filename = FilePos.get_file (Annot.pos label) in
           let entry = make_value value in
           FileMap.append filename entry byfile)
        byfile coll
    in

    (*
      2. add Type data, from doc_types stored in the environment
    *)
    let byfile =
      List.fold_left
        (fun byfile ((_, a) as typedef) ->
           let filename = FilePos.get_file a.QmlLoc.pos in
           let entry = make_type_def typedef in
           FileMap.append filename entry byfile)
        byfile (List.rev env.P.doc_types)
    in

    (* JSON OUTPUT *)
    let entry_to_json = Serialize.entry ~gamma ~annotmap in
    FileMap.iter (fun file entries ->
      let filename = file ^ ".api" in
      let jsonl = JsonTypes.Array (List.tail_map entry_to_json entries) in
      OManager.verbose "generating %S" filename ;
      let error = File.oc_output filename JsonPrint.Output.json jsonl in
      on_error filename error ;
    ) byfile ;

    (* HUMAN OUTPUT *)
    FileMap.iter (fun file entries ->
      let filename = file ^ ".api-txt" in
      OManager.verbose "generating %S" filename ;
      let error = File.pp_output filename (Format.pp_list "" Api.Entry.pp) entries in
      on_error filename error ;
    ) byfile ;

    (* EXITING PASS *)
    if not options.E.generate_interface_and_compile then
      exit 0

  );

  { env with Passes.
    qmlAst = qmlAst;
    typerEnv = { env.P.typerEnv with
                 QmlTypes.annotmap = annotmap;
               };
  }
