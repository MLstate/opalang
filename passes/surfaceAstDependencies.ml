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
(**
   FIXME: talk about the dependencies
   MAKE SEPARATE PARTS MORE DISTINCTS, PROVIDE A  REAL SIGNATURE

   This module contains functions that regroup recursive values in groups, eg:
   - in the source code:
        [f1() = f3()
         f2() = f1()
         f3() = f1()]
   - after parsing:
        [NewVal [(f1,f3())];
         NewVal [(f2,f1())];
         NewVal [(f3,f1())]]
   - after dependency analysis
        [NewVal [(f1,f3()); (f3,f1())];
         NewVal [(f2,f1())]]

   It also contains functions that rewrite modules:
   - in the source code:
       [{{ x() = y(); y() = x() }}]
   - after renaming:
       [{ x = @local(x', {} -> y'({}));
          y = @local(y', {} -> x'({})) }]
   - after rewrite:
       [ rec x' = {} -> y'({})
         and y' = {} -> x'({})
         { x = x'; y = y'} ]
   If your module contains submodules, then their fields will go out too:
   - source code:
       [{{ x = {{ x2 = 1 }} }}]
   - after rewrite:
       [ x2' = 1;
         x' = { x2 = x2' }
         { x = x' } ]
   This is useless here, but useful when you have recursive modules that
   call each other's submodules
*)

(* HACK : please, clean-up in opa lang *)
module Parser_utils = OpaParserUtils

(* depends *)
module List = BaseList
module String = BaseString


(* TODO remove *)
open SurfaceAst

(* shorthands *)
module SAH = SurfaceAstHelper
module C = SurfaceAstCons.ExprIdentCons
module D = SurfaceAstDecons
let copy_label = Parser_utils.copy_label

let (|>) = InfixOperator.(|>)
let ( @> ) f g x = x |> f |> g


(* debug *)
let pp_code_elt f (code_elt_node,annot) =
  Format.fprintf f "%d -> %a@\n"
    annot.QmlLoc.notes
    OpaPrint.ident#code_elt_node code_elt_node
let pp_code f l =
  List.iter (pp_code_elt f) l;
  Format.pp_print_flush f ()
let pp_set f s =
  IntSet.iter (fun i -> Format.fprintf f "%d " i) s;
  Format.fprintf f "@\n%!"


(*
(* For incremental, to detect quickly if a value has changed *)
type hash = unit
*)

(* for now just use fields by fields
   latter group (multi-pattern) of group (record pattern) of fields *)

(* [ match l with
     | {nil} ->
     | {hd tl} -> ... ]
   gives [[[nil],[hd,tl]], i guess *)

type fields_group = string list list
type fields = fields_group
module FieldsOrd = struct type t = fields let compare = compare end
module FieldsMap = BaseMap.Make(FieldsOrd)

(**
   In [/a/b/c], db_root would be [a]
*)
type db_root = string

type 'a gen_dep_context =
    { vals  : (Ident.t * Ident.t ty option) list
    ; types : Ident.t list
    ; fields_groups :  fields_group list
    ; db_roots : db_root list
    ; database : bool
    ; other: 'a }
let empty_gen_dep_context other =
  { vals = []
  ; types = []
  ; fields_groups = []
  ; db_roots = []
  ; database = false
  ; other = other }

(* FIXME: real merge? *)
let merge_vals l1 l2 = l1 @ l2
let merge_types l1 l2 = l1 @ l2
let merge_fields_groups l1 l2 = l1 @ l2
let merge_db_roots l1 l2 = l1 @ l2
let merge_database b1 b2 = b1 || b2

let merge_context merge_other context1 context2 =
  { vals = merge_vals context1.vals context2.vals
  ; types = merge_types context1.types context2.types
  ; fields_groups = merge_fields_groups context1.fields_groups context2.fields_groups
  ; db_roots = merge_db_roots context1.db_roots context2.db_roots
  ; database = merge_database context1.database context2.database
  ; other = merge_other context1.other context2.other }
let merge_contexts merge_other empty_context contexts =
  List.fold_left (merge_context merge_other) empty_context contexts

(* FIXME: collision, or duplicates? *)
(* should be consistent with merge *)
let add_root s context = {context with db_roots = s :: context.db_roots}
let add_group g context = {context with fields_groups = g :: context.fields_groups}
let add_type t context = {context with types = t :: context.types}
let add_types ts context = {context with types = ts @ context.types}
let add_val v context = {context with vals = (v,None) :: context.vals}
let add_vals vs context = {context with vals = List.map (fun v -> v,None) vs @ context.vals}
let add_database context = {context with database = true}

(* will contain what is used by a declaration,
   a declaration may contain many values *)
type directive_dep = [ `magic_tostring | `magic_to_xhtml ]
type dep = directive_dep list
type dep_context = dep gen_dep_context
let empty_dep_context = empty_gen_dep_context []
let merge_dep_other = (@)
let merge_dep_context = merge_context merge_dep_other
let merge_dep_contexts = merge_contexts merge_dep_other empty_dep_context

let db_root_of_path l =
  match l with
    | (QmlAst.Db.Decl_fld s) :: _ -> s
    | _ -> assert false

let directive_dependencies filter acc v =
  let dir_to_str_fun v =
    match v with
      | `magic_to_string -> Opacapi.magicToString
      | `magic_to_xml -> Opacapi.magicToXml
      | `fun_action -> Opacapi.FunActionServer.serialize_argument in
  match v with
    | ( `magic_to_string
      | `magic_to_xml
      | `fun_action) as v ->
        let str =  dir_to_str_fun v in
        let id = try OpaMapToIdent.val_noerr str with Not_found -> failwith (Printf.sprintf "Please define %s." str) in
        begin match filter with
          | None -> add_val id acc
          | Some f when f id -> add_val id acc
          | _ -> acc
        end
    | _ -> acc

let get_pat_dep_context acc p =
  OpaWalk.Pattern.fold
    (fun acc -> function
     | (PatCoerce (_,ty),_) ->
         let typenames = OpaWalk.Type.get_typenames_with_acc acc.types ty in
         {acc with types = typenames}
     | _ -> acc) acc p

(**
   [get_expr_dep_context] gives the toplevel direct dependencies of an
   expression
   The environment contains the identifiers in scope (both types and values)
   but not the toplevel ones
*)
let get_expr_dep_context ?filter e =
  SurfaceAstTraversal.Expr.traverse_fold_with_set
    (fun env acc (e, _label) ->
       let acc =
         match e with
           (* nothing special for bindings except that the environment
            * grows but this is already taken care of the fold_with_env
            * function *)
         | LetIn _
         | Apply _
         | Const _
         | Bypass _ -> acc

         | Lambda (spl,_) ->
             (match filter with
              | None ->
                  List.fold_left (fun acc (_,p) -> get_pat_dep_context acc p) acc spl
              | Some _ -> acc)
         | Match (_e,pel) ->
             (match filter with
              | None ->
                  List.fold_left (fun acc (p,_) -> get_pat_dep_context acc p) acc pel
              | Some _ -> acc)
         | DBPath (dbelt, _)->
             let acc = Option.if_none filter (add_database acc) acc in
             (* taking the first elt of the path *)
             ( match (fst (List.hd (fst dbelt))) with
               | FldKey s -> Option.if_none filter (add_root s acc) acc
               | NewKey
               | ExprKey _ ->
                   (* not possible, see the parser *)
                   assert false
             )
         | Record l ->
             Option.if_none filter (add_group [SAH.Record.field_names l] acc) acc
         | ExtendRecord (l, _e) ->
             Option.if_none filter (add_group [SAH.Record.field_names l] acc) acc
         | Dot (_,s) ->
             Option.if_none filter (add_group [[s]] acc) acc
         | Ident i ->
             if not (IdentSet.mem i env) then
               match filter with
               | None -> add_val i acc
               | Some f when f i -> add_val i acc
               | _ -> acc
             else
               acc
         | Directive (variant, _, tyl) -> (
             let acc = directive_dependencies filter acc variant in
             match filter with
             | None ->
                 let typenames = List.fold_left OpaWalk.Type.get_typenames_with_acc acc.types tyl in
                 {acc with types = typenames}
             | Some _ -> acc
           ) in
       env, acc
    ) empty_dep_context e


(**
   [get_local_expr_dep_context] is used when you don't want all the
   dependencies of an expression but only dependencies on the identifiers
   present in the set [names]
   it allows you to reorder/split a set of mutually recursive bindings
 *)
let get_local_expr_dep_context names expr =
  let filter i = List.exists (fun j -> Ident.equal i j) names in
    get_expr_dep_context ~filter expr

(**
   [get_code_elt_dep_context] gives you the toplevel dependencies of a code_elt
*)
let get_code_elt_dep_context (code_elt_node, _label) =
  match code_elt_node with
    | Database _ ->
        empty_dep_context
    | NewDbDef (QmlAst.Db.Db_TypeDecl ([_], ty)) ->
        (* db /map does not depend on /map, it defines it
         * on the other hand, db /map[_] = 2 does depend
         *)
        { empty_dep_context with
            types = OpaWalk.Type.get_typenames ty }
    | NewDbDef (QmlAst.Db.Db_TypeDecl (sl, ty)) ->
        { empty_dep_context with
            db_roots = [db_root_of_path sl];
            types = OpaWalk.Type.get_typenames ty }
    | NewDbDef (QmlAst.Db.Db_Default (p, _)
                   | QmlAst.Db.Db_Constraint (p, _)
                   | QmlAst.Db.Db_Alias (_, p)
                   | QmlAst.Db.Db_Virtual (p, _) as db_def) ->
        fst(QmlAst.Db.foldmap_expr
              (fun dep_context e -> merge_dep_context (get_expr_dep_context e) dep_context, e)
              {empty_dep_context with db_roots = [db_root_of_path p]}
              db_def)
    | NewType tds ->
        let contexts =
          List.map
            (fun (ty_def, _) ->
               { empty_dep_context with
                   types =
                     OpaWalk.Type.get_typenames ty_def.SurfaceAst.ty_def_body })
            tds in
        merge_dep_contexts contexts
    | NewVal (pel,_) ->
        let contexts = List.map (snd @> get_expr_dep_context) pel in
        merge_dep_contexts contexts
    | Package _ -> assert false

(* contains what a declaration has to offer *)
type api = unit
type api_context = api gen_dep_context
let empty_api_context = empty_gen_dep_context ()
let merge_api_other _ _ = ()
let merge_api_context = merge_context merge_api_other
let merge_api_contexts = merge_contexts merge_api_other empty_api_context

(**
   [get_code_elt_api_context] gives you what is provided by a code_elt
*)
let get_code_elt_api_context ((code_elt_node, _label) as code_elt) =
  match code_elt_node with
    | Database (ident,_,_) ->
        {empty_api_context with vals = [(ident,None)]}
    | NewDbDef (QmlAst.Db.Db_TypeDecl (p, _)) ->
        (* all the [db /map/...] define the root '/map'
         * or else in [ db /map/a : string
         *              db /map/b : string ], '/map' is never defined
         *)
        {empty_api_context with db_roots = [db_root_of_path p]; database = true}
    | NewDbDef _ ->
        {empty_api_context with database = true}
    | NewType tds ->
        let typenames =
          List.map
            (fun ({ SurfaceAst.ty_def_name = Typeident i ; _ }, _) -> i) tds in
        {empty_api_context with types = typenames}
    | NewVal _ ->
        let idents =
          OpaWalk.CodeEltTopPattern.fold_nonrec
            (fun acc p -> OpaWalk.Pattern.get_vars ~acc p)
            []
            code_elt in
        {empty_api_context with vals = List.map (fun p -> (p,None)) idents}
    | Package _ -> assert false

(**
   These maps map identifiers to an int that identifies the declaration
   that defines them
   the int is chosen to be the number in the annot of the code_elt

   Used to compute deps from api_context
 *)
type context_linker_cache =
    int IdentMap.t (* identifiers *)
  * int IdentMap.t (* type identifiers *)
  * int list StringMap.t (* for db_roots
                          * a list is needed because a db root may be defined by several declarations *)
  * int FieldsMap.t
  * int list (* the list of identifiers that provide the database *)

let empty_context_linker_cache : context_linker_cache =
  (IdentMap.empty, IdentMap.empty, StringMap.empty, FieldsMap.empty, [])

let get_context_linker_cache_input lcode =
  List.map
    (function ((_,annot) as code_elt) ->
       annot.QmlLoc.notes, get_code_elt_api_context code_elt
    ) lcode

let get_get_deps_input lcode =
  List.map
    (function ((_,annot) as code_elt) ->
       annot.QmlLoc.notes, get_code_elt_dep_context code_elt
    ) lcode

let context_linker_cache iapis : context_linker_cache =
  List.fold_left
    (fun (idents,types,db_roots,fields,database) (i,api) ->
       List.fold_left (fun idents (ident,_) -> IdentMap.safe_add ident i idents) idents api.vals,
       List.fold_left (fun types ident -> IdentMap.safe_add ident i types) types api.types,
       List.fold_left (fun db_roots db_root ->
                         let prev = Option.default [] (StringMap.find_opt db_root db_roots) in
                         StringMap.add db_root (i :: prev) db_roots
                      ) db_roots api.db_roots,
       List.fold_left (fun fields g -> FieldsMap.safe_add g i fields) fields api.fields_groups,
       (if api.database then i :: database else database)
    ) empty_context_linker_cache iapis

let identmap_find i m =
  match IdentMap.find_opt i m with
    | None ->
        OManager.i_error "SurfaceAstDependencies.identmap_find: %s\n%!" (Ident.to_string i)
    | Some v -> v

let get_deps ((vals,types,db_roots,_fields,database):context_linker_cache) ideps =
  List.map
    (fun (i,dep_context) ->
       let set = IntSet.empty in
       let set =
         List.fold_left
           (fun set (ident, _tyo) ->
              match IdentMap.find_opt ident vals with
              | None -> set
              | Some v -> IntSet.add v set
           ) set dep_context.vals in
       let set =
         List.fold_left
           (fun set type_ ->
              match IdentMap.find_opt type_ types with
              | None -> set
              | Some v -> IntSet.add v set
           ) set dep_context.types in
       let set =
         List.fold_left
           (fun set db_root ->
              match StringMap.find_opt db_root db_roots with
                | None ->
                  (* an expression is using an undefined db root
                   * this is a mistake, but someone else will take care of it *)
                  set
                | Some v -> IntSet.add_list v set
           ) set dep_context.db_roots in
       let set =
         if dep_context.database then
           IntSet.add_list database set
         else
           set in

(*
 * Here nobody defines so these dependencies are not likely to be resolved
 * fields
 * when NewType
 *)
       (* FIXME when the comment below is taken care of somewhere else
         let set =
         List.fold_left
           (fun set field ->
              IntSet.add (FieldsMap.find field fields) set
           ) set dep_context.fields in*)
       (i, set)
    ) ideps


(**
   create the map that map the number of a declaration to the declaration
   itself
*)
let map_back lcode =
  List.fold_left
    (fun map ((_,annot) as code_elt) ->
       IntMap.add annot.QmlLoc.notes code_elt map
    ) IntMap.empty lcode

(**
   When given a list of code_elt, gives back the list where all newval
   are regrouped into one newval and put it last in the list
*)
(* trying to not screw up completely the positions with the 'pos' *)
let rec special_flatten isrec (pos,vals,types,others) l =
  match l with
    | [] ->
        let vals =
          match vals with
          | [] -> []
          | [_] when not isrec -> [(NewVal (vals,false), Parser_utils.annot pos)]
          | _ -> [(NewVal (vals,true), Parser_utils.annot pos)] in
        let types =
          match types with
          | [] -> []
          | _ -> [(NewType types, Parser_utils.annot pos)] in
        others @ types @ vals
    | ((c,{QmlLoc.pos=pos2; _}) as h) :: t ->
        match c with
          | Package _ -> assert false
          | Database _
          | NewDbDef _ -> special_flatten isrec (pos, vals, types, h :: others) t
          | NewType tds ->
              let pos = FilePos.merge_pos pos pos2 in
              special_flatten isrec (pos, vals, tds @ types, others) t
          | NewVal (l,_) ->
              let pos = FilePos.merge_pos pos pos2 in
              special_flatten isrec (pos, l @ vals, types, others) t
let special_flatten (isrec,l) =
  special_flatten isrec (FilePos.nopos "SurfaceAstDependencies.special_flatten", [], [], []) l


let debug _s _f _x =
  #<If:SA_DEPENDENCIES>
    Format.printf "----- %s ------@\n%a%!" _s _f _x
  #<End>

let flatten_newval_newtype code =
  let aux = function
    | (NewType typedefs, label) ->
        List.map (fun td -> (NewType [td], Parser_utils.copy_label label)) typedefs
    | (NewVal (pel,rec_),label) ->
        (* actually the rec_ flag doesn't matter *)
        List.map (fun bnd -> (NewVal ([bnd], rec_), Parser_utils.copy_label label)) pel
    | c -> [c] in
  List.concat_map aux code

(**
   This function reorders the toplevel

   Take the function that computes the transivite closure of the dependency
   relation as an argument because doing otherwise would (for now) create
   circular dependencies between libraries

*)
let reorder_toplevel ?roots create_groups lcode =
  debug "reorder" pp_code lcode;
  let lcode = flatten_newval_newtype lcode in
  let iapis = get_context_linker_cache_input lcode in
  let ideps = get_get_deps_input lcode in
  let (vals,_,_,_,_) as context_linker_cache = context_linker_cache iapis in
  let deps = get_deps context_linker_cache ideps in
  debug "deps" (fun f l -> List.iter (fun (i,s) -> Format.fprintf f "%d -> %a" i pp_set s) l) deps;
  let roots =
    (* if we have roots that are not defined, we simply ignore them *)
    Option.map (List.filter_map (fun i -> IdentMap.find_opt i vals)) roots in
  let ((groups : (int * bool * IntSet.t) list),_) = create_groups ?roots deps in
  debug "groups" (fun f l -> List.iter (fun (i,isrec,s) -> Format.fprintf f "%d(%B) -> %a" i isrec pp_set s) l) groups;
  let map_back = map_back lcode in
  let declss =
    List.map
      (fun (_repr,isrec,group) ->
           isrec,IntSet.fold (fun i acc -> IntMap.find i map_back :: acc) group []
      ) groups in
  let lcode = List.concat_map special_flatten declss in
  debug "end" pp_code lcode;
  lcode

let safe_union s1 s2 =
  let s = IdentSet.union s1 s2 in
  assert (IdentSet.cardinal s1 + IdentSet.cardinal s2 = IdentSet.cardinal s);
  s

let get_all_deps get_var_set bindings =
  let names, exprs = List.split bindings in
  let possible_deps =
      names
   |> List.map get_var_set
   |> List.flatten in (* UNICITY: safe union *)
  assert (List.length possible_deps = List.length (List.uniq_unsorted possible_deps));
  List.map (get_local_expr_dep_context possible_deps) exprs

let regroup map_expr intset =
  assert (IntSet.cardinal intset <> 0);
    intset
 |> IntSet.elements
 |> List.map (fun i -> IntMap.find i map_expr)

(**
    reorder a list of bindings the same way reorder_toplevel does
    the bindings have the type ('a * expr) list, where 'a will be
    either ident or pattern
    use the functions below, that are already specialized for idents
    and patterns
*)
let reorder_expr :
    ('a -> Ident.t list)
       -> _ -> ('a * (_,_) expr) list -> (('a * (_,_) expr) list * bool) list =
    fun get_var_set create_group_list bindings ->
  (* FIXME: too complicated for simple cases ? *)
  let deps = get_all_deps get_var_set bindings in
  let apis =
    List.map
      (fun (p,_) ->
         let vars = get_var_set p in
         add_vals vars empty_api_context
      ) bindings in
  let ideps = List.mapi (fun i x -> (i,x)) deps in
  let iapis = List.mapi (fun i x -> (i,x)) apis in
  let context_linker_cache = context_linker_cache iapis in
  let deps = get_deps context_linker_cache ideps in
  let groups, _final = create_group_list deps in
  let bindings_with_int = List.mapi (fun i (name,e) -> (i,(name,e))) bindings in
  let map_expr = IntMap.from_list bindings_with_int in
    groups
 |> List.map (fun (_id,isrec,group) -> regroup map_expr group,isrec)

(**
    takes the Reordering.create_group_list, a list of (pattern * expr)
    and gives a list of list of (pattern * expr), representing groups of
    mutually dependent bindings
*)
let reorder_for_pat_bindings x =
  reorder_expr OpaWalk.Pattern.get_vars x
(**
   same as above, with (ident * expr) list
*)
let reorder_for_ident_bindings x y =
  reorder_expr (fun x -> [x]) x y


(**
   Module are encoded as Record l where l is a list of @local directives
   by the renaming pass
   The `local constructor contains the name that is used by other fields
   to refer to the present one.

   For example, the ast of [{{ x = 2 y = x }}] is:
   Record ([("x", (Directive (`local x', [(Const (Int 2), _)], _),_));
            ("y", (Directive (`local y', [(Ident x', _)], _), _))])

   This functions gives you the list of (field * identifier * position * expression)
   With the previous example, it would be:
   [("x", x', _, (Const (Int 2), _));
    ("y", y', _, (Ident x', _))]
*)

let extract_field_ident_expr_from_module e =
  let reannotate = OpaToQml.propagate_slicer_annotation e in
  let e, acc, coercer =
    D.FoldContext.letin
      ~through:[D.Context.Basic.slicer_directive;
                D.Context.Basic.opacapi;
                D.Context.Basic.coerce;
                D.Context.Basic.doctype] e in
  let acc = List.map (fun (i,e) -> (i, reannotate e)) acc in
  match e with
    | (Directive (`module_, [(Record l,label_record)], b),c) ->
        let l =
          List.map
            (fun (s,e') ->
               match e' with
                 | (Directive (`local ident, [e''], _),label) ->
                     s, ident, label, reannotate e''
                 | _ ->
                     Format.printf "@[<v2>Error:@ @[%a@],@ @[%a@],@ %s@]@."
                       OpaPrint.ident#expr e'
                       OpaPrint.ident#expr e
                       (SAH.Annot.to_string' e');
                     assert false
            ) l
        in
          (* putting the @module directive under the coercions *)
          l, label_record, acc, (fun e -> coercer (Directive (`module_, [e], b),c))
    | _ ->
        Printf.printf
          "Not a record through letins at %s\n%!" (SAH.Annot.to_string' e);
        assert false

(**
   gives a mapping from field to the ident bound to its content
   (see comment above for more details)
*)
let is_private e = D.Look.private_ ~through:[D.Remove.Basic.doctype; D.Remove.Basic.opacapi; D.Remove.Basic.slicer_directive] e
type access =
  | Possible (* normal module field *)
  | Forbidden (* field tagged as private *)
let module_assoc e =
  let l, _, _, _ = extract_field_ident_expr_from_module e in
  List.map
    (fun (s,ident,_,e) ->
       (s, ((if is_private e then Forbidden else Possible), ident))
    ) l

let is_module e =
  D.Look.module_local
    ~through:[D.Remove.Basic.slicer_directive
             ;D.Remove.Basic.coerce
             ;D.Remove.Basic.letin
             ;D.Remove.Basic.doctype
             ;D.Remove.Basic.opacapi
             ;D.Remove.Basic.async
             ] e

(**
   makes a mapping from identiers to an association list of (field * ident)
   from the identifiers in the list given whose value is a module
   (only does so for direct modules, not modules that are fields of other modules)
*)
let module_names ident_expr_list =
  let assoc =
    List.filter_map
      (fun (i,e) ->
         if is_module e then
           let assoc = module_assoc e in
           Some (i,assoc)
         else
           None
      ) ident_expr_list
  in
    IdentMap.from_list assoc

(**
   simplifies a path [ident.field1.field2] with [ident1.field2]
   if the map in argument contains a mapping from ident to l, and l (an
   association list) contains a mapping from [field1] to [ident1]
   This simplification is done recursively if possible
*)
let rec resolve_path :
    _ ->
      ((string * (access * Ident.t)) list IdentMap.t)
        -> Ident.t -> (string * QmlLoc.annot) list
          -> Ident.t * (string * QmlLoc.annot) list =
  fun label map first_ident full_path ->
    let rec aux i path =
      match IdentMap.find_opt i map with
      | None -> i, path
      | Some assoc ->
          match path with
          | [] -> i, path
          | (field,_) :: path_tail ->
              match List.assoc_opt field assoc with
              | None ->
                  OManager.serror
                    "@[<2>%s@\nThe field %s doesn't exist in the path %s.%s.@]@\n"
                    (SAH.Annot.to_string label)
                    field
                    (Ident.original_name first_ident)
                    (String.concat_map "." fst full_path);
                  i, path
              | Some (access, ident) ->
                  (match access with
                   | Possible -> ()
                   | Forbidden ->
                       OManager.serror
                         "@[<2>%s@\nThe path %s.%s is invalid because the field %s is private.@]@\n"
                         (SAH.Annot.to_string label)
                         (Ident.original_name first_ident)
                         (String.concat_map "." fst full_path)
                         field
                  );
                  aux ident path_tail in
    aux first_ident full_path

let rewrite_path_basic map e =
  let e', path = D.FoldThrough.dot e in
  match e' with
  | (Ident i, label) ->
      let i, path = resolve_path (snd e) map i path in
      SurfaceAstCons.Fold.dot (Ident i, label) path
  | _ -> e

(**
   Simplifies a path as described before everywhere in an expression
*)
let rewrite_path map e =
  OpaWalk.Expr.map_up (fun e -> rewrite_path_basic map e) e

let rec remove_access_directive e =
  match e with
  | (Directive ((`doctype _ | #distribution_directive as variant), [e], b), c) ->
      (Directive (variant, [remove_access_directive e], b), c)
  | (Directive (#access_directive, [e], _),_) ->
      e
  | _ -> e
  (*if D.Look.access_directive ~through:[D.Remove.Basic.doctype] e then
    let e, rebuild =
      D.Context.filter
        ~through:[D.Context.Basic.doctype]
        ~throw:[D.Remove.Basic.access_directive] in
    rebuild e
  else
    e*)

(**
   This function extract the definitions of module fields from the module
   {{ x = 2 }} -> let x = 2 in { x = x }
   The definitions of fields of submodules are not touched

   You can compute the fixpoint on this function: it will terminate because
   a module can only flattened once:
   module are recognised because they have @local directives, but this
   function removes them. So the number of record containing @local directives
   strictly decreases (and this function only works on records with @local
   directives)

*)
let flatten_fields (i,e) =
  if is_module e then
    let l, label, letin_bindings, coercer =
      extract_field_ident_expr_from_module e in
    let record =
      List.filter_map
        (fun (field,ident,label,e) ->
           if is_private e then None
           else Some (field, (Ident ident, label)))
        l in
    let record_expr = coercer (Record record, label) in
    let bindings =
      List.map
        (fun (_,ident,_,e) ->
           let e = remove_access_directive e in
           (ident,e)) l in
    (i, record_expr) :: letin_bindings @ bindings
  else
    [(i,e)]

(* this function flatten all the modules directly present in the expressions on the given bindings
 * it calls itself until a fixpoint is reached
 * (the fixpoint being detected when the number of bindings doesn't increase)
 *)
let rec flatten_module ident_expr_list =
  let map = module_names ident_expr_list in
    ident_expr_list
 |> List.map (fun (i,e) -> (i, rewrite_path map e))
 |> List.concat_map flatten_fields
 |> (fun l -> if List.length l = List.length ident_expr_list then l else flatten_module l)

let rec flatten_module_in_expr_basic create_group_list = function
  | (Directive (#access_directive as v, [e], _),label) ->
      OManager.serror "@[<2>%s@\nInvalid directive @@%a.@]@\n" (SAH.Annot.to_string label) OpaPrint.ident#variant v;
      flatten_module_in_expr_basic create_group_list e
  | (LetIn (_,iel,e), label) ->
      let iel = flatten_module iel in
      let biell = reorder_for_ident_bindings create_group_list iel in
      List.fold_right
        (fun (iel,rec_) e ->
           (LetIn (rec_,iel,e), copy_label label)) biell e
  | (_, label) as e ->
      if is_module e then
        let i = Ident.next "flatten_module" in
        (* FIXME: factorize me with the code above *)
        let iel = flatten_module [(i,e)] in
        let iell = reorder_for_ident_bindings create_group_list iel in
          match List.extract_last iell with
            (* FIXME: HACK: very fragile but needed by InlineModules *)
            | iell, ([(i',e)],false) when Ident.equal i i' ->
                (* Continuation to pass to
                   [SurfaceAstCons.with_builtin_position] in order to build the
                   directive expression. We can't simply call
                   [SurfaceAstCons.ExprIdentCons.E.letgen] because it makes
                   usage of a stack of location that is currently empty because
                   we are not in the parser. To avoid raising an "empty stack"
                   we must wrap our expression construction in a call to
                   [SurfaceAstCons.with_builtin_position]. *)
                let mk_directived_definition () =
                  List.fold_right
                    (fun (iel, rec_) e ->
                       (* Embedd the let-definition into the directive telling
                          that it is in fact a module field's body that has
                          been expatriated from its module. *)
                       let dir =
                         Directive
                           (`module_field_lifting,
                            [ SurfaceAstCons.ExprIdentCons.E.letgen
                                ~rec_: rec_ iel e ],
                            []) in
                       (dir, copy_label label))
                    iell e in
                SurfaceAstCons.with_builtin_position mk_directived_definition
            | _ ->
                (* See comment above to learn about the manual construction of
                   the directive expression. *)
                let mk_directived_definition () =
                  List.fold_right
                    (fun (iel, rec_) e ->
                       (* Like above, embedd the let-definition in directive. *)
                       let dir =
                         Directive
                           (`module_field_lifting,
                            [ SurfaceAstCons.ExprIdentCons.E.letgen
                                ~rec_: rec_ iel e ],
                            []) in
                       (dir, copy_label label))
                    iell (Ident i, copy_label label) in
                SurfaceAstCons.with_builtin_position mk_directived_definition
      else
        e

(**
   Rewrite local modules
*)
let flatten_module_in_expr create_group_list lcode =
  (* mapping down because we want to rewrite the modules in letin before
   * the modules that appear in other places *)
  SurfaceAstTraversal.ExprTraverse.Heterogeneous.lift_map_down
    (function
       | #basic_directive as v -> v
       | #dependency_directive -> Format.printf "%a@." OpaPrint.readable_ident#code lcode; assert false)
    (flatten_module_in_expr_basic create_group_list)
    lcode

(* used to create placeholders, they are not supposed to be used *)
let fresh_ident () = Ident.next "rewrite_module"

(**
   Rewrite toplevel modules
*)
let flatten_toplevel_module create_group_list lcode =
  List.concat_map
    (fun ((code_elt_node,label) as c) ->
       match code_elt_node with
         | NewVal (pel,_) ->
             (* replacing pattern that are not identifiers by fresh
              * identifiers that won't be used to transform the lhs of pattern
              * by lhs of identifiers *)
             let map,iel =
               List.fold_left_map
                 (fun map -> function
                    | ((PatVar {ident=i;_},_) as pat), e ->
                        (* directives are not lost => in map *)
                        IdentMap.add i pat map, (i, remove_access_directive e)
                    | pat, e ->
                        let i = fresh_ident () in
                        IdentMap.add i pat map, (i, remove_access_directive e)
                 ) IdentMap.empty pel in
             (* the actual flattening *)
             let iel = flatten_module iel in
             (* reputting the original patterns instead of the fake
              * identifiers *)
             let pel = List.map
               (fun (i,e) ->
                  match IdentMap.find_opt i map with
                    | None -> ((PatVar {ident=i;directives=[]}, copy_label label), e)
                    | Some p -> (p,e)
               ) iel in
             (* reordering the result *)
             let pelbl = reorder_for_pat_bindings create_group_list pel in
             List.map (fun (pel,b) -> (NewVal (pel,b), copy_label label)) pelbl
         | _ -> [c]
    ) lcode

(* some utils to print the dependency graph of the standard library *)
module G = GraphUtils.String.G
module Viz = GraphUtils.DefaultGraphviz(G)(struct let vertex_name x = x end)

(* What is this hack, matching directly the string "builtin" ?? *)
(* TODO: correct this, there is a function is_empty in FilePos *)
let dump_file_deps lcode =
  let iapis = get_context_linker_cache_input lcode in
  let ideps = get_get_deps_input lcode in
  let context_linker_cache = context_linker_cache iapis in
  let deps = get_deps context_linker_cache ideps in
  let map_back = map_back lcode in (* FIXME: insert all this inside the real function isntead of duplicating *)
  let filename_of int =
    let s = Filename.basename (FilePos.get_file (snd (IntMap.find int map_back)).QmlLoc.pos) in
    try Filename.chop_extension s with Invalid_argument _ -> s in
  let g = G.create () in
  let is_not_builtin a = not (String.is_prefix "builtin" a) in
  List.iter (fun (i,_) ->
               let a = filename_of i in
               if is_not_builtin a then
                 G.add_vertex g a) deps;
  List.iter
    (fun (i,set) ->
       let a = filename_of i in
       if is_not_builtin a then
         IntSet.iter
           (fun j ->
              let b = filename_of j in
              if is_not_builtin b && a <> b then
                G.add_edge g b a) set) deps;
  let package_name, _ = ObjectFiles.get_current_package () in
  let filename = Printf.sprintf "opadep_%s.dot" package_name in
  let filename =
    match ObjectFiles.get_compilation_directory () with
    | None -> filename
    | Some dir -> File.concat dir filename
  in
  OManager.unquiet "opadep: outputting @{<bright>%s@}" filename ;
  Viz.to_file filename g

let reorder_toplevel ?roots create_groups lcode =
  let () = if ObjectFiles.Arg.is_opadep () then dump_file_deps lcode in
  reorder_toplevel ?roots create_groups lcode

let rewrite_modules create_groups lcode =
  let lcode =
    lcode
    |> flatten_toplevel_module create_groups
    |> flatten_module_in_expr create_groups in
  OManager.flush_errors (); (* remove this line when s2 is removed *)
  lcode
