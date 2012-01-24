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

#<Debugvar:REORDER>

(* depends *)
module List = Base.List

(* refactoring in progress *)

(* alias *)
module TypeIdent = QmlAst.TypeIdent
module TypeIdentMap = QmlAst.TypeIdentMap

(* shorthands *)
module Q = QmlAst
module Db = QmlAst.Db

let ( |> ) x f = f x
let ( @> ) f g x = x |> f |> g


(* debug *)
let pp_code_elt f (code_elt_node,i) =
  Format.fprintf f "%d->%a\n"
    i
    QmlPrint.pp#code_elt code_elt_node


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
    { vals  : (Ident.t * QmlAst.ty option) list
    ; types : QmlAst.TypeIdent.t list
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
type directive_dep = [ `hybrid_value | `insert_server_value of Ident.t | `fun_action of QmlAst.fun_action_content option ]

(* That function fold dependencies for a given directive *)
let fold_directive_deps get_id_ident get_id_str (directive:directive_dep) fold acc =
  let ifold lst =
    List.fold_left
      (fun acc str_dep -> try fold (get_id_str str_dep) acc with Not_found -> acc)
      acc lst
  in match directive with
  | `hybrid_value ->
      ifold [Opacapi.OpaSerialize.unserialize_unsafe]
  | `insert_server_value i ->
      (try fold (get_id_ident i) acc with Not_found -> acc)
  | `fun_action _ ->
      ifold [Opacapi.FunActionServer.serialize_argument]

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

(* todo factoriser avec Reordering.types_deps *)
let get_typenames ty = QmlAstWalk.Type.fold (
  fun acc ty ->
    match ty with
    | Q.TypeName(_,tyn)-> tyn::acc
    |_ -> acc
) [] ty

let get_fields_ty ty = QmlAstWalk.Type.fold (
  fun acc ty ->
    match ty with
    | Q.TypeRecord (Q.TyRow (lf,_))-> [List.map fst lf]@acc
    | Q.TypeSum (Q.TyCol (llf,_))-> (List.map (List.map fst ) llf)@acc
    |_ -> acc
) [] ty

let get_fields_pat pat = QmlAstWalk.Pattern.fold_down (
  fun acc pat->
    match pat with
    | Q.PatRecord (_, fields, _) -> List.rev_map_append fst fields acc
    | _ -> acc
) [] pat

let get_type_dep_context ty = {
  empty_dep_context with
    types = get_typenames ty ;
    fields_groups = [get_fields_ty ty]
}


let type_deps =
  QmlAstWalk.Type.fold
    (fun acc -> function
      | QmlAst.TypeName (_, ident) -> ident :: acc
      | _ -> acc) []

let type_deps =
  QmlAstWalk.Type.fold
    (fun acc -> function
      | QmlAst.TypeName (_, ident) -> ident :: acc
      | _ -> acc) []

(* somewhat of a hack to avoid having to carry around an environment *)
let remote_call_key i =
  "__call__" ^ Ident.stident i
let remote_call_ident i =
  Ident.source (remote_call_key i)

(**
   [get_expr_dep_context] gives the toplevel direct dependencies of an
   expression
   The environment contains the identifiers in scope (both types and values)
   but not the toplevel ones
*)
let get_expr_dep_context ?filter e =
  QmlAstWalk.Expr.fold_with_exprmap
    (fun env acc e ->
       match e with
         (* nothing special for bindings except that the environment
          * grows but this is already taken care of the fold_with_env
          * function *)
         | Q.LetIn _
         | Q.Apply _
         | Q.Const _
         | Q.Bypass _
         | Q.Lambda _
         | Q.LetRecIn _ -> acc
         | Q.Coerce (_, _, ty) ->
             let deps_ty = type_deps ty in
             {acc with types = deps_ty @ acc.types }
         | Q.Match (_, _e, _pel) -> acc
             (*{ acc with  fields_groups = List.fold (fun groups (p,_)-> add_group (get_fields_pat p) groups) acc.fields_groups _pel }*)
             (*let _p = List.map pel in*)
             (* FIXME: ??? don't know what to do *)
         | Q.Path (_, dbelt, _)-> (
             let acc = Option.if_none filter (add_database acc) acc in
             (* taking the first elt of the path *)
             match List.hd dbelt with
             | Q.Db.FldKey s -> Option.if_none filter (add_root s acc) acc
             | Q.Db.NewKey
             | Q.Db.ExprKey _ ->
                 (* not possible, see the parser *)
                 assert false
           )
         | Q.Record (_, l) ->
               Option.if_none filter (add_group [List.map fst l] acc) acc

         | Q.Dot (_, _, f)
         | Q.ExtendRecord (_, f,_, _) ->
             Option.if_none filter (add_group [[f]] acc) acc

         | Q.Ident (_, i) ->
             if not (IdentMap.mem i env) then
               match filter with
                 | None -> add_val i acc
                 | Some f when f i -> add_val i acc
                 | _ -> acc
             else
               acc
         | Q.Directive (_, (`comet_call | `ajax_call _), el, tyl) -> (
             assert (tyl = []);
             match el with
             | [ Q.Ident (_, i) ] ->
                 (* when doing a remote call, we depend on the stub, not only on the implementation *)
                 {acc with vals = (remote_call_ident i, None) :: acc.vals}
             | _ -> assert false
           )
         | Q.Directive (_, dir, exprs, tys) ->
             let acc =
               match dir with
                | #directive_dep as dir ->
                    { acc with other = dir::acc.other }
                | _ -> acc
             in
             let acc =
               let fold_ty acc ty =
                 merge_dep_context
                   {empty_dep_context with types = get_typenames ty}
                   acc
               in
               (* FIXME: check what should be done wrt filter and this case,
                  surfaceDependencies and qmlDependencies desagree *)
               match filter with
               | None ->
                   (* we should fold on arguments + on the type of the directive *)
                   let acc = List.fold_left fold_ty acc tys in
                   let acc = fold_ty acc (QmlDirectives.ty dir exprs tys) in
                   acc

               | Some _ -> acc
             in
             acc

    ) empty_dep_context e


(**
   [get_local_expr_dep_context] is used when you don't want all the
   dependencies of an expression but only dependencies on the identifiers
   present in the set [names]
   it allows you to reorder/split a set of mutually recursive bindings
 *)
let get_local_expr_dep_context names expr =
  let filter i = IdentSet.mem i names in
    get_expr_dep_context ~filter expr

(**
   [get_code_elt_dep_context] gives you the toplevel dependencies of a code_elt
*)
let get_code_elt_dep_context code_elt_node =
  match code_elt_node with
    | Q.Database _ -> empty_dep_context
    | Q.NewDbValue (_, Db.Db_TypeDecl([_] ,ty)) ->
        { empty_dep_context with types = get_typenames ty}
    | Q.NewDbValue (_, Db.Db_TypeDecl(p,ty)) ->
        { empty_dep_context with
            db_roots = [db_root_of_path p] ;
            types = get_typenames ty
        }
    | Q.NewDbValue (_, ( Db.Db_Default (p, _)
                           | Db.Db_Constraint (p, _)
                           | Db.Db_Alias (_, p)
                           | Db.Db_Virtual (p, _) as db_def) ) ->
        fst(Db.foldmap_expr
              (fun dep_context e -> merge_dep_context (get_expr_dep_context e) dep_context, e)
              {empty_dep_context with db_roots = [db_root_of_path p]}
              db_def)

    | Q.NewType (_, tyl) ->
        (* FIXME: should i remove i from the dependencies?
         * probably not: the element will be dependent on itself, but
         * it seems normal since it is indeed recursive
         *)
        let contexts =
          List.map
            (fun ty_def -> get_type_dep_context ty_def.Q.ty_def_body) tyl in
        merge_dep_contexts contexts
    | Q.NewVal (_, pel)
    | Q.NewValRec (_, pel) ->
        (* FIXME: should i remove the pat vars from the dependencies? *)
        let contexts = List.map (snd @> get_expr_dep_context) pel in
        merge_dep_contexts contexts

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
let get_expr_api_context = function
  | Q.Directive (_, (`ajax_publish _|`comet_publish), el, _) -> (
      match el with
      | [ Q.Ident (_, i) ] -> [(remote_call_ident i, None)]
      | _ -> assert false
    )
  | _ -> []

let get_code_elt_api_context code_elt_node =
  match code_elt_node with
    | Q.Database _ ->
        empty_api_context
    | Q.NewDbValue (_, Db.Db_TypeDecl(p,_)) ->
        (* all the [db /map/...] define the root '/map'
         * or else in [ db /map/a : string
         *              db /map/b : string ], '/map' is never defined
         *)
      {empty_api_context with db_roots = [db_root_of_path p]; database = true}
    | Q.NewDbValue _ ->
      {empty_api_context with database = true}
    | Q.NewType (_, tyl) ->
        { empty_api_context with
          types =
            List.map (fun ty_def -> ty_def.QmlAst.ty_def_name) tyl (*; fields_groups = [get_fields_ty ty] *) }
    | Q.NewVal (_, iel)
    | Q.NewValRec (_, iel) ->
        {empty_api_context with vals = List.concat_map (fun (i,e) -> get_expr_api_context e @ [(i,None)]) iel}


(**
   These maps map identifiers to an int that identifies the declaration
   that defines them
   the int is chosen to be the number in the annot of the code_elt

   Used to compute deps from api_context
 *)
type context_linker_cache =
  { c_identifiers : int IdentMap.t
  ; c_type_identifiers : int QmlAst.TypeIdentMap.t
  ; c_db_roots : int list StringMap.t
  ; c_fields : int FieldsMap.t
  ; c_database : int list }

let empty_context_linker_cache : context_linker_cache =
  { c_identifiers = IdentMap.empty
  ; c_type_identifiers = TypeIdentMap.empty
  ; c_db_roots = StringMap.empty
  ; c_fields = FieldsMap.empty
  ; c_database = [] }

let get_context_linker_cache_input lcode =
  List.map (fun (code_elt,i) -> i, get_code_elt_api_context code_elt) lcode

let get_get_deps_input lcode =
  List.map (fun (code_elt,i) ->
    let foo = get_code_elt_dep_context code_elt in
    (* let () = (print_string @* string_of_int) i in
    let () = print_string " depends of " in
    let () = List.iter (print_string @* ((^) " ") @* ExprIdent.to_string @* fst) foo.vals in
    let () = print_string "\n" in
    *)
    code_elt, i, foo
  ) lcode



(* ************************************************************************** *)
(** {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let identMap_safe_add ident i idents =
  try IdentMap.safe_add ident i idents with
  | Invalid_argument "Base.Map.safe_add" ->
      Printf.printf "Cannot find %s\n%!" (Ident.to_string ident);
      (* Without any mean to create an expressive context for an erroe message,
         the best we can do is to assert, at least in case of raising we will
         at least have a line number in the source. *)
      assert false



(* ************************************************************************** *)
(** {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let typeIdentMap_safe_add ident i idents =
  try TypeIdentMap.safe_add ident i idents with
  | Invalid_argument "Base.Map.safe_add" ->
      (* Without any mean to create an expressive context for an erroe message,
         the best we can do is to assert, at least in case of raising we will
         at least have a line number in the source. *)
      assert false

let stringMap_safe_add ident i idents = StringMap.add ident i idents



(* ************************************************************************** *)
(** {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let fieldsMap_safe_add ident i idents =
  try FieldsMap.safe_add ident i idents with
  | Invalid_argument "Base.Map.safe_add" ->
      (* Without any mean to create an expressive context for an erroe message,
         the best we can do is to assert, at least in case of raising we will
         at least have a line number in the source. *)
      assert false


let context_linker_cache iapis =
  List.fold_left
    (fun { c_identifiers = idents
         ; c_type_identifiers =types
         ; c_db_roots = db_roots
         ; c_fields = fields
         ; c_database = database} ((i:int),api) ->
       { c_identifiers =
           List.fold_left (fun idents (ident,_) -> identMap_safe_add ident i idents) idents api.vals
       ; c_type_identifiers =
           List.fold_left (fun types ident -> typeIdentMap_safe_add ident i types) types api.types
       ; c_db_roots =
           List.fold_left (fun db_roots db_root ->
                             let prev = Option.default [] (StringMap.find_opt db_root db_roots) in
                             StringMap.add db_root (i :: prev) db_roots
                          ) db_roots api.db_roots
       ; c_fields =
           List.fold_left (fun fields g -> fieldsMap_safe_add g i fields) fields api.fields_groups
       ; c_database = if api.database then i :: database else database }
    ) empty_context_linker_cache iapis



(** @raise Not_found. *)
let identmap_find i m =
  match IdentMap.find_opt i m with
    | None ->
        (* if an exception is raised here, it means that someone depends on an
         * identifier but nobody defines it
         * it can happen in the following (legal) cases:
         * - we are reordering the client code and it contains server identifiers
         * - some identifiers are used but not yet defined
         *   (like the one used to share the runtime structure of types _v*_memo_ty)
         * And of course, it happens when you really have an unbound identifier
         *)
        (*Printf.printf "Dependencies.identmap_find: %s\n%!" (Ident.to_string i);*)
        raise Not_found
    | Some v -> v



(** @raise Not_found. *)
let typemap_find i m =
  match TypeIdentMap.find_opt i m with
    | None ->
        (*Printf.printf "Dependencies.typemap_find: %s\n%!" (TypeIdent.to_string i);*)
        raise Not_found
    | Some v -> v


let get_deps val_ { c_identifiers = vals
                  ; c_type_identifiers = types
                  ; c_db_roots = db_roots
                  ; c_fields = fields
                  ; c_database = database } ideps =
  let fold_directive_deps =
    fold_directive_deps
      (fun ident -> identmap_find ident vals)
      (fun str -> identmap_find (val_ str) vals) in
  List.map
    (fun (_,i,dep_context) ->
       let set = IntSet.empty in
       let set =
         List.fold_left
           (fun set (ident, _tyo) ->
              try IntSet.add (identmap_find ident vals) set with
                Not_found -> set)
           set dep_context.vals in
       let set =
         List.fold_left
           (fun set type_ ->
              try IntSet.add (typemap_find type_ types) set with
                Not_found -> set)
           set dep_context.types in
       let set =
         List.fold_left
           (fun set db_root ->
              try IntSet.add_list (StringMap.find db_root db_roots) set
              with Not_found -> set)
           set dep_context.db_roots in
      let set =
         if dep_context.database then
           IntSet.add_list database set
         else
           set in
       let set =
         List.fold_left
           (fun set field ->
              try IntSet.add (FieldsMap.find field fields) set with
                Not_found -> set)
           set dep_context.fields_groups in
       let set =
         List.fold_left
           (fun set directive ->
              fold_directive_deps directive IntSet.add set)
           set dep_context.other in
       (i, set)
    ) ideps

(**
   create the map that map the number of a declaration to the declaration
   itself
*)
let map_back lcode =
  List.fold_left
    (fun map (code_elt,i) ->
       IntMap.add i code_elt map
    ) IntMap.empty lcode

(**
   When given a list of code_elt, gives back the list where all newval
   are regrouped into one newval and put it last in the list
*)
let special_flatten (isRec:int -> bool) (of_int:int->'a) l =
  let isVal  = function Q.NewValRec _ | Q.NewVal _ -> true | _ -> false        in
  let getVal = function Q.NewValRec (_, l) | Q.NewVal (_, l) -> l    | _ -> assert false in
  let label = Annot.nolabel "QmlDependencies.special_flatten" in
  let newval rec_ l =
    if rec_ then Q.NewValRec (label, l) else Q.NewVal (label, l) in
    match l with
      | [i]  -> let c = of_int i in
            if isVal c then [newval (isRec i) (getVal c)]
            else [c]
      | _ ->
          let l = List.rev (List.map of_int l) in
          let vals,others = List.partition isVal l in
          (* this match ensures that we do not construct NewValRec []
           * which breaks assertions in the backend *)
          match vals with
            | [] -> others
            | _ -> others @ [ Q.NewValRec (label, (List.map getVal vals |> List.flatten)) ]

let flatten code =
  let flatten x = match x with
    | Q.NewVal (label, pel)
    | Q.NewValRec (label, pel) -> List.map (fun x -> Q.NewVal (label, [x])) pel
    | _ -> [x]
  in
  List.concat_map flatten code

let debug _f _x =
  #<If>
    (Format.printf "------@\n%a%!" _f _x)
  #<End>

(**
   This function reorders the toplevel

   Take the function that computes the transivite closure of the dependency
   relation as an argument because doing otherwise would (for now) create
   circular dependencies between libraries

*)
let reorder_toplevel val_ roots roots_addon create_groups lcode =
  let lcode = flatten lcode in
    (* FIXME: should i remove the pat vars from the dependencies? *)
  let lcode = List.mapi (fun i c -> (c,i)) lcode in
  debug pp_code lcode;
  let iapis = get_context_linker_cache_input lcode in
  let ideps = get_get_deps_input lcode in
  let {c_identifiers=vals;c_type_identifiers=types} as context_linker_cache = context_linker_cache iapis in
  let deps = get_deps val_ context_linker_cache ideps in

  debug (fun f l -> List.iter (fun (i,s) -> Format.fprintf f "%d -> %a" i pp_set s) l) deps;
  let roots =
    if roots = [] then [] else
      List.rev_append
        (
          List.rev_append
            (
              (* if we have roots that are not defined, we simply ignore them *)
              List.filter_map (fun i -> IdentMap.find_opt i vals) roots
            )
            ( TypeIdentMap.fold (fun _ v acc -> v::acc) types [] ) (*todo virer ca quand on aura vire libconvert *)
        )
        (
          List.fold_left
            (fun acc (code, i) ->
              match code with
              | Q.Database _ -> i::acc
              | Q.NewDbValue _ -> i::acc
              | _ -> acc
            )
            []
            lcode
        )
  in
  let roots_addon =
    IdentMap.fold
      (fun ident list acc ->
        Option.default_map
          acc
          (fun val' ->
            IntMap.add val'
              (List.filter_map (fun i -> IdentMap.find_opt i vals) list) acc)
          (IdentMap.find_opt ident vals)
      )
      roots_addon
      IntMap.empty
  in
  let ((groups : (int * bool * IntSet.t) list),_) = create_groups roots roots_addon deps in
  debug (fun f l -> List.iter (fun (i,isrec,s) -> Format.fprintf f "%d(%B) -> %a" i isrec pp_set s) l) groups;
  let map_back = map_back lcode in
  let declss = List.map (fun (_, _, group) -> IntSet.fold (fun i acc -> i::acc) group []) groups
  (* this should be equivalent to: List.map (fun (_, group) -> IntSet.elements group) groups
     but somehow, it is not ! maybe the rest depends on the order ? *)
  in
  let isRec =
    let deps = IntMap.from_list deps in
    fun i -> IntSet.mem i (IntMap.find i deps)
  in
  let of_int i = IntMap.find i map_back in
  let lcode = List.concat_map (special_flatten isRec of_int) declss in
    (*debug pp_code lcode;*)
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
   |> List.fold_left safe_union IdentSet.empty in
  List.map (get_local_expr_dep_context possible_deps) exprs

let regroup map_expr (_,intset) =
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
let reorder roots roots_addon create_groups lcode =
    reorder_toplevel roots roots_addon create_groups lcode


(*------------------------------------*)
(*------ not separated cleaning  -----*)
(*------------------------------------*)

let idents_of_annots unreachable_annots code =
  List.fold_left
    (fun acc -> function
     | Q.NewValRec (_, iel)
     | Q.NewVal (_, iel) ->
         List.fold_left
           (fun acc (i, expr) ->
              let annot = Q.QAnnot.expr expr in
              if IntSet.mem (Annot.to_int annot) unreachable_annots then
                IdentSet.add i acc
              else
                acc) acc iel
     | _ -> assert false) IdentSet.empty code

let is_root e =
  QmlAstWalk.Expr.traverse_exists
    (fun tra -> function
     | Q.Apply _ -> true
     | Q.Lambda _ -> false
     | e -> tra e) e
let get_unreachable_idents_of_code val_ roots server_code client_code =
  let code = server_code @ client_code in
  let lcode = flatten code in
  let roots =
    List.filter_map
      (function
       | Q.NewVal (_, [_i, e])
       | Q.NewValRec (_, [_i, e]) ->
           if List.exists (fun j -> Ident.equal _i j) roots || is_root e then (
             (*Printf.printf "%s is a root\n%!" (Ident.to_string _i);*)
             let annot = Q.QAnnot.expr e in
             Some (Annot.to_int annot)
           ) else None
       | _ -> assert false) lcode in
  let lcode =
    List.map (function
              | Q.NewVal    (_, [_, e])
              | Q.NewValRec (_, [_, e]) as c ->
                  let annot = Q.QAnnot.expr e in
                  (c, Annot.to_int annot)
              | _ -> assert false) lcode in
  let iapis = get_context_linker_cache_input lcode in
  let ideps = get_get_deps_input lcode in
  let context_linker_cache = context_linker_cache iapis in
  let deps = get_deps val_ context_linker_cache ideps in
  let unreachable_annots = GraphUtils.Int.give_unreachable_nodes roots deps in
  let unreachable_idents = idents_of_annots unreachable_annots code in
  let filter code =
    (*Printf.printf "%d unreachable idents\n%!" (IdentSet.size unreachable_idents);*)
    QmlAstWalk.Code.filter_binding
      (fun (i,_) -> not (IdentSet.mem i unreachable_idents)) code in
  unreachable_idents, filter server_code, filter client_code
