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
   Alpha renaming of the Ast

   Alpha renaming is done the following way:
   - first scan the toplevel and rename all toplevel identifiers
     it also records which of these identifiers are modules
     (so that you have enough information when you see an 'open' later)
     Since all the identifiers in the AST have the same type, an intermediate
     datastructure is introduced to keep the alpha renamed toplevel identifiers
     and the not alpha renamed expressions
     this pass creates the toplevel environment, and a part of the global
     environment
   - then fold into the expressions
     this pass manages three kind of environment:
     - the toplevel one (the one mentioned three lines above)
       when an identifier is not (locally) bound, it is looked up in the toplevel
       environment
       contains bindings for variables and type names (independently)
     - the local one
       contains bindings for both variable and type names (independently)
       it maps source name to unique names
       only variables in the scope appear in this environment
     - the folding one
       contains a map from source type variable to unique type variables
       contains also a map from unique identifiers to property about them
       (mainly to see if they have been used or not)
       type variables need to appear in this environment (contrary to variable
       and type names) because they are (except in typedef) implicitely bound
       eg: when you see [x = (2 : 'a) y = (2 : 'a) x], you have to keep the
       keep the environment that renamed 'a in the branch of 'x' to some unique
       name to be able to give the same unique name to the 'a in the branch of
       'y'

   Type variables are bound to the closest surrounding field (or toplevel
   definition) in coercions
   In type definitions, they are bound to the type definition explicitely
   or bound to the surrounding module type

*)

(* depends *)
module List = BaseList
module Format = BaseFormat
module String = BaseString

(* HACK : please, clean-up in opa lang *)
module Parser_utils = OpaParserUtils

open SurfaceAst
open SurfaceAstHelper

let warning_set = WarningClass.Set.create ()
let warn_duplicateL0 =
  let doc = "Duplication of names at level 0" in
  WarningClass.create ~name:"duplicateL0" ~doc ~err:false ~enable:true ()
let warn_unused =
  let doc = "Complain when a variable is unused" in
  WarningClass.create ~name:"unused" ~doc ~err:false ~enable:(#<If:TESTING>false#<Else>true#<End>) ()
let warn_used =
  let doc = "Complain when a variable starting with one _ is used" in
  WarningClass.create ~parent:warn_unused ~name:"used" ~doc ~err:false ~enable:(#<If:TESTING>false#<Else>true#<End>) ()

let () = WarningClass.Set.add_all warning_set [
  warn_duplicateL0;
  warn_unused;
  warn_used;
]

(*--------------------------------*)
(*------------ utils -------------*)
(*--------------------------------*)
let stringmap_filter_map f map =
  StringMap.map (fun x -> Option.get (f x)) (StringMap.filter_val (fun x -> f x <> None) map)


(*--------------------------------*)
(*----------- typedefs -----------*)
(*--------------------------------*)
type 'a result =
  | Result of 'a
  | Error of string

(**
   Normal represents an identifier who wasn't defined as [ident = {{ .. }}]
   OpenedIdent represents a path to an identifier
   In "m = \{\{ x = 2 \}\} ... open m ... do x ... "
   The map would be "m -> 'node(m) -> node(x)', uniq(m), \[\]"
                    "x -> 'node(x)'           , uniq(x), \[x\]"
   That way, when you see 'x', you can replace it by 'm.x'
*)
type var_in_scope =
  | Normal of Ident.t
  | OpenedIdent of string Tree.t * Ident.t * string list

type scope_env =
    { scope_vars : var_in_scope StringMap.t;
      scope_types : Ident.t StringMap.t ;
    }

(* these properties are used to issue warnings *)
type properties =
    { used : bool
    ; no_warning : unit }

(**
   Some information about the identifiers: if they were used, etc.
*)

type information = {
  properties : properties;
  is_type : bool;
  exported : bool;
  ident : Ident.t;
  pos : FilePos.pos;
  warning_when: [`used | `unused | `never];
}

(**
   The different kind of environment needed
*)

type toplevel_env =
    { tnames : (var_in_scope * FilePos.pos) list StringMap.t
    ; ttypes : (Ident.t * FilePos.pos) list StringMap.t
    }
type 'a folding_env =
    { fglobal : information IdentMap.t
    ; ftypevars : Ident.t StringMap.t
    ; data: 'a (* - when renaming types, it contains a boolean saying whether
                * type variable are explicitely bound or not
                * - when renaming patterns, it contains a set containing
                * the identifiers previously defined in the pattern
                * - when renaming expressions, it contains unit
                *)
    }

type local_env =
    { lnames: var_in_scope StringMap.t
    ; ltypes : Ident.t StringMap.t
    }

type 'a all_envs =
    { t : toplevel_env
    ; l : local_env
    ; f : 'a folding_env
    }

type env = {
  all_envs : unit all_envs; (* the environment for renaming *)
  maptoident_val : Ident.t StringMap.t;
  maptoident_typ : Ident.t StringMap.t;
}


(*-------------------------------------*)
(*---------- debug printing -----------*)
(*-------------------------------------*)

let pp_key f map =
  StringMap.iter (fun name key ->
                    let is_m =
                      match key with
                        | [OpenedIdent _] -> true
                        | _ -> false in
                    Printf.fprintf f "%s-%d-%B " name (List.length key) is_m
                 ) map;
  Printf.fprintf f "\n\n%!"

let pp_stringmap_aux p f set =
  StringMap.iter (fun k v -> Format.fprintf f "@ (%S,%a);" k p v) set
let pp_stringmap p f map =
  Format.fprintf f "@[@[<hv2>StringMap.of_list [%a@]@ ]@]" (pp_stringmap_aux p) map
let pp_identmap_aux p f set =
  IdentMap.iter (fun k v -> Format.fprintf f "@ (%s,%a);" (Ident.to_string k) p v) set
let pp_identmap p f map =
  Format.fprintf f "@[@[<hv2>IdentMap.of_list [%a@]@ ]@]" (pp_identmap_aux p) map
let pp_list_aux p f list =
  List.iter (fun v -> Format.fprintf f "@ %a;" p v) list
let pp_list p f list =
  Format.fprintf f "@[@[<hv2>[%a@]@ ]@]" (pp_list_aux p) list
let pp_ident f i =
  Format.fprintf f "%s" (Ident.to_string i)
let pp_fixme f _ = Format.fprintf f "<FIXME>"

let pp_var_in_scope f = function
  | Normal i -> Format.fprintf f "Normal %a" pp_ident i
  | OpenedIdent (_,i,l) -> Format.fprintf f "OpenedIdent (%a,%d)" pp_ident i (List.length l)
let pp_toplevel_env f { tnames = tnames
                      ; ttypes = ttypes } =
  Format.fprintf f "@[@[<v 2>{@,@[<2>tname:@ %a@]@ \
                                @[<2>ttypes:@ %a@]@]@,}@]"
    (pp_stringmap (fun f l -> pp_list pp_var_in_scope f (List.map fst l))) tnames
    pp_fixme ttypes
let pp_information = pp_fixme
let pp_folding_env f {fglobal=fglobal; _} =
  Format.fprintf f "{fglobal=%a; ...}"
    (pp_identmap pp_information) fglobal
let pp_local_env = pp_fixme
let pp_all_envs ff {t = t; l = l; f = f} =
  Format.fprintf ff "@[@[<v 2>{@,@[<2>t:@ %a@]@ \
                                 @[<2>l:@ %a@]@ \
                                 @[<2>f:@ %a@]@]@,}@]"
    pp_toplevel_env t
    pp_local_env l
    pp_folding_env f
let print_and_return env =
  Format.printf "%a@." pp_all_envs env;
  env

(*-------------------------------*)
(*--------- hierarchy -----------*)
(*-------------------------------*)
(* was meant for incremental compilation, could possibly be removed
 * if it doesn't matter anymore
 * this way, we wouldn't have very long identifiers *)
type hierar = int * string list
let empty_hierar = (0,[])
let max_hierar_high = 6
let (+>) s ((n,l) as h : hierar) =
  if n > max_hierar_high then h else (n+1,s :: l)
let fake_hierar context = context +> empty_hierar

(* Generate an exprIdent from a (source) name and a hierarchy
 * (list of string to make renaming somewhat stable) *)
let ident_of_string ~label name ((_,l):hierar) =
  let filename = FilePos.get_file label.QmlLoc.pos in
  let buff = FBuffer.make 0 in
  let descr = List.fold_left FBuffer.add buff (l |> List.rev) |> FBuffer.contents in
  Ident.next ~filename ~descr name
let source_of_string = Ident.source


(*-------------------------------------*)
(*----------- initial env -------------*)
(*-------------------------------------*)

let init_property' ?warning ~exported ~is_type ident pos =
  { properties = { used = false; no_warning = () }
  ; ident = ident
  ; is_type = is_type
  ; exported = exported
  ; warning_when =
      (match warning with
      | None -> Ident.renaming_should_warn_when ident
      | Some v -> v)
  ; pos = pos }
let init_property ?warning ~exported ~is_type ident label =
  init_property' ?warning ~exported ~is_type ident label.QmlLoc.pos

let empty_local_env =
  { lnames = StringMap.empty
  ; ltypes = StringMap.empty
  }

let empty_folding_env =
  { fglobal = IdentMap.empty
  ; ftypevars = StringMap.empty
  ; data = ()
  }

let empty_toplevel_env =
  { tnames = StringMap.empty
  ; ttypes = StringMap.empty
  }

let empty_envs =
  { t = empty_toplevel_env
  ; l = empty_local_env
  ; f = empty_folding_env
  }

let with_typevars r t =
  {r with f = {r.f with ftypevars = t }}
let with_tnames r tnames =
  {r with t = {r.t with tnames = tnames }}
let with_data r v =
  {r with f = {r.f with data = v}}
let with_type_env r v =
  { r with f = {r.f with ftypevars = v }}
let with_lnames r v =
  { r with l = {r.l with lnames = v }}
let with_global r g =
  { r with f = {r.f with fglobal = g }}

let init_env compiler_inserted_names compiler_inserted_types =
  let nopos = FilePos.nopos "SurfaceAstRenaming.init_env" in
  let tnames,fglobal =
    List.fold_left
      (fun (tnames, fglobal) name ->
         let ident = ident_of_string ~label:(SurfaceAstCons.Label.builtin()) name (fake_hierar "renaming") in
         StringMap.add name [(Normal ident,FilePos.nopos "Builtin name")] tnames,
         IdentMap.add ident (init_property' ~exported:false ~is_type:false ident
                               nopos) fglobal
      ) (StringMap.empty, IdentMap.empty) compiler_inserted_names in
  let ttypes, fglobal =
    List.fold_left
      (fun (ttypes, fglobal) name ->
         let pos = FilePos.nopos "Builtin type" in
         let ident = ident_of_string  ~label:(SurfaceAstCons.Label.builtin()) name (fake_hierar "renaming") in
         StringMap.add name [(ident,pos)] ttypes,
         IdentMap.add ident (init_property' ~exported:false ~is_type:true ident nopos) fglobal
      ) (StringMap.empty, fglobal) compiler_inserted_types in
  { all_envs =
      { t = {tnames = tnames; ttypes = ttypes}
      ; f = {empty_folding_env with fglobal = fglobal}
      ; l = empty_local_env };
    maptoident_typ = StringMap.empty;
    maptoident_val = StringMap.empty;
  }


(*-----------------------*)
(*--- managing 'open' ---*)
(*-----------------------*)
(* a few functions to deal with the 'open' node *)

(* tree_of_expr gives
       x
      / \
    [y   z]
     |   |
    []  [w]
         |
         []
   from:
   x = {{ y = 2 z = {{ w = 3 }} }}

   tree_option_of_expr gives you a result only if the argument is a module,
   or a path to a module
*)
let ignore_for_modules =
  let module D = SurfaceAstDecons in
  [ D.Remove.Basic.expand
  ; D.Remove.Basic.opacapi
  ; D.Remove.Basic.opavalue_directive
  ; D.Remove.Basic.letin
  ; D.Remove.Basic.coerce
  ; D.Remove.Basic.open_
  ; D.Remove.Basic.slicer_directive ]

let rec tree_of_expr name l =
  let children =
    List.map
      (fun (field,e) ->
         Option.default
           (Tree.leaf field)
           (tree_option_of_expr field e)
      ) l in
    Tree.Tree (name,children)
and tree_option_of_expr name e =
  match SurfaceAstDecons.FoldThrough.fields ~through:ignore_for_modules e with
    | None -> None
      (* the following expression was used to be able to open records modulo
       * alias (but it didn't in all cases, especially at toplevel)
       * path_expr_to_module old_tree_map e *)
    | Some fields -> Some (tree_of_expr name fields)

(*----------------------------------*)
(*---- error/warning management ----*)
(*----------------------------------*)
(* Gives you an advice when you have an unbound variable *)
let filter_closest name li =
  let dist = (float_of_int (String.length name)) *. 2. /. 3. in
  let li = List.filter (fun (_, d) -> d < dist) li in
  let li =
    List.fold_left (fun (li, (old, delta)) (name, d) ->
                      let d' =  old -. d in if d' <= delta
                      then ((name, d)::li, (d, d'))
                      else (li, (old, delta)) ) ([], (0., dist)) li |> fst |> List.rev in
  li
let get_closest_field_names typo l =
  HintUtils.get_closest_names (List.map fst l) typo

(* Error/Warning: Using OpaError *)

let exists_duplicatesL0 = ref false
let duplicate_type_declarations = ref false
let print_duplicatesL0 () =
  if !exists_duplicatesL0 then
    OManager.warning ~wclass:warn_duplicateL0
      "@[<2>  You have @{<bright>duplicate definitions@} at toplevel.@\nThe compilation result can depend on source files order.@]";
  if !duplicate_type_declarations then
    OManager.serror "You have duplicate type declarations, which is forbidden."

let make_placeholder label name = ident_of_string ~label "renaming_placeholder" (fake_hierar name)
let is_placeholder ident = Ident.original_name ident = "renaming_placeholder"
let pos_of_label label = QmlLoc.pos label
let string_of_label label = FilePos.to_string (pos_of_label label)
(* Following the standard layout for error messages *)
let make_error error label fmt =
  let context = OpaError.Context.pos (pos_of_label label) in
  error context ("@[<2>  "^^fmt^^"@]")
let serror label = make_error OpaError.serror label
let error label = make_error OpaError.error label
let unbound kind name all_env label =
  let string, names_in_scope =
    match kind with
      | `var ->
          "variable",
          List.uniq (StringMap.keys all_env.l.lnames @ StringMap.keys all_env.t.tnames)
      | `typ ->
          "type name",
          List.uniq (StringMap.keys all_env.l.ltypes @ StringMap.keys all_env.t.ttypes) in
  serror label "the %s @{<bright>%s@} is unbound." string name;
  OManager.printf "%a%!" (HintUtils.pp_suggestion names_in_scope) name;
  make_placeholder label name

let non_linear_pattern name label =
  serror label "the variable @{<bright>%s@} appears several times (in function parameters or in a pattern)." name
let invalid_open label =
  serror label "you are trying to @{<bright>open@} an invalid expression.";
  [], make_placeholder label "open", []
let invalid_coerced_open label =
  serror label "you are applying a wrong @{<bright>coercion@} on the @{<bright>opened@} expression.";
  fun x -> x

let check_for_toplevel_duplicates val_map type_map =
  exists_duplicatesL0 := false;
  StringMap.iter (fun name l ->
    match l with
      | [] -> invalid_arg "SurfaceAstRenaming.check_for_toplevel_duplicates"
      | [_] -> ()
      | _ ->
          let positions = List.map snd l in
          exists_duplicatesL0 := true;
          let origins = List.filter_map
            (function
               | (OpenedIdent (_,ident,(_ :: _ as path)),_) ->
                   Some (String.concat_map "." Base.identity (Ident.original_name ident :: path))
               | _ ->
                   None
            ) l in
          let n1 = List.length l in
          let n2 = List.length origins in
          OManager.warning ~wclass:warn_duplicateL0
            "%a@\n@[<2>  @{<bright>%s@} is defined @{<bright>%d@} times at the toplevel%s.@]"
            (Format.pp_list "@\n" FilePos.pp_pos) positions
            name n1 (if n2 = 0 then "" else Printf.sprintf " (%d of which by an 'open' statement)" n2)
                 ) val_map;
  StringMap.iter (fun name l ->
    match l with
    | [] -> assert false
    | [_] -> ()
    | _ ->
        duplicate_type_declarations := true;
        let positions = List.map snd l in
        OManager.serror
          "%a@\n@[<2>  The type @{<bright>%s@} is defined %d times."
          (Format.pp_list "@\n" FilePos.pp_pos) positions
          name
          (List.length l)
  ) type_map

let is_exported i infos =
  try (IdentMap.find i infos).exported
  with Not_found -> false
let exported_var global_env l =
  let rec aux o = function
    | (OpenedIdent (_,ident,[]),pos) :: l
    | (Normal ident,pos) :: l ->
        if is_exported ident global_env then
          aux (Some (ident,pos)) l
        else
          aux o l
    | _ :: l -> aux o l
    | [] -> o in
  aux None l

let check_for_unused_names identmap toplevel_names =
  if WarningClass.is_warn warn_used || WarningClass.is_warn warn_unused then (
    IdentMap.iter
      (fun val_ key ->
         if not (FilePos.is_empty key.pos) && (* no warning for compiler introduced names *)
            not key.is_type (* no warnings for type *)
         then
           if not key.properties.used && key.warning_when = `unused then
             let l = StringMap.find_opt (Ident.original_name val_) toplevel_names in
             let v =
               match l with
               | None -> None
               | Some l -> Option.map fst (exported_var identmap l) in
             match v with
             | Some x when Ident.equal val_ x -> () (* no warning for the exported names either *)
             | _ ->
                 OManager.warning ~wclass:warn_unused "%a@\n  Unused %s %s."
                   FilePos.pp_pos key.pos
                   (if key.is_type then "type name" else "variable")
                   (Ident.original_name val_)
           else if key.properties.used && key.warning_when = `used then
             OManager.warning ~wclass:warn_used "%a@\n  Used %s %s."
               FilePos.pp_pos key.pos
               (if key.is_type then "type name" else "variable")
               (Ident.original_name val_)
      ) identmap;
  )

(* Check that a value is not bound several times in a set of recursives bindings
 * like [rec x() = void and x() = void] *)
let rec check_unicity ~case ~compare_by iel =
  match iel with
    | [] -> ()
    | v :: t ->
        let (i, label) = compare_by v in
        if List.exists (fun w -> i = fst (compare_by w)) t then
          serror label "@{<bright>%s@} is bound multiple times in %s." i case;
        check_unicity ~case ~compare_by t

(*----------------------------*)
(*---------- tuple -----------*)
(*----------------------------*)
(*
   Because the set of types tuple_2, tuple_3, ... is extensible, we need
   to take special care of them
   The following reference holds a map of the size of the tuple used in the code
   It should be given to the pass that actually defines the types
   These types are not defined in the toplevel map like the other types, so they
   need to be added at the end (so that looking for the unique name of "tuple_2"
   doesn't raise Not_found)
*)
let tuples = ref IntMap.empty
let add_tuple_n num ident = tuples := IntMap.safe_add num ident !tuples
let get_tuple_n num = IntMap.find_opt num !tuples
let get_tuple_size name =
  match String.slice '_' name with
    | ["tuple"; s] ->
        ( match Base.int_of_string_opt s with
            | None -> None
            | Some n -> if n >= 0 then Some n else None
        )
    | _ -> None
(** Interface to the stuff on tuples *)
let add_tuple mk_ident name =
  match get_tuple_size name with
    | None -> `none
    | Some n ->
        match get_tuple_n n with
          | None -> let ident = mk_ident () in
                    add_tuple_n n ident;
                    `new_ ident
          | Some ident -> `old ident
let get_tuple_int_map () =
  let v = !tuples in
  tuples := IntMap.empty;
  v
let set_tuple_int_map map =
  tuples := map

let get_tuple_string_map () =
  IntMap.fold
    (fun n ident acc -> StringMap.add (Printf.sprintf "tuple_%d" n) ident acc)
    !tuples
    StringMap.empty

(*-------------------------*)
(*-- pattern environment --*)
(*-------------------------*)
(* ie the value of the field data when foldmapping a pattern
 * it is the set of the all the names defined in the current left hand side of a rule
 *)
let empty_pat_env =
  StringSet.empty

let is_hiding_pat_var name all_env =
  StringSet.mem name (snd all_env.f.data)

(*--------------------------------*)
(*---- updating environments -----*)
(*--------------------------------*)
(* tags an identifier as used when looking it up *)
let use_var ident all_env =
  let fglobal = all_env.f.fglobal in
    if is_placeholder ident then
      all_env
    else
      try
        let old_prop = IdentMap.find ident fglobal in
        let new_prop = {old_prop with properties = {old_prop.properties with used = true}} in
        let fglobal = IdentMap.add ident new_prop fglobal in
        {all_env with f = {all_env.f with fglobal = fglobal}}
      with
        Not_found ->
          all_env

let add_type name hierar all_env label =
  let ident = ident_of_string ~label name hierar in
  let infos = init_property ~exported:true ~is_type:true ident label in
    {all_env with l = {all_env.l with ltypes = StringMap.add name ident all_env.l.ltypes};
                  f = {all_env.f with fglobal = IdentMap.add ident infos all_env.f.fglobal}},
  ident
let push_in_front k v map = try StringMap.add k (v :: (StringMap.find k map)) map with Not_found -> StringMap.add k [v] map
let add_global_type name hierar all_env label =
  let ident = ident_of_string ~label name hierar in
  let infos = init_property ~exported:true ~is_type:true ident label in
    {all_env with t = {all_env.t with ttypes = push_in_front name (ident,label.QmlLoc.pos) all_env.t.ttypes};
                  f = {all_env.f with fglobal = IdentMap.add ident infos all_env.f.fglobal}},
  ident

let add_type_var name hierar all_env label =
  let ident = ident_of_string ~label name hierar in
  let folding_env = all_env.f in
    ({all_env with f =
         {folding_env with
            ftypevars = StringMap.add name ident folding_env.ftypevars}},
     ident)

let add_var ?(no_warning=false) ?(exported=true) name hierar all_env label =
  let ident = ident_of_string ~label name hierar in
  let warning = if no_warning then Some `never else None in
  let infos = init_property ?warning ~exported ~is_type:false ident label in
  {all_env with l = {all_env.l with lnames = StringMap.add name (Normal ident) all_env.l.lnames};
     f = {all_env.f with fglobal = IdentMap.add ident infos all_env.f.fglobal}},
  ident
let add_opened_var name tree ident path all_env =
  let all_env = use_var ident all_env in
  {all_env with
     l = {all_env.l with
            lnames = StringMap.add name (OpenedIdent (tree, ident, path @ [name])) all_env.l.lnames}}
(*let add_opened_var_global name tree ident path all_env =
  let all_env = use_var ident all_env in
  let old_l = Option.default [] (StringMap.find_opt name all_env.t.tnames) in
  {all_env with
     t = {all_env.t with
            tnames = StringMap.add name (OpenedIdent (tree, ident, path @ [name]) :: old_l) all_env.t.tnames}}*)
let add_pat_var ?no_warning {SurfaceAst.ident=name;directives=directives} hierar all_env label =
  if is_hiding_pat_var name all_env then
    non_linear_pattern name label;
  let exported, bindings = all_env.f.data in
  let all_env = with_data all_env (exported, StringSet.add name bindings) in
  let env,ident = add_var ?no_warning ~exported name hierar all_env label in
  env, {SurfaceAst.ident ;SurfaceAst.directives}
(* do we really need to give all_env back all the time? *)



(*--------------------------------*)
(*-- look up in the environment --*)
(*--------------------------------*)
let get_type name all_env label =
  let ident, all_env =
    match StringMap.find_opt name all_env.l.ltypes with
      | None ->
          ( match StringMap.find_opt name all_env.t.ttypes with
              | None ->
                  ( match add_tuple (fun () -> source_of_string name) name with
                      | `none ->
                          (* unbound type *)
                          unbound `typ name all_env label, all_env
                      | `old ident -> ident, all_env
                      | `new_ ident ->
                          (* definition of a new tuple type *)
                          ident,
                          with_global all_env
                            (IdentMap.add
                               (* For exported, either value is good, because we won't look at it
                                * since the type doesn't appear in the toplevel environment *)
                               ident (init_property ~exported:true ~is_type:true ident label)
                               all_env.f.fglobal)
                  )
              | Some [] -> assert false
              | Some ((ident,_pos) :: _) ->
                  ident, all_env
          )
      | Some ident -> ident, all_env
  in
    (use_var ident all_env).f, ident
let get_typevar name hierar all_env label =
  (* if the variable is bound to a typedef, use it *)
  match List.find_opt (fun ident -> Ident.original_name ident = name) all_env.f.data with
    | None ->
        (* else, it is in the environment, use it *)
        ( match StringMap.find_opt name all_env.f.ftypevars with
            | None ->
                (* else define it in the environment *)
                let all_env, ident = add_type_var name hierar all_env label in
                all_env.f, ident
            | Some ident ->
                all_env.f, ident)
    | Some ident ->
        all_env.f, ident
let get_var_from_var_in_scope all_env label = function
  | Normal ident ->
      (use_var ident all_env).f, Ident ident
  | OpenedIdent (_tree, ident, path) ->
      (* when saying [m = {{x = 1}} open m x]
       * m is marked as used (x doesn't even appear in the global environment)
       *)
      (use_var ident all_env).f, Parser_utils.undecorate (Parser_utils.dot_path (Ident ident, label) path)

let get_var_from_toplevel name all_env label =
  match StringMap.find_opt name all_env.t.tnames with
    | Some ((var_in_scope,_) :: _) ->
        get_var_from_var_in_scope all_env label var_in_scope
    | Some [] ->
        (* not well formed toplevel environment *)
        failwith (Printf.sprintf "SurfaceAstRenaming: get_var: malformed environment: %s" name)
    | None ->
        (* unbound variable *)
        all_env.f, Ident (unbound `var name all_env label)

let get_var name all_env label =
  match StringMap.find_opt name all_env.l.lnames with
    | Some var_in_scope -> get_var_from_var_in_scope all_env label var_in_scope
    | None -> get_var_from_toplevel name all_env label


(*--------------------------*)
(*-- renaming for types ----*)
(*--------------------------*)
(*
   The 'exported' functions are grouped at the end
   These functions have all type :
   all_envs -> string list -> 'a -> all_env, 'a
   Technically, this function fold with the full environment
   The local environment and toplevel environment should not go up the tree,
   but since there is no bindings in type expressions (except for type variables
   but their map is already in the folding environment), it makes no difference
   and it is simpler to write
*)

let f_list_aux f all_env hierar l =
  List.fold_right
    (fun x (folding_env,l) ->
       let f_env, x = f {all_env with f = folding_env} hierar x in
       f_env, x :: l
    ) l (all_env.f, [])

let rec f_typeinstance_node label all_env hierar (Typeident ident,tyl) =
  let f_env, ident = get_type ident all_env label in
  let f_env, tyl = f_tys {all_env with f = f_env} hierar tyl in
  f_env, (Typeident ident, tyl)

and f_arrow_t all_env hierar (arrow_t_node, label) =
  let f_env, arrow_t_node = f_arrow_t_node all_env hierar arrow_t_node in
  f_env, (arrow_t_node, label)
and f_arrow_t_node all_env hierar (row_t, ty) =
  let f_env, row_t = f_row_t all_env hierar row_t in
  let f_env, ty = f_ty {all_env with f = f_env} hierar ty in
  f_env, (row_t, ty)

and f_row_t all_env hierar (v, label) =
  let f_env, v = f_row_t_node label all_env hierar v in
  f_env, (v, label)
and f_row_t_node label all_env hierar (TyRow (fields, rowvaro)) =
  let f_env, fields = f_fields_t_node_list all_env hierar fields in
    match rowvaro with
      | None -> f_env, TyRow (fields, None)
      | Some (Rowvar v) ->
          let f_env, v = f_typevar label {all_env with f = f_env} hierar v in
          f_env, TyRow (fields, Some (Rowvar v))

and f_fields_t_node_list all_env hierar fields =
  f_list_aux f_fields_t_node all_env hierar fields

and f_module_fields all_env hierar fields =
  let original_ftypevars = all_env.f.ftypevars in
  let new_ftypevars = StringMap.empty in
  let f_env, fields =
    List.fold_left_map
      (fun f_env (field,ty) ->
         let f_env, ty = f_ty {all_env with f = {f_env with ftypevars = new_ftypevars}} hierar ty in
         (* rev_map is used so that variables have the same order as in the source
          * it shouldn't matter but it is easier to read *)
         let defined_typed_vars = List.rev_map SurfaceAstHelper.flatvar (StringMap.elts f_env.ftypevars) in
         let ty = if defined_typed_vars = [] then ty else (TypeForall (defined_typed_vars, ty), Parser_utils.nlabel ty) in
         f_env, (field,ty)) all_env.f fields in
  {f_env with ftypevars = original_ftypevars}, fields

and f_fields_t_node all_env hierar (field_name, ty) =
  let f_env, ty = f_ty all_env hierar ty in
  f_env, (field_name, ty)

and f_sum_t_list all_env hierar sum =
  f_list_aux f_sum_t all_env hierar sum
and f_sum_t all_env hierar (sum_t_node, label) =
  let f_env, sum_t_node = f_sum_t_node label all_env hierar sum_t_node in
  f_env, (sum_t_node, label)
and f_sum_t_node label all_env hierar = function
  | SumName ti ->
      let f_env, ti = f_typeinstance_node label all_env hierar ti in
      f_env, SumName ti
  | SumRecord row_t ->
      let f_env, row_t = f_row_t_node label all_env hierar row_t in
      f_env, SumRecord row_t
  | SumVar (Colvar name) ->
      let f_env, name = f_typevar label all_env hierar name in
      f_env, SumVar (Colvar name)

and f_ty all_env hierar (ty_node, label) =
  let f_env, ty_node = f_ty_node label all_env hierar ty_node in
  f_env, (ty_node, label)
and f_tys all_env hierar tys =
  f_list_aux f_ty all_env hierar tys
and f_ty_node label (all_env:Ident.t list all_envs) hierar = function
  | TypeExternal
  | TypeConst _ as t ->
      all_env.f, t
  | TypeVar (Flatvar name) ->
      let f_env, name = f_typevar label all_env hierar name in
      f_env, TypeVar (Flatvar name)
  | TypeArrow r ->
      let f_env, r = f_arrow_t_node all_env hierar r in
      f_env, TypeArrow r
  | TypeRecord r ->
      let f_env, r = f_row_t_node label all_env hierar r in
      f_env, TypeRecord r
  | TypeSumSugar l ->
      let f_env, l = f_sum_t_list all_env hierar l in
      f_env, TypeSumSugar l
  | TypeNamed ti ->
      let f_env, ti = f_typeinstance_node label all_env hierar ti in
      f_env, TypeNamed ti
  | TypeForall (vars, t) ->
      let original_vars = vars in
      let original_ftypevars = all_env.f.ftypevars in
      let f_env, vars =
        (* put the vars in the environment *)
        List.fold_left_map
          (fun f_env (Flatvar var) ->
             let all_env, var = add_type_var var hierar {all_env with f = f_env} label in
             all_env.f, (Flatvar var)) all_env.f vars in
      (* rename the underlying type *)
      let f_env, t = f_ty {all_env with f = f_env} hierar t in
      let f_env = (* remove the bindings for the quantified variables
                   * (and restore the original one if any) *)
        {f_env with ftypevars =
            List.fold_left
              (fun (map:_ StringMap.t) (Flatvar v) ->
                 try
                   StringMap.add v
                     (StringMap.find v original_ftypevars) map
                 with Not_found ->
                   StringMap.remove v map
              ) f_env.ftypevars original_vars} in
      f_env, TypeForall (vars,t)
  | TypeModule fields ->
      let f_env, fields = f_module_fields all_env hierar fields in
      f_env, TypeRecord (TyRow (fields, None))

and f_flatvar label (all_env:Ident.t list all_envs) hierar (Flatvar name) =
  let f_env, name = f_typevar label all_env hierar name in
  f_env, Flatvar name
and f_typevar label all_env hierar name =
  get_typevar name hierar all_env label
let f_flatvars label x y z = f_list_aux (f_flatvar label) x y z

(**
   The field all_env.f.data contains the variable bound to the surrounding typedef
   or [] if we are in a coercion
   When looking for a variable, this field is looked up BEFORE the field all_env.f.ftypevars

  Meaning of empty_type_env:
  - true: ignore the given type var environment and use the empty env instead
    used when opening a new scope for typevars
  - false: take the given type_env

  Meaning if give_original:
  - true: ignore the computed type var environment, and give back the one
          initially given to the function
    used when closing a new scope
    eg: [type t('a) = 'a x : 'a], once the typedef is renamed, you throw the
        type env to take the one you had before
  - false: give the computed environment
*)
let f_ty_make_ext func ~empty_type_env ~give_original all_env hierar v =
  let f = all_env.f in
  let new_f =
    {f with
       data = [];
       ftypevars = if empty_type_env then StringMap.empty else f.ftypevars
    } in
  let new_all_env = {all_env with f = new_f} in
  let f_env,result = func new_all_env hierar v in
  let f_env =
    { f_env with
        data = f.data;
        ftypevars = if give_original then all_env.f.ftypevars else f_env.ftypevars
    } in
    {new_all_env with f = f_env}, result

(**
    The entry points for renaming types
    You can't directly call the previous functions because the types of the
    environments are not compatible: the 'a of 'a folding_env is bool here,
    StringSet.t in patterns, unit in expressions

*)
let f_ty_ext ~empty_type_env =
  f_ty_make_ext f_ty ~empty_type_env
let f_arrow_t_ext ~empty_type_env =
  f_ty_make_ext f_arrow_t ~empty_type_env
let f_flatvars_ext label ~empty_type_env =
  f_ty_make_ext (f_list_aux (f_flatvar label)) ~empty_type_env



(*------------------------------*)
(*---- renaming for typedefs ---*)
(*------------------------------*)
(*
   Taken care of specially, because, contrary to other cases, in typedefs
   you can (and need to) bind your type variables explicitely
*)
let f_global_typeident all_env hierar (Typeident name) label =
  add_global_type name hierar all_env label
let f_typedef all_env hierar (tyvl,ty) =
  let original_data = all_env.f.data in
  let original_ftypevars = all_env.f.ftypevars in
  let f_env, tyvl =
    (* renaming the vars in an empty environment
     * (because when saying type t('a), 'a can never be shared, it is a definition) *)
    f_flatvars
      (Parser_utils.label ty)
      {all_env with f = {all_env.f with ftypevars = StringMap.empty; data = []}}
      hierar
      tyvl in
  (* putting the vars of the typedef in the 'data' field, that is the field containing
   * the variable that are shared in the whole type *)
  let all_env = {all_env with f = {f_env with data = List.map (fun (Flatvar v) -> v) tyvl}} in
  let f_env, ty = f_ty all_env hierar ty in
  (* discarding everything from the renaming of the ty but the global_env *)
  let all_env = {all_env with f = {f_env with data = original_data; ftypevars = original_ftypevars}} in
  all_env, (tyvl, ty)

let f_typedefs all_env hierar typedefs =
  check_unicity
    ~case: "a type definition"
    ~compare_by:
      (fun ({ SurfaceAst.ty_def_name = Typeident i ; _ }, label) -> (i, label))
    typedefs ;
  let all_env, l =
    List.fold_left
      (fun (all_env, l)
         ({
            SurfaceAst.ty_def_options = options ;
            SurfaceAst.ty_def_visibility = visibility ;
            SurfaceAst.ty_def_name = Typeident name ;
            SurfaceAst.ty_def_params = c ;
            SurfaceAst.ty_def_body = d ;
          }, e) ->
           let (all_env, ident) = add_type name hierar all_env e in
         all_env, (options, visibility, ident, c, d, e) :: l)
      (all_env,[]) typedefs in
  (* The list of types is reversed once, but it doesn't matter since there is
     no ambiguity (no types occurs several times in the list). *)
  List.fold_left
    (fun (all_env, l) (options, visibility, ident, tyvl, ty, e) ->
       let (all_env, (tyvl, ty)) = f_typedef all_env hierar (tyvl, ty) in
       all_env, (
         {
           SurfaceAst.ty_def_options = options ;
           SurfaceAst.ty_def_visibility = visibility ;
           SurfaceAst.ty_def_name = Typeident ident ;
           SurfaceAst.ty_def_params = tyvl ;
           SurfaceAst.ty_def_body = ty ;
         }, e) :: l)
    (all_env, []) l


(*------------------------------*)
(*---- renaming for patterns ---*)
(*------------------------------*)
(*
   Gives back the environment with bindings from the patterns
   and renames everything
   The field 'data' of all_env.f.data contains the pattern environment, that
   is used to check the linearity of the pattern, ie that no variable
   appears twice in a pattern
 *)
let f_list_aux f all_env hierar l =
  List.fold_right
    (fun x (all_env, l) ->
       let all_env, x = f all_env hierar x in
       all_env, x :: l
    ) l (all_env, [])

let rec f_pat all_env hierar (pat_node, label) =
  let all_env, pat_node = f_pat_node label all_env hierar pat_node in
  all_env, (pat_node, label)
and f_pat_node label all_env hierar = function
  | PatAny
  | PatConst _ as p -> all_env, p
  | PatRecord (p, rowvar) ->
      let all_env, p = f_pat_record_node all_env hierar p in
      all_env, PatRecord (p, rowvar)
  | PatVar name ->
      let all_env, name = f_pat_var label all_env hierar name in
      all_env, PatVar name
  | PatCoerce (p, ty) ->
      let all_env, p = f_pat all_env hierar p in
      let all_env, ty = f_ty_ext ~empty_type_env:false ~give_original:false all_env hierar ty in
      all_env, PatCoerce (p, ty)
  | PatAs (p, name) ->
      let all_env, p = f_pat all_env hierar p in
      let all_env, name = f_pat_var label all_env hierar name in
      all_env, PatAs (p, name)

and f_pat_record_node all_env hierar l =
  f_list_aux f_pat_record_node_elt all_env hierar l
and f_pat_record_node_elt all_env hierar (s, p) =
  let all_env, p = f_pat all_env hierar p in
  all_env, (s,p)
and f_pat_var ?no_warning label all_env hierar name =
  add_pat_var ?no_warning name hierar all_env label

let f_pat_make_ext ?(exported=true) ?(pat_env=empty_pat_env) func all_env hierar v =
  let new_all_env = {all_env with f = {all_env.f with data = (exported,pat_env)}} in
  let new_all_env, v = func new_all_env hierar v in
  let _exported,pat_env = new_all_env.f.data in
  let new_all_env = {new_all_env with f = {new_all_env.f with data = ()}} in
  new_all_env, pat_env, v

(**
   Entry point for renaming patterns
   See the remarks in the equivalent comments for types
*)
let f_pat_ext_with_pat_env ?exported ?pat_env x =
  f_pat_make_ext ?exported ?pat_env f_pat x
let f_pat_ext x =
  f_pat_make_ext f_pat x
let f_pat_record_node_ext x =
  f_pat_make_ext f_pat_record_node x
let f_pat_var_ext ?no_warning label x =
  f_pat_make_ext (f_pat_var ?no_warning label) x


(*------------------------------*)
(*------ dealing with opens ----*)
(*------------------------------*)
let find_opt_global name all_env =
  match StringMap.find_opt name all_env.t.tnames with
    | Some ((v,_) :: _) -> Some v
    | Some [] | None -> None

let find_opt_local_or_global name all_env =
  match StringMap.find_opt name all_env.l.lnames with
    | None -> find_opt_global name all_env
    | v -> v

let path_expr_to_module_aux p = function
  | Some (OpenedIdent (tree, ident, path)) ->
      (match Tree.get_path_opt tree p with
           (* the path is not in the tree, which means a 'dot' access
            * on something which is not a static record *)
         | None -> Error "the path you gave does not exist"
             (* the path is in the tree, but not a module/record *)
         | Some t when Tree.is_leaf t -> Error "the path does not point to a module"
             (* the path is good: it is in the tree and points to an actual module *)
         | Some t -> Result (t, ident, path @ p)
      )
  | Some (Normal _) ->
      Error "the identifier is not a module"
  | None -> Error "the identifier is unbound"

(**
   if [e] has the form [ident.field1.field2...]
   gives you back the subtree pointed to by [ident.field1.field2...], if any
 *)
let path_expr_to_module e all_env =
  (* app_to_dot removes any coerce that may be in the way *)
  let e, p = Dot.app_to_dot_for_renaming e in
  match fst e with
    | Ident s ->
        path_expr_to_module_aux p (find_opt_local_or_global s all_env)
    | Dot ((Directive (`toplevel,[],[]),_),s) ->
        path_expr_to_module_aux p (find_opt_global s all_env)
    | Dot _-> assert false
    | _ -> Error "you are trying to open something that is not a path"


(**
   gives you the fields available when opening the (path to a) module [e]
   prints an error if it is not a module

   if the module is coerced with an explicit type record (no typename)
   only the fields present in the coercion are opened
   if we coerce a submodule of the module, then we are not preventing them
   from being fully opened later on. For example, this is an error:

     M1 = \{\{ M2 = \{\{ x = 2 y = 3 \}\} z = 4 \}\}
     open M1 : \{ z = 4 \}
     M2

   But this is correct:

     M1 = \{\{ M2 = \{\{ x = 2 y = 3 \}\} z = 4 \}\}
     open M1 : \{ M2 : \{ x = 2 \} \}
     open M2
     y

 *)
let path_expr_to_fields e all_env =
  match path_expr_to_module e all_env with
    | Error _diag ->
        (*Format.printf "%s\n%!" _diag;*)
        invalid_open (Parser_utils.label e)
    | Result (t, ident, path) ->
        let children = Tree.children t in
        let fields = List.map Tree.value children in
        let filter =
          (* the filter is used to implement coerced open *)
          ( match fst e with
              | Directive (`coerce, [e], [ty]) ->
                  (* if we have a coerce *)
                    ( match fst ty with
                        | TypeRecord r ->
                            (* then we must have a record, and we open only
                             * the fields defined in the record *)
                            let fields = TypeRecord.field_names r in
                              List.filter (fun x -> List.mem x fields)
                        | _ ->
                            invalid_coerced_open (Parser_utils.label e)
                    )
              (* if we do not have a coerce at the toplevel,
               * then we open every field *)
              | _ -> fun x -> x
          ) in
        let opened_field = filter fields in
        let opened_trees =
          List.filter (fun t -> List.mem (Tree.value t) opened_field) children
        in
          opened_trees, ident, path

(* env2 overwrites env1 if they have common keys *)
let merge_env env1 env2 =
  {env1 with lnames = StringMap.fold StringMap.add env2 env1.lnames}


(*------------------------------*)
(*-- renaming for expressions --*)
(*------------------------------*)
let f_list_aux f all_env hierar l =
  List.fold_right
    (fun x (f_env, l) ->
       let f_env, x = f {all_env with f = f_env} hierar x in
       f_env, x :: l
    ) l (all_env.f, [])

let rec f_expr all_env hierar (e, label) =
  let f_env, e = f_expr_node all_env hierar label e in
  f_env, (e, label)

and f_expr_node all_env hierar label : (string, renaming_directive) expr_node -> _ * (Ident.t, dependency_directive) expr_node = function
  | (Const i : (string,renaming_directive) expr_node) -> all_env.f, Const i
  | Bypass b -> all_env.f, Bypass b
  | Ident name -> f_ident all_env hierar name label
  | Dot ((Directive (`toplevel,[],[]),_),s) ->
      get_var_from_toplevel s all_env label
  | Dot (e, s) ->
      let f_env, e = f_expr all_env hierar e in
      f_env, Dot (e,s)
  | Apply (e, record) ->
      let f_env, e = f_expr all_env hierar e in
      let f_env, record = f_record ~kind:`fake {all_env with f=f_env} hierar record in
      f_env, Apply (e, record)
  | Lambda (p, e) ->
      let all_env, _pat_env, p = f_pat_record_node_ext all_env hierar p in
      let f_env, e = f_expr all_env hierar e in
      f_env, Lambda (p, e)
  | LetIn (rec_,iel,e) ->
      let all_env, iel = f_bindings ~rec_ all_env hierar iel in
      let f_env, e = f_expr all_env hierar e in
      f_env, LetIn (rec_, iel, e)
  | Match (e, pel) ->
      let f_env, e = f_expr all_env ("MATCH" +> hierar) e in
      let f_env, pel = f_pel {all_env with f = f_env} hierar pel in
      f_env, Match (e, pel)
  | Directive (`module_, [(Record r, b)], []) ->
      let f_env, (r: (Ident.t,_) record_node) = f_record_node ~kind:`module_ all_env hierar r in
      (f_env, Directive (`module_, [(Record r, b)], []))
  | Directive (`module_, _, _) -> assert false
  | Record r ->
      let f_env, r = f_record_node ~kind:`record all_env hierar r in
      f_env, Record r
  | ExtendRecord (r, e) ->
      let f_env, r = f_record_node ~kind:`record all_env hierar r in
      let f_env, e = f_expr {all_env with f = f_env} hierar e in
      f_env, ExtendRecord (r, e)
  | DBPath (dbelt, d) ->
      let f_env, dbelt = f_dbelt all_env hierar dbelt in
      f_env, DBPath (dbelt, d)
  | Directive (`open_, l, _) ->
      ( match l with
          | [e1;e2] ->
              let trees, ident, path = path_expr_to_fields e1 all_env in
              let all_env =
                List.fold_left
                  (fun all_env (Tree.Tree (name,_) as tree) ->
                     add_opened_var name tree ident path all_env
                  ) all_env trees in
              let f_env, e2 = f_expr all_env ("OPEN" +> hierar) e2 in
              f_env, fst e2
          | _ -> failwith "SurfaceAstRenaming: f_expr_node: malformed open directive"
      )
  | Directive (`toplevel_open, _, _) ->
      assert false
  | Directive (`toplevel,_,_) ->
      error label "the only valid use of @{<bright>@@toplevel@} is: @@toplevel.identifier."
  | Directive (((#basic_directive | #access_directive) as v,_,_) as d) ->
      let f_env, d = f_directive all_env (if v = `coerce then hierar else "DIR" +> hierar) d in
      f_env, Directive d

and update_all_env_with str ident e all_env =
  match tree_option_of_expr str e with
    | None -> all_env
    | Some t ->
        with_lnames all_env
          (StringMap.add str (OpenedIdent (t,ident,[])) all_env.l.lnames)

and f_bindings ~rec_ all_env hierar iel =
  check_unicity ~case:"a set of recursive bindings" ~compare_by:(fun (i,e) -> (i,Parser_utils.label e)) iel;
  (* creating the new names for every identifier and binding them into a new
   * environment *)
  (* all_env is what is seen by functions and by the [e] of [let ... in e] *)
  let old_all_env = all_env in
  let new_all_env, iel =
    List.fold_left_map
      (fun all_env (i,e) ->
        let all_env, _pat_env, {SurfaceAst.ident=ident ; directives=_} =
          f_pat_var_ext (Parser_utils.label e) all_env hierar {SurfaceAst.ident=i;directives=[]} 
        in
        (update_all_env_with i ident e all_env, (ident,e))
      ) all_env iel
  in
  let all_env = if rec_ then new_all_env else old_all_env in
  let (f_env, iel) =
    List.fold_left_map
      (fun f_env (i_new,e) ->
         let f_env, e_new = f_expr {all_env with f = f_env} hierar e in
         (f_env, (i_new, e_new))
      ) new_all_env.f iel in
  {new_all_env with f = f_env}, iel

(**
    Variants should not contain any kind of variables, because they won't be
    renamed (or you need to add a special case here)
*)
and f_directive all_env hierar (variant, el, tl) =
  let f_env, el = f_expr_list all_env hierar el in
  let f_env, tl =
    let not_coerce = variant != `coerce in
    let fold_map f_env ty =
      let all_env, ty =
        f_ty_ext
          (* here making a special case for coerce because, when we have a coercion, the scope of its typevars is the current one
           * for all the other directives, like @wrap : 'a -> private('a), we always want its 'a to be fresh *)
          ~empty_type_env:not_coerce ~give_original:not_coerce
          {all_env with f = f_env}
          hierar ty
      in
      all_env.f, ty
    in
    List.fold_left_map fold_map f_env tl
  in
  f_env, (variant, el, tl)

and f_pel all_env hierar pel =
  f_list_aux f_patternexpr all_env hierar pel

and f_patternexpr all_env hierar (p,e) =
  let all_env, _pat_env, p = f_pat_ext all_env hierar p in
  let f_env, e = f_expr all_env hierar e in
  f_env, (p, e)

and f_dbelt all_env hierar (dbelt_node, label) =
  let f_env, dbelt_node = f_dbelt_node all_env hierar dbelt_node in
  f_env, (dbelt_node, label)

and f_dbelt_node all_env hierar l =
  f_list_aux f_preprocessed_db_element all_env hierar l

and f_preprocessed_db_element all_env hierar (e, label) =
  let f_env, e = f_preprocessed_db_element_node all_env hierar e in
  f_env, (e, label)

and f_preprocessed_db_element_node all_env hierar = function
  | NewKey
  | FldKey _ as v -> all_env.f, v
  | ExprKey e ->
      let f_env, e = f_expr all_env hierar e in
      f_env, ExprKey e

and f_expr_list all_env hierar el =
  f_list_aux f_expr all_env hierar el

(**
   The actual_record flag changes the scopes of variables and type variables
*)
and f_record ~kind all_env hierar (record_node, label) =
  let f_env, record_node = f_record_node ~kind all_env hierar record_node in
  f_env, (record_node, label)

and f_record_node ~kind all_env hierar record_node =
  match kind with
    | `fake | `record ->
        f_list_aux f_stringexpr all_env hierar record_node
    | `module_ ->
      (* type variables are limited to field declarations, or toplevel declarations
       * whichever comes first
       * so, when reaching a field declarations, we discard the current type
       * environment *)
        let make_local (i,e) =
          (Directive ((`local i), [e], []), Parser_utils.nlabel e) in
        let field_names = Record.field_names record_node in
        let all_env, iel =
          f_module all_env hierar record_node in
        all_env.f, List.combine field_names (List.map make_local iel)

and f_module all_env hierar iel =
  let dump_type_env_mods f_env = {f_env with ftypevars = all_env.f.ftypevars} in
  let module_name = hierar |> snd |> List.last in
  check_unicity ~case:(Printf.sprintf "the module `%s`" module_name) ~compare_by:(fun (i,e) -> (i,Parser_utils.label e)) iel;
  (* creating the new names for every identifier and binding them into a new
   * environment *)
  (* all_env is what is seen by functions and by the [e] of [let ... in e] *)
  let all_env =
    List.fold_left
      (fun all_env (i,e) ->
         let all_env, _pat_env, {SurfaceAst.ident=ident ; directives=_} =
           f_pat_var_ext ~no_warning:true (Parser_utils.label e) all_env hierar {SurfaceAst.ident=i; directives=[]} in
         update_all_env_with i ident e all_env
      ) all_env iel in
  let (f_env, rev_iel) =
    List.fold_left
      (fun (f_env,l) (i,e) ->
         let f_env, e_new = f_expr {all_env with f = f_env} (("SUB"^i) +> hierar) e in
         let i_new =
           (* ok to lookup in the environment where all variable in the current
            * bindings appear since names are unique in a set of bindings *)
           match StringMap.find i all_env.l.lnames with
             | Normal i' -> i'
             | OpenedIdent (_,i,l) -> assert (l = []); i in
         let new_f_env = dump_type_env_mods f_env in
         let new_l = (i_new, e_new) :: l in
         (new_f_env, new_l)
      ) (dump_type_env_mods all_env.f, []) iel in
  let iel = List.rev rev_iel in
  {all_env with f = dump_type_env_mods f_env}, iel


and f_stringexpr all_env hierar (s,e) =
  (* we are treating a fake record, used in Apply or Lambda, so we keep the same
   * type environment *)
  let f_env, e = f_expr all_env hierar e in
  f_env, (s,e)

and f_ident all_env _hierar name label =
  get_var name all_env label

and f_expr_ext envs hierar e =
  f_expr envs hierar e


(*---------------------------*)
(*-- renaming declarations --*)
(*---------------------------*)
(*
  This is done once at the beginning (see the very first comment)
  The toplevel environment maps toplevel source names to list of unique names
  (because we want to keep some kind of reasonable scope even if we redefine
  a value several times at toplevel)
  In [p = e] at toplevel, e can see the variables of the toplevel environment
  If there are several toplevel definitions for a given name [n], the following
  rules apply:
  - if p is [n] and e is a lambda or a module, then p hides other names
  - or else, the latest definition that appears before the current binding is chosen
*)

type 'b common_code_elt_node =
  [ `NewDbDef of ((string, 'b) expr, string ty) QmlAst.Db.db_def
  | `NewType of (
      QmlAst.ty_def_options
      * type_def_visibility
      * Ident.t typeident
      * string typevar list
      * string ty
      * QmlLoc.annot
    ) list ]

(* the toplevel declaration after toplevel renaming and before renaming in the rhs *)
type 'b tmp_code_elt_node =
  [ 'b common_code_elt_node
  | `Database of Ident.t * string list * QmlAst.Db.options list
  | `NewVal of (Ident.t StringMap.t * Ident.t pat * (string, 'b) expr) list * bool ]
type 'b tmp_code_elt = 'b tmp_code_elt_node QmlLoc.label
type 'b tmp_code = 'b tmp_code_elt list

(* the toplevel declaration after toplevel renaming of types only *)
type 'b tmp_code_elt_node_only_types =
  [ 'b common_code_elt_node
  | `Database of string * string list * QmlAst.Db.options list
  | `NewVal of (string pat * (string, 'b) expr) list * bool ]
type 'b tmp_code_elt_only_types = 'b tmp_code_elt_node_only_types QmlLoc.label
type 'b tmp_code_only_types = 'b tmp_code_elt_only_types list

let toplevel_code_elt_node_pe_map =
  let is_exported e =
    let module D = SurfaceAstDecons in
    not (
      D.Look.at
        ~through:[ D.Remove.Basic.expand;
                   D.Remove.Basic.opacapi;
                   D.Remove.Basic.opavalue_directive;
                   D.Remove.Basic.slicer_directive;
                 ]
        ~at:[D.Remove.Basic.access_not_public] e
    ) in
  fun (all_env,pat_env_option) (p,e) ->
  let local_env = all_env.l in
  let all_env, pat_env, pat = f_pat_ext_with_pat_env ~exported:(is_exported e) ?pat_env:pat_env_option all_env empty_hierar p in
  let map = all_env.t.tnames in
  let tnames =
    match pat, p with
      | (PatVar ident, label), (PatVar s, _) ->
          (* here, we append at the end of the list
           * but it isn't a real problem since the length of the list
           * if the number of times a toplevel identifier is redefined
           * (which is just one most of the time, maybe 2 or 3 in some cases)
           *)
          (* we don't need to touch the global environment
           * because the call to f_pat_ext already does that
           * so we can just ignore what it changed in the local scope
           * to put it in the toplevel one
           *)
          let s = s.SurfaceAst.ident in
          let ident = ident.SurfaceAst.ident in
          let old_l = Option.default [] (StringMap.find_opt s map) in
            ( match tree_option_of_expr s e with
                | None -> StringMap.add s (old_l @ [(Normal ident,label.QmlLoc.pos)]) map
                | Some t -> StringMap.add s (old_l @ [(OpenedIdent (t,ident,[]),label.QmlLoc.pos)]) map
            )
      | (_,label), _ ->
          StringSet.fold
            (fun s map ->
               let ident = StringMap.find s all_env.l.lnames in
               let old_l = Option.default [] (StringMap.find_opt s map) in
                 StringMap.add s (old_l @ [(ident,label.QmlLoc.pos)]) map
            ) pat_env map
  in
    ({all_env with l = local_env;
                   t = {all_env.t with tnames = tnames}}, Some pat_env),
    (all_env.f.ftypevars, pat, e)

(*------------------------------*)
(*--- toplevel type renaming ---*)
(*------------------------------*)
let toplevel_code_elt_node_map_for_types envs : _ -> (_ * _ tmp_code_elt_node_only_types) = function
  | Database (v1,v2,v3) -> envs, `Database (v1,v2,v3)
  | NewDbDef v1 -> envs, `NewDbDef v1
  | NewType tds ->
      let (envs, tds) =
        List.fold_left_map
          (fun envs (ty_def, label) ->
             let (new_envs, new_name) =
               f_global_typeident
                 envs empty_hierar ty_def.SurfaceAst.ty_def_name label in
             (new_envs,
              (
                ty_def.SurfaceAst.ty_def_options,
                ty_def.SurfaceAst.ty_def_visibility,
                Typeident new_name,
                ty_def.SurfaceAst.ty_def_params,
                ty_def.SurfaceAst.ty_def_body,
                label
              )))
          envs tds in
      (envs, (`NewType tds))
  | NewVal (pel,rec_) -> envs, `NewVal (pel,rec_)
  | Package _ -> assert false

let toplevel_code_elt_map_for_types all_env (c,label) =
  let acc, c = toplevel_code_elt_node_map_for_types all_env c in
  acc, (c,label)

let toplevel_code_map_for_types all_env c : (_ * _ tmp_code_only_types) =
  (* even though we use fold_left, the elements inside the list all_env.t.tnames
   * appear in the order of their definition
   * (because instead of consing new element, we append them) *)
  List.fold_left_map toplevel_code_elt_map_for_types all_env c

(*-------------------------------*)
(*-- toplevel pattern renaming --*)
(*-------------------------------*)
let toplevel_code_elt_node_map_for_patterns envs label : _ -> (_ * _ tmp_code_elt_node) = function
  | `NewVal (pel,rec_) ->
      let (new_envs,_pat_env), new_l = List.fold_left_map toplevel_code_elt_node_pe_map (envs,None) pel in
      new_envs, `NewVal (new_l,rec_)
  | `Database (name, l, props) ->
      let local_env = envs.l in
      let (envs, _pat_env, {SurfaceAst.ident=ident;directives=_}) = f_pat_var_ext label envs (fake_hierar "DB") {SurfaceAst.ident=name; directives=[]} in
      let tnames = envs.t.tnames in
      let names = Option.default [] (StringMap.find_opt name tnames) in
      let names = names @ [(Normal ident,label.QmlLoc.pos)] in
      let tnames = StringMap.add name names tnames in
      {envs with l = local_env; t = {envs.t with tnames = tnames}}, `Database (ident, l, props)
  | #common_code_elt_node as v ->
      envs, v
let toplevel_code_elt_map_for_patterns all_env (c,label) =
  let acc, c = toplevel_code_elt_node_map_for_patterns all_env label c in
  acc, (c, label)
let toplevel_code_map_for_patterns all_env c : (_ * _ tmp_code) =
  List.fold_left_map toplevel_code_elt_map_for_patterns all_env c

(*-------------------------------*)
(*-- toplevel shallow renaming --*)
(*-------------------------------*)
(* When renaming the toplevel, we go through it once for grabbing all the types
 * and a second time for renaming all the patterns (and since there may be coercions,
 * we need all the types to do that *)
let toplevel_code_map all_env c =
  let all_env, c = toplevel_code_map_for_types all_env c in
  toplevel_code_map_for_patterns all_env c

(*---------------------------------*)
(*-------- toplevel scope ---------*)
(*---------------------------------*)
(*
   used to deal with scopes at toplevel
   eg: [a = 2
        a = 3
        b = a]
   [b] refers to the second [a], because when we went over the definition
   of the second [a], we removed any previous defined of [a] from the environment
 *)
let drop_until v l =
  let rec aux acc = function
      | [] -> List.rev acc
      | ((Normal h, _) :: _) as l when v = h -> l
      | ((OpenedIdent (_, h, []), _) :: _) as l when v = h -> l
      | h :: t -> aux (h :: acc) t
  in
    aux [] l
let drop_if v = function
  | [] ->
      failwith "SurfaceAstRenaming: drop_if: malformed toplevel environment "
  (* if we are the first one on the list and we are not recursive
   * it means that we are in a situation like:
   * --- beginning of the file
   * (a,_) = e <--
   * a = 2
   * ---
   * we are the first one to define a, but we can't access a
   * in e, so we remove it from the map
   *)
  | (Normal h, _) :: _ when v = h -> []
  | (OpenedIdent (_, h, []), _) :: _ when v = h -> []
  | l -> l
let change_toplevel_scope dropper s a toplevel =
  let l = StringMap.find s toplevel in
  let new_l = dropper a l in
    if new_l = [] then
      StringMap.remove s toplevel
    else
      StringMap.add s new_l toplevel

(* gives an assoc list (original_name -> ident) of the vars of the pattern *)
let make_assoc_of_pattern pattern =
  List.map
    (fun ident ->
       let s = Ident.original_name ident in
       (s,ident))
    (SurfaceAstTraversal.Pattern.get_var_list_pattern pattern)

let update_toplevel_names2 include_idents all_env idents =
  let dropper = if include_idents then drop_until else drop_if in
  List.fold_left
    (fun toplevel_names (s,a) ->
       change_toplevel_scope dropper s a toplevel_names)
    all_env.t.tnames idents
let update_toplevel_names1 ~rec_ all_env patterns =
  (* there shouldn't be severals binding for the same name, or it means we
   * messed up the error [rec a = 1 and a = 2] *)
  let idents = List.concat_map make_assoc_of_pattern patterns in
  let toplevel_before = update_toplevel_names2 rec_ all_env idents in
  let toplevel_after = if rec_ then toplevel_before else update_toplevel_names2 true all_env idents in
  let all_env_before = (with_tnames all_env toplevel_before) in
  let make_all_env_after = (fun env -> with_tnames env toplevel_after) in
  all_env_before,make_all_env_after,rec_

let update_toplevel_names all_env pel rec_ =
  let rec_ =
    rec_ ||
      (match pel with
       | [] | _ :: _ :: _ -> assert false (* not syntactically valid *)
       | [pattern,expr] ->
           match pattern with
           | (PatVar _, _) when Rec.recursive_scope_before_renaming expr -> true
           | _ -> false) in
  update_toplevel_names1 ~rec_ all_env (List.map fst pel)

(*-----------------------------*)
(*----- renaming newvals ------*)
(*-----------------------------*)
let rec f_code_elt_node all_env : _ -> _ * (_,_) code_elt_node list = function
  | `NewVal (envpel,rec_) ->
      let pel = List.map (fun (_env,p,e) -> (p,e)) envpel in
      let all_env,make_all_env_after,rec_ = update_toplevel_names all_env pel rec_ in
      let f_env, pel =
        List.fold_left_map
          (fun f_env (type_env,p,e) ->
             let vars = SurfaceAstTraversal.Pattern.get_vars_ident p in
             let str = IdentSet.fold (fun s acc -> acc ^ (Ident.original_name s)) vars "" in
             let all_env = with_type_env {all_env with f = f_env} type_env in
             let f_env, e = f_expr_ext all_env (fake_hierar str) e in
             f_env, (p,e)
          ) all_env.f envpel in
      let all_env_after = make_all_env_after {all_env with f = f_env} in
      all_env_after, [NewVal (pel,rec_)]
  | `NewType tds ->
      let all_env,tds =
        List.fold_left_map
          (fun all_env (options, visibility, typeident, tyvl, ty, label) ->
             let (all_env, (tyvl, ty)) =
               f_typedef all_env (fake_hierar "TYPE") (tyvl, ty) in
             let ty_def = {
               SurfaceAst.ty_def_options = options ;
               SurfaceAst.ty_def_visibility = visibility ;
               SurfaceAst.ty_def_name = typeident ;
               SurfaceAst.ty_def_params = tyvl ;
               SurfaceAst.ty_def_body = ty } in
             all_env, (ty_def, label))
          all_env tds in
      (all_env, [NewType tds])
  | `Database (a,b,c) ->
      all_env, [Database (a,b,c)]
  | `NewDbDef x ->
      let all_env, x = QmlAst.Db.foldmap_expr
        (fun env e ->
           let f_env, x = f_expr_ext env (fake_hierar "DBD") e in
           {env with f = f_env}, x)
        all_env x in
      let all_env, x = QmlAst.Db.foldmap_ty
        (fun env ty ->
           f_ty_ext ~empty_type_env:true ~give_original:false env (fake_hierar "DBV") ty)
        all_env x in
      all_env, [NewDbDef x]

let f_code_elt envs (code_elt_node,label) : _ * (_, _) code_elt list =
  let acc, code_elt_node = f_code_elt_node envs code_elt_node in
  acc, List.map (fun x -> x, Parser_utils.copy_label label) code_elt_node

(*----------------------------*)
(*---- Interface to stdlib ---*)
(*----------------------------*)
let stringmap_safe_merge acc names =
  (* should be really a safe_merge, not a merge that checks that the values
   * are the same when the keys are the same but the renaming is called once for
   * user code and once for stdlib code in s2 so that doesn't work *)
  try StringMap.merge
    (fun i j ->
       if not (Ident.equal i j) then raise Exit;
       i) acc names
  with Exit ->
    let intersection = StringMap.filter_keys (fun k -> StringMap.mem k acc) names in
    Printf.printf "intersection:%s\nacc:%s\nnames:%s\n%!"
      (DebugPrint.print (StringMap.keys intersection))
      (DebugPrint.print (StringMap.keys acc))
      (DebugPrint.print (StringMap.keys names));
    assert false

(* Sets a reference that is used in the compiler to create renamed identifiers
 * from their source name *)
let merge_maptoident_val infos acc toplevel_names =
  let names =
    stringmap_filter_map
      (function
       | [] -> assert false
       | (Normal i,_) :: _
       | (OpenedIdent (_, i, []),_) :: _ when is_exported i infos -> Some i
           (* it means that some value is first defined at toplevel by an open statement
            * it is possible but then the compiler won't access it
            * (everything should be defined as an alias with @opacapi
            * so no open *)
       | _  -> None) toplevel_names in
  stringmap_safe_merge acc names
let merge_maptoident_typ infos acc toplevel_types =
  let types =
    stringmap_filter_map
      (function
       | [(i,_)] when is_exported i infos -> Some i
       | [_] -> None
       | _ -> assert false) toplevel_types in
  stringmap_safe_merge acc (stringmap_safe_merge (get_tuple_string_map ()) types)

(*------------------------------*)
(*------- main function --------*)
(*------------------------------*)
(* TODO all error should be printed (or reprinted or only the first error),
   at the end of the compiler output to provide better readability *)
let f_code env code =
  (* renaming the whole toplevel names *)
  let envs = env.all_envs in
  let full_env, code = toplevel_code_map envs code in
  let envs, code = List.fold_left_map f_code_elt full_env code in
 (* to stop if unbound idents exists,
    help reading compiler output, since zillion of warning hide
    true error message *)
  OManager.flush_errors ();
  (* checks on the code use the toplevel environment with all bindings *)
  check_for_unused_names envs.f.fglobal full_env.t.tnames;
  check_for_toplevel_duplicates full_env.t.tnames full_env.t.ttypes;
  (* errors reporting *)
  print_duplicatesL0 ();
  OManager.flush_errors ();
  (* == *)
  let maptoident_val = merge_maptoident_val full_env.f.fglobal env.maptoident_val full_env.t.tnames in
  let maptoident_typ = merge_maptoident_typ full_env.f.fglobal env.maptoident_typ full_env.t.ttypes in
  (* in non separated mode, the environment given back only contains the bindings
     still in scope (can't remember why, perhaps related to toplevel open?)
     in separated mode, this function is called only once, so we can give the full env
     that will only be used to save the right bindings for separate compilation
  *)
  let envs = if ObjectFiles.Arg.is_separated () then full_env else envs in
  let env = {
    all_envs = envs;
    maptoident_val = maptoident_val;
    maptoident_typ = maptoident_typ;
  } in
  env, List.flatten code

let code = f_code


(*----------------------------------*)
(*------ separate compilation ------*)
(*----------------------------------*)
module ObjectExpr =
  ObjectFiles.Make(struct
                     type t = (Ident.t * FilePos.pos) StringMap.t
                     let pass = "renaming_exp"
                     let pp f _ = Format.pp_print_string f "<dummy>"
                   end)
module ObjectType =
  ObjectFiles.Make(struct
                     type t = (Ident.t * FilePos.pos) StringMap.t
                     let pass = "renaming_type"
                     let pp f _ = Format.pp_print_string f "<dummy>"
                   end)
module ObjectOpaMapToIdent =
  ObjectFiles.Make(struct
                     type t = Ident.t StringMap.t * Ident.t StringMap.t
                     let pass = "renaming_opamaptoident"
                     let pp f _ = Format.pp_print_string f "<dummy>"
                   end)

let extract_types_in_scope_except_tuple env =
  let all_env = env.all_envs in
  stringmap_filter_map
    (function
     | [(ident,pos)] ->
         if is_exported ident all_env.f.fglobal then
           Some (ident,pos)
         else
           None
     | _ -> assert false) all_env.t.ttypes

let extract_types_in_scope env =
  let map = extract_types_in_scope_except_tuple env in
  StringMap.safe_merge (StringMap.map (fun x -> (x,FilePos.nopos "builtin types")) (get_tuple_string_map ())) map

let save_env env =
  let all_env = env.all_envs in
  let map =
    stringmap_filter_map (exported_var all_env.f.fglobal) all_env.t.tnames in
  ObjectExpr.save map;
  let map = extract_types_in_scope_except_tuple env in
  ObjectType.save map;
  let my_val,my_typ =
    if ObjectFiles.stdlib_packages (ObjectFiles.get_current_package ()) then
      env.maptoident_val, env.maptoident_typ
    else
      (* Need something better here *)
      (* only saving the local maptoident for the stdlib packages *)
      env.maptoident_val, env.maptoident_typ
  in
  ObjectOpaMapToIdent.save (my_val,my_typ);
  let val_,typ =
    ObjectOpaMapToIdent.fold
      (fun (val1,typ1) (val2,typ2) ->
         (StringMap.safe_merge val1 val2, stringmap_safe_merge typ1 typ2))
      (my_val,my_typ) in
  OpaMapToIdent.set_val_map val_;
  OpaMapToIdent.set_typ_map typ

let load_expr_env all_env map =
  let tnames =
    StringMap.fold
      (fun k (v,pos) tenv ->
         match StringMap.find_opt k tenv with
         | None -> StringMap.add k [(Normal v,pos)] tenv
         | Some l -> StringMap.add k (l @ [(Normal v,pos)]) tenv
      ) map all_env.t.tnames in
  {all_env with t = {all_env.t with tnames = tnames}}

let load_type_env all_env map =
  let ttypes =
    StringMap.fold
      (fun k (v,pos) tenv -> push_in_front k (v,pos) tenv)
      map all_env.t.ttypes in
  {all_env with t = {all_env.t with ttypes = ttypes}}

let load_env env =
  let all_env = env.all_envs in
  let all_env = ObjectExpr.fold load_expr_env all_env in
  let all_env = ObjectType.fold load_type_env all_env in
  {env with all_envs = all_env}


let get_exported_values env =
  StringMap.fold
    (fun _ renamed_as accu -> IdentSet.add renamed_as accu)
    env.maptoident_val
    IdentSet.empty
