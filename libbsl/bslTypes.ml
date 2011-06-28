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
(* SEE THE DOCUMENTATION IN MLI FILE *)

(* dependencies *)
module List = Base.List
module String = Base.String

let (@*) = InfixOperator.(@*)

(* refactoring in progress *)

(* typevars *)
module TypeVar      = QmlTypeVars.TypeVar
module TypeVarPrint = QmlTypeVars.TypeVarPrint
module TypeVarSet   = QmlTypeVars.TypeVarSet
module TypeVarMap   = QmlTypeVars.TypeVarMap

let (~@) i = QmlTypeVars.get_canonical_typevar i

let debug fmt =
  OManager.printf ("@{<cyan>[BslTypes]@}@ @[<2>"^^fmt^^"@]@.")

(* exported in this module for simplicity, do not hack libbsl please *)
let private_typevar_name_table = Hashtbl.create 10
let private_typevar_counter = ref 0
let (~$) name =
  let index =
    try
      Hashtbl.find private_typevar_name_table name
    with
    | Not_found ->
        let index = !private_typevar_counter in
        incr(private_typevar_counter);
        Hashtbl.add private_typevar_name_table name index;
        index
  in
  ~@ index

(* Guideline : whenever you match an AST, define an alias rather than doing 'open' *)
module Q = QmlAst

type pos = FilePos.pos
type typevar = TypeVarSet.elt
type t =
  | Const    of pos * Q.const_ty
  | TypeVar  of pos * typevar
  | Void     of pos
  | Bool     of pos
  | Option   of pos * t
  | OpaValue of pos * t
  | Fun      of pos * t list * t
  | External of pos * string * t list

let pos = LangAst.pos
let reset_pos = LangAst.reset_pos
let merge_pos = LangAst.merge_pos

type 't pprinter = 't Base.Format.pprinter
let pp_list = Base.Format.pp_list
let pp_parameters = LangPrint.pp_parameters

let ty_void = Opacapi.Types.void
let ty_bool = Opacapi.Types.bool
let ty_option = Opacapi.Types.option
let ty_opavalue = "opa"
let ty_external = "external"

let pp_scope ~scope fmt =
  let typevar fmt = TypeVarPrint.pp scope fmt in
  let rec aux parfun fmt = function
    | Const (_, k) ->
        Q.Const.pp_ty fmt k

    | TypeVar (_, v) ->
        typevar fmt v

    | Void _ ->
        Format.pp_print_string fmt ty_void

    | Bool _ ->
        Format.pp_print_string fmt ty_bool

    | Option (_, t) ->
        Format.fprintf fmt "%s(%a)" ty_option (aux false) t

    | OpaValue (_, t) ->
        Format.fprintf fmt "%s[%a]" ty_opavalue (aux false) t

    | ( Fun (_, u, v) ) as t ->
        if parfun then Format.fprintf fmt "(%a)" (aux false) t else
          let paren_out = true in
          Format.fprintf fmt "%a -> %a" (pp_list ", " (aux true)) u (aux paren_out) v

    | External (_, n, vs) ->
        Format.fprintf fmt "%s[%a]" ty_external (pp_parameters (aux true) n) vs

  in aux false fmt

let pp fmt =
  let scope = TypeVarPrint.new_scope () in
  pp_scope ~scope fmt

(* Error without context *)
let (!!) pos fmt =
  FilePos.citation OManager.oformatter.contents pos ;
  OManager.error fmt


type this_t = t (* ocaml does not like type 'a t = t in Subs *)

module Subs : TraverseInterface.S2
  with type 'a t = this_t constraint 'a = _ * _ * _ =
struct
  type 'a t = this_t constraint 'a = _ * _ * _

  let foldmap tra acc t =
    match t with
    | Const _
    | TypeVar _
    | Void _
    | Bool _ -> acc, t
    | Option (pos, t') ->
        let acc, ft' = tra acc t' in
        acc,
        if t' == ft' then t else
          Option (pos, ft')

    | OpaValue (pos, t') ->
        let acc, ft' = tra acc t' in
        acc,
        if t' == ft' then t else
          OpaValue (pos, ft')

    | Fun (pos, u, v) ->
        let acc, fu = List.fold_left_map_stable tra acc u in
        let acc, fv = tra acc v in
        acc,
        if u == fu && v = fv then t else
          Fun (pos, fu, fv)

    | External (pos, name, params) ->
        let acc, fparams = List.fold_left_map_stable tra acc params in
        acc,
        if params == fparams then t else
          External (pos, name, fparams)

  (* TODO: optimized versions *)
  let iter x = Traverse.Unoptimized.iter foldmap x
  let map x = Traverse.Unoptimized.map foldmap x
  let fold x = Traverse.Unoptimized.fold foldmap x
end

module Walk = Traverse.Make2 ( Subs )

type freevars = TypeVarSet.t

let fold_freevars =
  Walk.fold
    (fun acc -> function
       | TypeVar (_, v) -> TypeVarSet.add v acc
       | _ -> acc)

let freevars = fold_freevars TypeVarSet.empty

let nopos = FilePos.nopos "BslTypes"
let quantify_sort set =
  let var v = TypeVar (nopos, v) in
  List.map var (TypeVarSet.elements set)

(* reset type variable from 0 *)
(* dont factorize ty because of the Hashtbl *)
let normalize t =
  let c = ref 0 in
  let box = Hashtbl.create 10 in
  let map i =
    try
      Hashtbl.find box i
    with
    | Not_found ->
        let v = ~@ !c in
        incr(c) ;
        (* TODO: hook there for Valentine's patch *)
        Hashtbl.add box i v ;
        v
  in
  Walk.map_down
    (function
     | ( TypeVar (pos, v) ) as t ->
         let fv = map v in
         if v == fv then t else TypeVar (pos, fv)
     | t -> t) t

(* for coherence of label normalize *)
let normalize_alias = normalize

let rec opavalue = function
  | OpaValue (_, t) -> opavalue t
  | Fun (pos, arg, ret) ->
      let arg = List.map opavalue arg in
      let ret = opavalue ret in
      Fun (pos, arg, ret)
  | External _ as t -> t
  | t ->
      OpaValue (pos t, t)

let purge_opavalue = Walk.map_up (
  function
  | OpaValue (_, t) -> t
  | t -> t
)

(* {6 Substitution} *)

type 'a substitution = 'a TypeVarMap.t

let empty_substitution = TypeVarMap.empty

let substitute subst =
  Walk.map_up (* <!> beware, map_down loops if 'a is rewriten in a term containing 'a *)
    (function
     | ( TypeVar (_, v) ) as t -> (
         match TypeVarMap.find_opt v subst with
         | Some t -> t
         | None -> t
       )
     | t -> t)

(* {6 comparaison} *)

(* TODO inline normalization with 2 hashtbl, not this naive approach *)
let compare ?(normalize=false) a b =
  let rec compare a b =
    match a, b with
    | Const (_, c), Const (_, c') -> Pervasives.compare c c'
    | Const _, _ -> -1
    | _, Const _ -> 1
    | TypeVar (_, v), TypeVar (_, v') -> TypeVarSet.compare_elt v v'
    | TypeVar _, _ -> -1
    | _, TypeVar _ -> 1
    | Void _, Void _ -> 0
    | Void _, _ -> -1
    | _, Void _ -> 1
    | Bool _, Bool _ -> 0
    | Bool _, _ -> -1
    | _, Bool _ -> 1
    | Option (_, u), Option (_, v) -> compare u v
    | Option _, _ -> -1
    | _, Option _ -> 1
    | OpaValue (_, t), OpaValue (_, t') -> compare t t'
    | OpaValue _, _ -> -1
    | _, OpaValue _ -> 1
    | Fun (_, u, v), Fun (_, u', v') ->
        let r = List.make_compare compare u u' in
        if r <> 0 then r
        else compare v v'
    | Fun _ , _ -> -1
    | _, Fun _ -> 1
    | External (_, n, tl), External (_, m, ttl) ->
        let r = String.compare n m in
        if r <> 0 then r
        else List.make_compare compare tl ttl
  in
  if normalize
  then compare (normalize_alias a) (normalize_alias b) (* cf TODO *)
  else compare a b

(* {6 Checking} *)

let pp_citation fmt t =
  let pos = pos t in
  if not (FilePos.is_empty pos) then FilePos.citation fmt pos else ()

let pp_context fmt t =
  let pos = pos t in
  if not (FilePos.is_empty pos) then FilePos.citation fmt pos else pp fmt t

let pp_multi_context fmt ts =
  let mpos = List.fold_left (fun acc t -> FilePos.merge_pos acc (pos t)) nopos ts in
  (if not (FilePos.is_empty mpos) then FilePos.citation fmt mpos);
  List.iter (
    fun t ->
      if FilePos.is_empty (pos t) then Format.fprintf fmt "type %a@\n" pp t
  )
    ts


let is_second_order t =
  let contains_arrow = Walk.exists (
    function
    | Fun _ -> true
    | _ -> false
  )
  in
  match t with
  | Fun (_, args, returned) ->
      List.exists contains_arrow (returned::args)
  | _ -> false


let fail_check global_expected global_found expected found () =
  let fmt = OManager.oformatter.contents in
  (* FilePos.citation OManager.oformatter.contents (pos t) ; *)
  OManager.printf "During External Primitives Type Checking@\n" ;
  pp_citation fmt global_expected;
  pp_citation fmt global_found;
  OManager.printf "The context expects the type       : %a@\n" pp global_expected ;
  OManager.printf "where the type found at runtime is : %a@\n" pp global_found ;
  if (global_expected != expected) && ( global_found != found) then
    OManager.printf "Type %a is not included in type %a@\n" pp found pp expected
  else ();
  OManager.error "@\n"

let check_parametric_type fail aux subst n n' vs vs' =
  if String.compare n n' <> 0 then fail () else
    try
      List.fold_left2 aux subst vs vs'
    with
    | Invalid_argument _ -> fail ()

let check_inclusion ?(static_strict_check=true) subst ~expected ~found =
  let fail = fail_check expected found in
  let rec aux subst expected found =
    let () =
      #<If:BSL_PROJECTION>
        debug "check_inclusion(aux) %b %a %a@." static_strict_check pp expected pp found
      #<End>
    in
    let fail = fail expected found in
    match (expected, found) with
    | TypeVar (_, v), t -> (
        match TypeVarMap.find_opt v subst with
        | None ->
            (* From there, any v should be a t *)
            TypeVarMap.add v t subst
        | Some t' ->
            aux subst t' t
      )

    | t, TypeVar (_, v) -> (
        if static_strict_check then
          fail ()
        else
        (*
          Used principally in opatop, when we apply e.g. a function of type :
          ['a -> foo('a)] on a string, the result in a foo(string), but we
          should not tag the type as foo(string), because a conversion would
          be done on the string.
        *)
          match TypeVarMap.find_opt v subst with
          | None ->
              (* From there, any v should be a t *)
              TypeVarMap.add v t subst
          | Some t' ->
              aux subst t' t
      )

    | Const (_, c), Const (_, c') ->
        if Pervasives.compare c c' <> 0 then fail () else subst
    | Const _, _
    | _, Const _ -> fail ()

    | Void _, Void _ -> subst
    | Void _, _
    | _, Void _ -> fail ()

    | Bool _, Bool _ -> subst
    | Bool _ , _
    | _, Bool _ -> fail ()

    | Option (_, t), Option (_, t') -> aux subst t t'
    | Option _, _
    | _, Option _ -> fail ()

    | OpaValue (_, t), OpaValue (_, t') -> aux subst t t'
    | OpaValue _, _
    | _, OpaValue _ -> fail ()

    | Fun (_, u, v), Fun (_, u', v') ->
        let subst =
          try
            List.fold_left2 aux subst u u'
          with
          | Invalid_argument "List.fold_left2" -> fail ()
        in
        aux subst v' v (* beware of inversion *)

    | Fun _, _
    | _, Fun _ -> fail ()

    | External (_, n, vs), External (_, n', vs') ->
        check_parametric_type fail aux subst n n' vs vs'

  in aux subst expected found

let check ?static_strict_check ~expected ~found =
  let _ = check_inclusion ?static_strict_check empty_substitution ~expected ~found in ()

let fail_specialize vars t fmt =
  let citation t =
    let pos = pos t in
    if not (FilePos.is_empty pos) then FilePos.citation OManager.oformatter.contents pos in
  (* FilePos.citation OManager.oformatter.contents (pos t) ; *)
  OManager.printf "During External Primitives Type Specialization@\n" ;
  citation t;
  OManager.printf "The type is : %a@\n" pp t;
  OManager.printf "@[<2>The parameters are :@\n%a@]@\n" (pp_list "@\n" pp) vars;
  OManager.error fmt

let specialize vars t =
  match t with
  | External (_, _, v) ->
      let subst = empty_substitution in
      let subst =
        let fold2 subst expected found = check_inclusion subst ~expected ~found in
        try
          List.fold_left2 fold2 subst v vars
        with
        | Invalid_argument "List.fold_left2" -> fail_specialize vars t "Invalid arity@\n"
      in
      substitute subst t

  | _ ->
      fail_specialize vars t "BslTypes.specialize : this type is not parametric,@ it cannot be specialized@\n"

(* {6 Binding with QmlAst.ty} *)

let of_const c = Const (nopos, Q.Const.type_of c)

(* TODO: add pos in QmlAst *)
let fail_ty ty sub_ty fmt =
  (* FilePos.citation OManager.oformatter.contents (pos t) ; *)
  OManager.printf "Context type is : %a@\n" QmlPrint.pp#ty ty ;
  (if ty != sub_ty then
     OManager.printf "in the part : %a@\n" QmlPrint.pp#ty sub_ty ;
  );
  OManager.error fmt


(* TODO: detect opavalue cases *)
let of_ty ~gamma ty =
  let fail_ty x = fail_ty ty x in
  let rec aux ?(name=None) varmap ty =
    let fail_ty x = fail_ty ty x in
    let pos = (* get pos from ty *) nopos in
    match ty with
    | Q.TypeConst const -> varmap, Const (pos, const)

    | Q.TypeVar var -> (
        (* build a TypeVar , with coherence for a full call to of_ty *)
        match TypeVarMap.find_opt var varmap with
        | None ->
            let alpha = TypeVar (pos, TypeVar.refresh var) in
            TypeVarMap.add var alpha varmap, alpha
        | Some alpha -> varmap, alpha
      )

    | Q.TypeArrow (la, b) ->
        let varmap, auxla = List.fold_left_map aux varmap la in
        let varmap, auxb = aux varmap b in
        varmap, Fun (pos, auxla, auxb)

    | (Q.TypeName (ty_list, typeident)) -> (
        match
          (* A priori, this is part is used by the back-end so it needs to
             access the internal of types even if they are not visible from the
             package. Typechecking will have to have ensured that types were
             used in a consistent way. *)
          QmlTypes.Env.TypeIdent.findi_opt
            ~visibility_applies: false typeident gamma with
        | Some (typeident, typ) ->
            let ident = Q.TypeIdent.to_string typeident in
            if Q.TypeIdent.is_external_ty typeident then
              let varmap, maped_ty = List.fold_left_map aux varmap ty_list in
              let maped = External (pos, ident, maped_ty) in
              varmap, maped
            else (
              (* Standard extended types in LibBSL : bool, option *)
              match ident with
              | "bool" -> varmap, Bool pos
              | "unit" | "void" -> varmap, Void pos
              | "option" -> (
                  match ty_list with
                  | [what] ->
                      let varmap, auxwhat = aux varmap what in
                      varmap, Option (pos, auxwhat)
                  | _ ->
                      (** Here we can raise a public typing exception because the type option is
                          used with a wrong number of parameters *)
                      fail_ty
                        "The external constructor \"option\" expects 1 arg@ but is here called with %d arguments(s)@\n"
                        (List.length ty_list)
                )
              | other -> (
                  (* warning : an typename do point on a other type that is instancied *)
                  (* side-effect so that we can use the quantify_sort function *)
                  List.iter (ignore @* aux varmap) ty_list;
                  let ty = (QmlTypes.Scheme.specialize ~typeident:typeident ~ty:ty_list typ) in
                  aux ~name:(Some other) varmap ty
                )
            )

        | None ->
            (* This can appears in some funny cases with #typer off, playing with qmltop *)
            (* in this case, we build an extern type *)
            let ident = Q.TypeIdent.to_string typeident in
            let varmap, maped_ty = List.fold_left_map aux varmap ty_list in
            let maped = External (pos, ident, maped_ty) in
            varmap, maped
      )

    | Q.TypeRecord (Q.TyRow ([], None))
    | Q.TypeSum (Q.TyCol ([[]], _)) ->
        (* We close the column variable by passing it the the external primitive *)
        varmap, Void pos

    | Q.TypeRecord t ->
        let name =
          match name with
          | Some n -> n
          | None ->
              fail_ty "Opa anonymous records cannot escape the opa wold.@ You should name this type for interacting with an external primitive.@\n"
        in
        let fold_map varmap (field, ty) =
          let varmap, auxty = aux varmap ty in varmap, (field, auxty) in
        let varmap, maped_fields =
          List.fold_left_map fold_map varmap (QmlAstWalk.Row.elements t) in
        let parameters =
          let freevars =
            List.fold_left
              (fun acc (_, t) -> fold_freevars acc t) TypeVarSet.empty maped_fields
          in
          (* there we are sure that the order is correct because of
             the side-effect done with TypeName *)
          quantify_sort freevars
        in
        let std = External (pos, name, parameters) in
        varmap, std

    | other ->
        let pos = nopos (* TODO: add pos in QmlAst *) in
        OManager.warning ~wclass:WarningClass.bsl_type_checking
          "%aThe type@ %a@ will be given as an external alpha to the external library@\n"
          pp_citation pos QmlPrint.pp#ty other ;
        (* This is dangerous,
           but the type checking combine with the bypass typer assure
           that we cannot do bad things *)
        (* However, this is buggy. We should return the same var from 2 equal ty *)
        (* TODO: version 1 : try just with a Hashtbl on other directly, is maybe enough *)
        (* TODO: version 2 : hazardous use of Type comparaison (mamamia) *)
        varmap, TypeVar (pos, TypeVar.next ())
  in
  let _, sttyp = aux ~name:None TypeVarMap.empty ty in
  sttyp

(* TODO: add position in QmlAst *)
let to_ty ?(typeident=Q.TypeIdent.of_string) t = (* don't factorize t because of the typevarmap *)
  let map =
    let box = ref TypeVarMap.empty in
    (fun v ->
       match TypeVarMap.find_opt v !box with
       | Some qv -> qv
       | None ->
           let qv = Q.TypeVar (TypeVar.refresh v) in
           box := TypeVarMap.add v qv !box;
           qv
    )
  in
  let rec aux = function
    | Const (_, const) -> Q.TypeConst const
    | TypeVar (_, v) -> map v
    | Void _ -> Q.TypeRecord (Q.TyRow ([], None))
    | Bool _ -> Q.TypeName ([], Q.TypeIdent.of_string Opacapi.Types.bool)
    | Option (_, t) -> Q.TypeName ([aux t], Q.TypeIdent.of_string Opacapi.Types.option)
    | OpaValue(_, t) -> aux t
    | Fun (_, u, v) ->
        let u = List.map aux u in
        let v = aux v in
        Q.TypeArrow (u, v)

    | External (_, name, tlist) ->
        Q.TypeName (List.map aux tlist, typeident ~check:false name)

  in aux t

(* {6 Bslregister Code Generation} *)

(* TODO: use a brightness or something for highlithing citations *)
let fmt m =
"%aType variables for parametric extern types should be generic only.@\nThe instance : %a@ in the type  : %a@\nis not allowed in the interface of this bypass@\n"^^m

let notify context allowed runtime instance parent =
  if allowed then
    OManager.warning ~wclass:WarningClass.bsl_backend_restriction
      (fmt
         "However, a compatibility of runtime(s) is specified for this bypass@\nthis primitive will be available only with the following back-ends :@\n@\t%s@\n"
      )
      pp_citation context pp instance pp parent runtime
  else
    OManager.error
      (fmt
         "However, if you want this primitive to be used with a specific runtime back-end algebra,@\nplease add a runtime restriction specification :@\nexample : ##register [backend:qmlflat, ...]"
      )
      pp_citation context pp instance pp parent

let check_runtime_restriction bsltags t =
  let context = t in
  let runtime, allowed =
    if BslTags.never_projected bsltags
    then "anyruntime ([no-projection] is set)", true
    else
      match Option.default_map [] StringSet.elements bsltags.BslTags.backend_restriction with
      | [] -> "", false
      | backend -> (String.concat ", " backend, true)
  in
  (* detect instanciate types variables *)
  let iter parent = function
    | OpaValue _
    | External _
    | TypeVar _ -> ()
    | instance ->
        notify context allowed runtime instance parent

  in
  Walk.traverse_iter
    (fun tra t -> match t with
     | OpaValue _ -> () (* ignored case *)
         (* checked cases *)
     | External (_, _, params) -> List.iter (iter t) params; tra t
     | t -> tra t) t

(* {6 Bslregister Code Generation} *)

(* TODO:
   export a function in FilePos to get position after BslDynloading.
*)
(* <!> keep in synch with BslPluginInterface, meta_plugin__02 *)
let meta_pos = FilePos.nopos "BslTypes.meta_pos"
let var_mp = "mp"

let rec pp_meta_scope scope fmt t =
  let pp_meta f = pp_meta_scope scope f in
  match t with
  | Const (_, c) -> Format.fprintf fmt "B.Const (%s, Q.%s)" var_mp (Q.Const.meta c)
  | TypeVar (_, v) -> Format.fprintf fmt "B.TypeVar (%s, ~$ %S)" var_mp (TypeVarPrint.get scope v)
  | Void _ -> Format.fprintf fmt "B.Void %s" var_mp
  | Bool _ -> Format.fprintf fmt "B.Bool %s" var_mp
  | Option (_, t) -> Format.fprintf fmt "B.Option (%s, (%a))" var_mp pp_meta t
  | OpaValue (_, t) -> Format.fprintf fmt "B.OpaValue (%s, (%a))" var_mp pp_meta t

  | Fun (_, u, v)      ->
      Format.fprintf fmt "B.Fun (%s, [%a], (%a))" var_mp (pp_list "@ ;@ " pp_meta) u pp_meta v

  | External (_, n, vs) ->
      Format.fprintf fmt "B.External (%s, %S, [%a])" var_mp n (pp_list "@ ;@ " pp_meta) vs

and pp_meta_fields_scope scope fmt fds =
  let fd fmt (f, t) = Format.fprintf fmt "(%S, %a)" f (pp_meta_scope scope) t in
  Format.fprintf fmt "[ %a ]" (pp_list "@ ;@ " fd) fds

let pp_meta fmt t =
  let scope = TypeVarPrint.new_scope () in
  pp_meta_scope scope fmt t

let pp_meta_fields fmt fds =
  let scope = TypeVarPrint.new_scope () in
  pp_meta_fields_scope scope fmt fds
