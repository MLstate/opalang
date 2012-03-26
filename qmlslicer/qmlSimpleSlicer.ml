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
module Format = Base.Format
module List = Base.List
module String = Base.String
module Q = QmlAst
module Package = ObjectFiles.Package

module WClass = struct

  let all =
    WarningClass.create
      ~public:true
      ~name:"slicer"
      ~doc:"All the warnings of the slicer"
      ~err:true
      ~enable:true
      ()

  let sliced_expr =
    WarningClass.create
      ~parent:all
      ~public:true
      ~name:"sliced_expr"
      ~doc:"Warns when a declaration with a @sliced_expr is not defined on both sides"
      ~err:true
      ~enable:true
      ()

  module Server = struct
    (* can only be checked at link time *)
    (** when a server directive has no purpose, TODO *)
    let useless =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"server.useless"
        ~doc:"Warns when a declaration with a server directive is never called from the client (i.e. remove it)"
        ~err:false
        ~enable:true
        ()

    (** when a server directive is ignored *)
    let meaningless =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"server.meaningless"
        ~doc:"Warns when a declaration with a server directive is using a protected (or server_private) value"
        ~err:false
        ~enable:true
        ()

    (** when a server directive is generating first order call back to the client *)
    let misleading =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"server.misleading"
        ~doc:"Warns when a declaration with a server directive is calling the client called"
        ~err:false
        ~enable:true
        ()
  end

  module Protected = struct
    (** when a protected directive is generating first order call back to the client *)
    let misleading =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"protected.misleading"
        ~doc:"Warns when a declaration with a protected directive is calling the client called"
        ~err:false
        ~enable:true
        ()

    (** when a exposed directive is generating first order call back to the client *)
    let implicit_access =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"protected.implicit.expose"
        ~doc:"Warns when a xhtml event is giving access to a protected value by being implicitly exposed but completly server side (safe in most cases)"
        ~err:false
        ~enable:true
        ()
  end

  module Exposed = struct
    (** when an exposed directive is not exposing a protected value *)
    let meaningless =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"exposed.meaningless"
        ~doc:"Warns when a declaration is asked to be exposed but it is not using any protected value"
        ~err:false
        ~enable:true
        ()
    (** when an exposed directive is adding an entry point uselessly TODO *)
    let useless =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"exposed.useless"
        ~doc:"Warns when a declaration with an exposed directive is never called from client"
        ~err:false
        ~enable:true
        ()
    (** when a exposed directive is generating first order call back to the client *)
    let misleading =
      WarningClass.create
        ~parent:all
        ~public:true
        ~name:"exposed.misleading"
        ~doc:"Warns when a declaration with an exposed directive is calling the client"
        ~err:false
        ~enable:true
        ()
  end

  let as_ignored l = List.iter (fun wclass -> WarningClass.set_warn       wclass false;
                                              WarningClass.set_warn_error wclass false) l
  let as_warning l = List.iter (fun wclass -> WarningClass.set_warn       wclass true;
                                              WarningClass.set_warn_error wclass false) l
  let as_error li lw all = List.iter (fun wclass ->
    if not(List.mem wclass li) && not(List.mem wclass lw) then (
      WarningClass.set_warn       wclass true;
      WarningClass.set_warn_error wclass true
    )
  ) all

  (* first list => ignored
     second list => warning
     otherwise error *)
  let all_swclass = [ Server.meaningless ; Server.useless  ; Server.misleading    ;
                      Exposed.meaningless; Exposed.useless ; Exposed.misleading   ;
                      (*TODO*)            (* TODO *)         Protected.misleading ; Protected.implicit_access]
  let security_levels_warnings = [
    "low", (
      all_swclass,
      []
    );
    "warnall", (
      [],
      all_swclass
    );
    "normal", (
      [Exposed.misleading; Server.misleading; Protected.implicit_access; Server.misleading],
      [Protected.misleading]
    );
    "high", (
      [Exposed.misleading],
      [Server.misleading;Protected.implicit_access]
    );
    "higher", (
      [],
      [Exposed.misleading;Protected.implicit_access]
    );
    "pedantic", (
      [],
      []
    )
  ]

  let select_security_level level =
    let ignored, warn = List.assoc level security_levels_warnings
    in
    as_ignored ignored;
    as_warning warn;
    as_error ignored warn all_swclass

  let security_levels = List.map fst security_levels_warnings

  let warning_set = WarningClass.Set.create_from_list ([
    all;
    sliced_expr;
  ] @ all_swclass)

end

let warning_set = WClass.warning_set

module Options = struct

  module Arg = Base.Arg

  module Type = struct
    type options = {
      check_level : string;
    }
  end

  include Type

  let default_options = {
    check_level = "normal"
  }
  let _ = WClass.select_security_level default_options.check_level

  let r = ref default_options

  let list =
  [
    "--slicer-check",
    Arg.Symbol (WClass.security_levels, (fun level ->
      r := { check_level = level };
      WClass.select_security_level level;
    )),
    Format.sprintf " Level of security of the slicing checks (%a) [%s]"
      (Format.pp_list "@ " Format.pp_print_string) WClass.security_levels
      default_options.check_level
    ;
  ]

end


type splitted_code = {
  code : QmlAst.code ;
  published : Pass_ExplicitInstantiation.published_map;
  renaming : QmlRenamingMap.t ;
}

type side_annotation =
  | Client (* client side *)
  | Server (* server side *)
  | Both   (* side independent *)
type user_wish =
  | Prefer
  | Force
type user_annotation = { side : side_annotation; wish : user_wish }
type client_code_kind =
  [ `expression
  | `insert_server_value (* SHOULD NOT BE USED FOR FUNCTIONAL TYPES, it is useless *)
  | `alias ]
type server_code_kind =
  [ `expression
  | `alias ]

(**
   A weakened form of type [privacy], exported to the rest of the compiler
*)
type publication = [ `Published of [`sync | `async | `funaction ]
                   | `Private ]

type privacy =
  | Published of bool (* the bool indicate that the publish was implicit *)
  | Private
  | Visible

let variant_of_async async =
  if async then `async else `sync

type 'a value =
  | Local of 'a
  | External of Package.t

type information = (* TODO: explicit the invariants *)
    { (* fields that aren't computed *)
      privacy : privacy;
      implemented_both : bool;
      user_annotation : user_annotation option;
      ident : Ident.t;
      async : bool;
      mutable expr : Q.expr value; (* this field is muted only at the very end
                             * to avoid marshalling the expression *)

      (* computed by initialize_env *)
      mutable calls_server_bypass : BslKey.t option;
      mutable calls_client_bypass : BslKey.t option;
      mutable has_sliced_expr : bool;
      mutable lambda_lifted : Ident.t list;

      (* computed by propagate_server_private *)
      mutable calls_private : information value option; (* this field is independent of the @publish annotation *)

      (* TODO handle instantaneous deps *)

      (* computed by the kind of effect analysis *)
      mutable does_side_effects : bool;

      (* these fields are computed by choose sides *)
      mutable needs_the_server : bool;
      mutable needs_the_client : bool;
      (*mutable need_serialization : bool; (* not equivalent to calls_the_client || calls_the_server
                                          * because you need serialization mechanisms
                                          * for @insert_server_value, but there is no call
                                          * (at least if there is no function) *)*)
      mutable on_the_server : server_code_kind option option; (* TODO: use a right default value? options because this is unset at the beginning *)
      mutable on_the_client : client_code_kind option option; (* same thing *)
      mutable publish_on_the_client : bool; (* need a @comet_publish *)
      mutable publish_on_the_server : bool; (* need an @ajax_publish *)

      (* these fields are computed by the renaming are used to be able
       * to do the same alpha conversion of identifiers across different
       * compilation units *)
      mutable server_ident : [ `ident of Ident.t
                             | `tsc of (QmlAst.ty, unit) QmlGenericScheme.tsc option
                             | `ident_tsc of Ident.t * (QmlAst.ty, unit) QmlGenericScheme.tsc option
                             | `undefined ];
      (* if the declaration is defined on the server,
       * gives the renamed identifier
       * if not, it gives the typescheme to put on the @ajax_call *)
      mutable client_ident : [ `ident of Ident.t
                             | `tsc of (QmlAst.ty, unit) QmlGenericScheme.tsc option
                             | `ident_tsc of Ident.t * (QmlAst.ty, unit) QmlGenericScheme.tsc option
                             | `undefined ];
    }

let pp_option pp_a f = function
  | None -> Format.pp_print_string f "None"
  | Some a -> Format.fprintf f "Some %a" pp_a a
let pp_server_ident f = function
  | `ident i -> Format.fprintf f "`ident %s" (Ident.to_string i)
  | `tsc tsc_opt -> Format.fprintf f "`tsc %a" (pp_option QmlPrint.pp_base#tsc) tsc_opt
  | `ident_tsc (i, tsc_opt) ->
      Format.fprintf f "`ident_tsc (%s, %a)" (Ident.to_string i) (pp_option
        QmlPrint.pp_base#tsc) tsc_opt
  | `undefined -> Format.pp_print_string f "undefined"
let pp_kind f = function
  | `expression -> Format.pp_print_string f "`expression"
  | `insert_server_value -> Format.pp_print_string f "`insert_server_value"
  | `alias -> Format.pp_print_string f "`alias"
let pp_value pp_a f = function
  | Local a -> Format.fprintf f "Local %a" pp_a a
  | External p -> Format.fprintf f "External %a" Package.pp p
let pp_info_ident f {ident; _} = Format.pp_print_string f (Ident.to_string ident)
let pp_privacy f = function
  | Published _ -> Format.pp_print_string f "Published"
  | Private -> Format.pp_print_string f "Private"
  | Visible -> Format.pp_print_string f "Visible"
let pp_info f {ident; server_ident; client_ident;
               publish_on_the_server; publish_on_the_client;
               on_the_server; on_the_client;
               calls_private; privacy;
               calls_server_bypass; calls_client_bypass; _} =
  Format.fprintf f "@[<v>{@[<v2>@ ident: %s@ server_ident: %a@ client_ident: %a\
                                @ publish_on_the_server: %B@ publish_on_the_client: %B\
                                @ on_the_server: %a@ on_the_client: %a\
                                @ calls_private: %a@ privacy: %a\
                                @ calls_server_bypass: %a@ calls_client_bypass: %a@]@ }@]"
    (Ident.to_string ident) pp_server_ident server_ident pp_server_ident client_ident
    publish_on_the_server publish_on_the_client
    (pp_option (pp_option pp_kind)) on_the_server (pp_option (pp_option pp_kind)) on_the_client
    (pp_option (pp_value pp_info_ident)) calls_private pp_privacy privacy
    (pp_option BslKey.pp) calls_server_bypass (pp_option BslKey.pp) calls_client_bypass

module Information =
struct
  type t = information
  let compare info1 info2 = Ident.compare info1.ident info2.ident
  let equal info1 info2 = Ident.equal info1.ident info2.ident
  let hash info = Ident.hash info.ident
end
module G = struct
  include Graph.Imperative.Digraph.ConcreteBidirectional(Information)
  let exists_succ f graph node =
    Return.set_checkpoint
      (fun label ->
         iter_succ (fun node -> if f node then Return.return label true) graph node;
         false
      )
  let find_succ f graph node =
    Return.set_checkpoint
      (fun label ->
         iter_succ (fun node -> if f node then Return.return label node) graph node;
         raise Not_found
      )
end

type environment =
    { informations : information IdentTable.t;
      call_graph : G.t;
      client_language : BslLanguage.t;
      server_language : BslLanguage.t; (* could have a debug mode where both sides are ml *)
      bymap : BslLib.BSL.ByPassMap.t;
      gamma : QmlTypes.gamma;
      annotmap : Q.annotmap;
    }

let get_bypass_side env bslkey =
  match BslLib.BSL.ByPassMap.find_opt env.bymap bslkey with
  | None -> assert false (* shouldn't have undefined bypass at that point *)
  | Some bypass ->
      let langs = BslLib.BSL.ByPass.langs bypass in
      let impl_client = List.mem env.client_language langs in
      let impl_server = List.mem env.server_language langs in
      match impl_server,impl_client with
      | true,true -> `both
      | false,true -> `client
      | true,false -> `server
      | _ -> assert false (* could happen in we use a c only bypass *)


(* TODO: annotation of db default values as full server
 * annotation of dbgen generated code as server private *)
(* TODO: pas de passe collect annotations *)
(* TODO: handle recursive annotations full_server -> pas besoin de serialization + at least on the server but not only? *)
(* TODO: annotation @assert_both etc? *)
(* TODO: never insert_server_value of any datatype containing functions? *)

let empty_env bymap typer_env =
  { informations = IdentTable.create 100;
    call_graph = G.create ();
    client_language = BslLanguage.js;
    server_language = BslLanguage.ml;
    bymap = bymap;
    gamma = typer_env.QmlTypes.gamma;
    annotmap = typer_env.QmlTypes.annotmap;
  }

(* same as rewriteAsyncLambda presumably *)
type ignored_directive = [
| Q.type_directive
| Q.lambda_lifting_directive
| Q.slicer_directive
]
let async_lambda e =
  QmlAstWalk.Expr.traverse_exists
    (fun tra -> function
     | Q.Coerce _
     | Q.Directive (_, #ignored_directive, _, _) ->
         tra e
     | Q.Lambda _ -> true
     | _ -> false
    ) e

let rec slicer_annots_of_expr visibility both_implem side_annot async annotmap expr =
  match expr with
  | Q.Directive (label, `async, [e], _) when async_lambda e ->
      async := true;
      let tsc_gen = QmlAnnotMap.find_tsc_opt_label label !annotmap in
      annotmap := QmlAnnotMap.add_tsc_opt_label (Q.Label.expr e) tsc_gen !annotmap;
      slicer_annots_of_expr visibility both_implem side_annot async annotmap e
  | Q.Coerce (label, e, ty) ->
      let e' = slicer_annots_of_expr visibility both_implem side_annot async annotmap e in
      (*if e == e' then expr else*) Q.Coerce (label, e', ty)
  | Q.Directive (label, (#Q.type_directive as d), [e], ty) ->
      let e' = slicer_annots_of_expr visibility both_implem side_annot async annotmap e in
      (*if e == e' then expr else*) Q.Directive (label, d, [e'], ty)
  | Q.Directive (label, (`visibility_annotation _ | `side_annotation _ as v), [e], _) ->
    begin match v, !visibility, !side_annot with
       | `visibility_annotation v, None, _ ->
           visibility := Some (
             match v with
             | `public (`sync | `async as sync) ->
                 (async := match sync with `async -> true | `sync -> !async);
                 Published false
             | `private_ -> Private
             | `public `funaction -> Published true (* `sync*)
                 (* problem: since fun actions are lambda lifting with two groups
                  * of lambda, the funaction is onclick="f(env)(arg)"
                  * and the remote call f(env) does not return void
                  * it should be solved by putting fun action lifting after typing
                  * and by putting a partial apply directly
                  * when this is done, `funaction should become `async as is done
                  * in the commented code below
                  *)
           )
       | `side_annotation v, _, None ->
           side_annot := Some (
             match v with
             | `client -> {side=Client;wish=Force}
             | `server -> {side=Server;wish=Force}
             | `both -> {side=Both;wish=Force}
             | `both_implem -> both_implem := true; {side=Both;wish=Force}
             | `prefer_client -> {side=Client;wish=Prefer}
             | `prefer_server -> {side=Server;wish=Prefer}
             | `prefer_both -> {side=Both;wish=Prefer}
           )
       | `visibility_annotation _, Some _, _ ->
         let context = QmlError.Context.expr expr in
           QmlError.serror context "You have conflicting security annotations (protected,exposed) on the same declaration."
       | `side_annotation _,       _, Some _ ->
           let context = QmlError.Context.expr expr in
           QmlError.serror context "You have conflicting side annotations (server,client) on the same declaration."
    end;
    let tsc_gen = QmlAnnotMap.find_tsc_opt_label label !annotmap in
    annotmap := QmlAnnotMap.add_tsc_opt_label (Q.Label.expr e) tsc_gen !annotmap;
    slicer_annots_of_expr visibility both_implem side_annot async annotmap e
  | _ -> expr

let default_information ~env ~annotmap (ident,expr) =
  let visibility = ref None in
  let both_implem = ref false in
  let side_annot = ref None in
  let async = ref false in
  let expr = slicer_annots_of_expr visibility both_implem side_annot async annotmap expr in
  if !async then (
    (* we can't have asynchronous calls to functions that return something else than void
     * note that {} / ... is not good either because f(x:{} / ...) = x cannot
     * be called asynchronous
     * So we are NOT checking that the return type is unifiable with void,
     * we want exactly void
     * Another way to do that would be to force the typer to unify void and the return type
     * but for that, the directive would need to be still in the ast when typing *)
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr expr) !annotmap in
    let fail () =
      let context = QmlError.Context.expr expr in
      QmlError.serror context
        "@[@@async_publish can be put only on functions whose return type is {}@]@\n\
         @[<2>Hint:@\nit has type %a@]@."
        QmlPrint.pp#ty ty
    in
    (match QmlTypesUtils.Inspect.get_arrow_through_alias_and_private env.gamma ty with
     | None -> fail ()
     | Some (_params, ty) ->
         if not (QmlTypesUtils.Inspect.is_type_void env.gamma ty) then fail ());
  );
  { calls_private = None;
    lambda_lifted = [];
    calls_server_bypass = None;
    calls_client_bypass = None;
    privacy = Option.default Visible !visibility;
    implemented_both = !both_implem;
    user_annotation = !side_annot;
    async = !async;
    has_sliced_expr = false;
    expr = Local expr;
    on_the_server = None;
    on_the_client = None;
    publish_on_the_server = false;
    publish_on_the_client = false;
    needs_the_client = false;
    needs_the_server = false;
    ident = ident;
    does_side_effects = false;
    server_ident = `undefined;
    client_ident = `undefined;
  }, expr

let get_expr = function
  | {expr = Local expr; _} -> expr
  | {expr = External _; _} -> assert false
let is_external = function
  | {expr = External _; _} -> true
  | {expr = Local _; _} -> false

let pp_pos_a f label = FilePos.pp_pos f (Annot.pos label)
let pp_pos f info =
  match info.expr with
  | Local expr -> pp_pos_a f (Q.Label.expr expr)
  | External package -> Package.pp_full f package

let update_call_graph env info =
  let infos = env.informations in
  let call_graph = env.call_graph in
  match info.expr with
  | External _ -> ()
  | Local expr ->
      QmlAstWalk.Expr.iter_context_down
        (fun context -> function
         | Q.Ident (_, i) -> (
             try
               let info_i = IdentTable.find infos i in
               G.add_edge call_graph info info_i
             with Not_found -> ()
           );
             context

         | Q.Bypass (_, key) -> (
             match get_bypass_side env key with
             | `server -> info.calls_server_bypass <- Some key
             | `client -> info.calls_client_bypass <- Some key
             | `both -> ()
           );
             context

         | Q.Directive (label, `sliced_expr, _, _) ->
             if context then
               OManager.serror "@[<v>%a@]@\n@[<2>  You have a nested @@sliced_expr.@]"
                 pp_pos_a label;
             info.has_sliced_expr <- true;
             true

         | Q.Directive (label, (`side_annotation _ | `visibility_annotation _), _, _) ->
             let error_context = QmlError.Context.label label in
             QmlError.serror error_context "@[This is an invalid slicer annotation: they can only appear on toplevel bindings (or inside toplevel modules) or on function bindings.@]";
             context

         | Q.Directive (_, `lifted_lambda (_,hierarchy), _, _) ->
             assert (info.lambda_lifted = []);
             (* if the code is lifted, you have only one function per toplevel
                declaration (so at most one @lifted_lambda) *)
             info.lambda_lifted <- hierarchy;
             context

         | _ ->
             context
        )
        false
        expr

let initialize_env ~env code =
  let annotmap = ref env.annotmap in
  let call_graph = env.call_graph in
  let initialize_bindings iel =
    List.map
      (fun ((i,_) as bnd) ->
         let info, e = default_information ~env ~annotmap bnd in
         IdentTable.add env.informations i info;
         G.add_vertex call_graph info;
         (i, e)
      ) iel in
  let code =
    List.map
      (function
       | Q.NewVal (label,iel) ->
           Q.NewVal (label,initialize_bindings iel)
       | Q.NewValRec (label,iel) ->
           Q.NewValRec (label,initialize_bindings iel)
       | Q.NewType _ -> assert false
       | Q.Database _ -> assert false
       | Q.NewDbValue _ -> assert false)
      code in
  IdentTable.iter (fun _ info -> update_call_graph env info) env.informations;
  {env with annotmap = !annotmap}, code

module G_for_server_private =
struct
  include G
  let iter_succ f graph node =
    iter_succ (fun node -> match node.privacy with Published _ -> () | _ -> f node) graph node
  let exists_succ f graph node =
    exists_succ (fun node -> match node.privacy with Published _ -> false | _ -> f node) graph node
  let find_succ f graph node =
    find_succ (fun node -> match node.privacy with Published _ -> false | _ -> f node)  graph node
  let find_opt_succ f graph node =
    try Some (find_succ f graph node) with Not_found -> None
end
module SCC_for_server_private = GraphUtils.Components.Make(G_for_server_private)

let propagate_server_private env =
  let graph = env.call_graph in
  let groups = SCC_for_server_private.scc ~size:200 graph in
  List.iter
    (fun group ->
       let info_opt =
         List.find_map
           (fun info ->
              if info.calls_server_bypass <> None || info.privacy = Private
              then Some info
              else
                G_for_server_private.find_opt_succ
                  (fun node -> node.calls_private <> None) graph info)
           group in
       match info_opt with
       | Some info -> List.iter (fun node -> node.calls_private <- Some (Local info)) group
       | None -> ()
    ) groups


module S_eff =
struct
  type t = QmlEffects.SlicerEffect.env
  let pass = "qmlSimpleSlicerEffect"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R_eff =
struct
  include ObjectFiles.Make(S_eff)
  let load () =
    fold
      (fun (eff1,typ1) (eff2,typ2) ->
         (IdentMap.safe_merge eff1 eff2, IdentMap.safe_merge typ1 typ2))
      (IdentMap.empty,IdentMap.empty)
  let save (load_eff,load_typ) (final_eff,final_typ) =
    let diff_env = (IdentMap.diff final_eff load_eff, IdentMap.diff final_typ load_typ) in
    save diff_env
end

let analyse_side_effects env code =
  let bypass_typer =
    let typer = BslLib.BSL.ByPassMap.bypass_typer env.bymap in
    fun s -> Option.get (typer s) in
  let initial_env = R_eff.load () in
  let (effect_env,_) as final_env = QmlEffects.SlicerEffect.infer_code ~initial_env bypass_typer code in
  R_eff.save initial_env final_env;
  IdentTable.iter
    (fun ident info ->
       info.does_side_effects <- QmlEffects.SlicerEffect.flatten_effect (IdentMap.find ident effect_env)
    ) env.informations

module SCC = GraphUtils.Components.Make(G)

let get_arity_opt gamma annotmap e =
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
  match QmlTypesUtils.Inspect.get_arrow_through_alias_and_private gamma ty with
  | Some (params, _) -> Some (List.length params)
  | None -> None

let get_arity_of_functional_type gamma annotmap e =
  Option.get (get_arity_opt gamma annotmap e)

let has_functional_type gamma annotmap e =
  match get_arity_opt gamma annotmap e with
  | None -> false
  | Some _ -> true


let rec find_private_path acc info =
  let acc = info :: acc in
  match info.privacy with
  | Private -> List.rev acc, `annot
  | Published _ | Visible ->
      match info.calls_server_bypass with
      | Some key -> List.tl (List.rev acc), `key key
      | None ->
          match info.calls_private with
          | None -> assert false
          | Some (Local info) -> find_private_path acc info
          | Some (External package) -> List.rev acc, `package package

let find_private_path info = find_private_path [] info
let pp_private_path pp_pos f info =
  let l,end_ = find_private_path info in
  let pp_info f info =
    Format.fprintf f "%s at @[<v>%a@]"
      (Ident.original_name info.ident)
      pp_pos info in
  let pp_end f = function
    | `key key -> Format.fprintf f "%%%%%a%%%% which is a server bypass" BslKey.pp key
    | `package package -> Format.fprintf f "from package %a" Package.pp_full package
    | `annot -> Format.fprintf f "which is annotated as 'protected'" in
  if l = [] then
    Format.fprintf f "@[<v>%a@]"
      pp_end end_
  else
    Format.fprintf f "@[<v>%a@ %a@]"
      (Format.pp_list "@ " pp_info) l
      pp_end end_

(* FIXME: with the smarter analysis for side effects, this function doesn't work anymore:
 * @server b = 1
 * @client a = (-> b)() would probably not do an insert_server_value when it should
 * this function contains some bugs anyway *)
let direct_dep_on_the_server env node =
  let informations = env.informations in
  let rec aux tra bnds = function
    | Q.Lambda _ -> true
    | Q.Ident (_, i) as expr -> (
        (* we don't have to care about recursive deps
         * (cases when on_the_server or on_the_client can be None)
         * because in recursion we only have lambdas, which never do side effects *)
        try
          match IdentTable.find informations i with
          | { on_the_server = Some (Some _); on_the_client = Some None; _} ->
              (* avoiding to put an insert_server_value on cases such as @server f() = ...; @both g = f
               * this is hackish and this should be done better by computing dependencies while doing
               * side effect analysis *)
              has_functional_type env.gamma env.annotmap expr
          | _ -> true
        with
        | Not_found -> true
      )
    | e -> tra bnds e in
  not (QmlAstWalk.Expr.traverse_forall_context_down aux IdentSet.empty (get_expr node))

type faulty = Private_path | No

let warn_tagged_but_use node ~wclass ~tagged ~use (faulty:faulty) consequence=
  OManager.warning ~wclass
    "@[<v>%a@]@\n@[<2>  %s is tagged as '%s' but it uses '%s' values%a%s@]"
    pp_pos node
    (Ident.original_name node.ident)
    tagged
    use
    (fun b node -> match faulty with
    | No -> Format.fprintf b "%s" ". "
    | Private_path -> Format.fprintf b ":@\n%a@\n" (pp_private_path pp_pos) node
    )
    node
    consequence

let may_warn_tagged_but_use ~emit node ~wclass ~tagged ~use faulty consequence =
  if emit then (
    warn_tagged_but_use node ~wclass ~tagged ~use faulty consequence;
    false
  ) else false

let check_privacy ~emit_error:_ ~emit node =
  let may_warn ~wclass ~tagged ~use faulty consequence =
    ignore(may_warn_tagged_but_use ~emit node ~wclass ~tagged ~use faulty consequence)
  in
  match node.privacy with
  | Published implicit ->
    (* an explicit exposed value is giving access to nothing protected *)
    let c1 = node.calls_private = None && not(implicit) in
    if c1 then may_warn ~wclass:WClass.Exposed.meaningless
      ~tagged:"exposed" ~use:"only non protected" No
      "The directive will be ignored"
    ;
    (* an implict exposed value is giving access to a protected value *)
    let c2 = node.calls_private <> None && implicit in
    if c2 then may_warn ~wclass:WClass.Protected.implicit_access
      ~tagged:"implicit exposed" ~use:"protected" Private_path
      "The access to these value is guaranteed to be safe, but they can be accessed."
    ;
    let c3 = node.needs_the_client && not(implicit) in
    if c3 then may_warn ~wclass:WClass.Exposed.misleading
      ~tagged:"exposed" ~use:"client" No
      "This is can be inefficient and may be a security threat."
    ;
    c1 && c2 && c3
  | Visible -> true
  | Private ->
    let c1 = node.needs_the_client in
    if c1 then may_warn ~wclass:WClass.Protected.misleading
      ~tagged:"protected" ~use:"client" No
      "This is probably a security threat."
    ;
    c1

let check_side ~emit_error ~emit node =
  let side_str = function
    | Server -> "server"
    | Both -> "both"
    | Client -> "client"
  in
  let c1 = if node.calls_private <> None then (
    match node.user_annotation with
    | Some {wish=Force; side=Server} when not(node.does_side_effects)->
      may_warn_tagged_but_use ~emit node ~wclass:WClass.Server.meaningless
        ~tagged:"server" ~use:"protected" Private_path
        "The directive will be ignored.";
    | Some {wish=Force; side=(Client|Both) as side} ->
      if emit || emit_error then (
      OManager.serror "@[<v>%a@]@\n@[<4>  %s is tagged as '%s' but it uses 'protected' values:@\n%a@]"
        pp_pos node
        (Ident.original_name node.ident)
        (side_str side)
        (pp_private_path pp_pos) node;
        false
      ) else true
    | _ -> true
  ) else true
  in
  let c2 = if node.needs_the_client then (
    match node.user_annotation with
    | Some {wish=Force; side=Server} ->
      may_warn_tagged_but_use ~emit node ~wclass:WClass.Server.misleading
        ~tagged:"server" ~use:"client" No
        "This can be inefficient.";
    | _ -> true
  ) else true
  in
  let c3 = if node.has_sliced_expr then (
    match node.user_annotation with
    | Some {wish=Force; side=(Client|Server) as side} ->
      may_warn_tagged_but_use ~emit node ~wclass:WClass.sliced_expr
        ~tagged:(side_str side) ~use:"sliced_expr" No
        "This is unusual."
    | _ -> true
  ) else true
  in c1 && c2 && c3

let check_node ?(emit_error=false) ~emit node =
  let c1 = check_privacy ~emit_error ~emit node in
  let c2 = check_side ~emit_error ~emit node in
  c1 && c2

let look_at_user_annotation env pp_pos node annot =
  let rec aux node annot =
    ignore( check_node ~emit_error:true ~emit:false node); (* only to catch errors *)
    match annot with
    | Some {wish=Force; side=Client} ->
        node.on_the_server <- Some None;
        node.on_the_client <- Some (Some `expression);
        node.publish_on_the_server <- false;
        node.publish_on_the_client <- true
    | Some {wish=Force; side=Server} ->
      (match node.calls_client_bypass with
        | Some key ->
             OManager.serror "@[<v>%a@]@\n@[<2>  %s is tagged as @@server but it contains a client bypass (%%%%%a%%%%).@]"
             pp_pos node
               (Ident.original_name node.ident)
               BslKey.pp key
        | None -> ());
        node.on_the_server <- Some (Some `expression);
        node.on_the_client <- Some None;
        node.publish_on_the_server <- node.calls_private = None || (match node.privacy with Published _-> true | _-> false);
        node.publish_on_the_client <- false
    | Some {wish=Force; side=Both} ->
        let fake_server, fake_client =
          if node.calls_private <> None then (
            (
            match node.privacy with
            | Published _ -> ()
            | _ ->
                OManager.serror "@[<v>%a@]@\n@[<4>  %s is tagged as 'both' but it uses a 'protected' values:@\n%a@]"
                  pp_pos node
                  (Ident.original_name node.ident)
                  (pp_private_path pp_pos) node
            );
            if node.implemented_both then
              OManager.serror "@[<v>%a@]@\n@[<4>  %s is tagged as 'both_implem' but it uses 'protected' values:@\n%a@]"
                pp_pos node
                (Ident.original_name node.ident)
                (pp_private_path pp_pos) node;
            false, true
          ) else
            match node.calls_client_bypass with
            | Some key ->
                if node.implemented_both then (
                  OManager.serror "@[<v>%a@]@\n@[<4>  %s is tagged as 'both_implem' but it uses the client bypass %s@]"
                    pp_pos node
                    (Ident.original_name node.ident)
                    (BslKey.to_string key)
                );
                true, false
            | None ->
                false, false in
        let on_the_server =
          if fake_server then
            let functional_type = has_functional_type env.gamma env.annotmap (get_expr node) in
            if not functional_type then
              OManager.serror "@[<v>%a@]@\n@[<2>  %s is tagged as 'both' but it contains a client bypass (%%%%%a%%%%) and it is not a function.@]"
                pp_pos node
                (Ident.original_name node.ident)
                BslKey.pp (Option.get node.calls_client_bypass);
            `alias
          else
            `expression in
        let on_the_client =
          let functional_type = has_functional_type env.gamma env.annotmap (get_expr node) in
          if fake_client then
            if functional_type then
              `alias
            else
              `insert_server_value
          else if node.implemented_both then
            `expression
          else if node.does_side_effects then
            `insert_server_value
          else
            (* not sure exactly what should happen when you have instantaneous deps, should possibly be a slicing error *)
            if direct_dep_on_the_server env node then
              if functional_type then
                `alias
              else
                `insert_server_value
            else
              `expression in
        if node.has_sliced_expr then (
          (match on_the_client with
           | `expression -> ()
           | `alias | `insert_server_value ->
               OManager.warning ~wclass:WClass.sliced_expr "@[<v>%a@]@\n@[<2>  %s contains a 'sliced_expr' but the client code will not be executed.@]"
                 pp_pos node
                 (Ident.original_name node.ident)
          );
          (match on_the_server with
           | `expression -> ()
           | `alias ->
               OManager.warning ~wclass:WClass.sliced_expr "@[<v>%a@]@\n@[<2>  %s contains a 'sliced_expr' but the server code will not be executed.@]"
                 pp_pos node
                 (Ident.original_name node.ident))
        );
        node.on_the_server <- Some (Some on_the_server);
        node.on_the_client <- Some (Some on_the_client);
        node.publish_on_the_server <- on_the_client = `alias;
        node.publish_on_the_client <- on_the_server = `alias
    | Some {wish=Prefer; side=Client} ->
        (* same check as for @client to be sure that we have no error and no warning *)
        if node.calls_private <> None || node.has_sliced_expr then
          aux node None
        else
          aux node (Some {wish=Force; side=Client})
    | Some {wish=Prefer; side=Server} ->
        (* same check as for @server *)
        if node.calls_client_bypass <> None || node.has_sliced_expr then
          aux node None
        else
          aux node (Some {wish=Force; side=Server})
    | Some {wish=Prefer; side=Both} ->
        (* not exactly the same check as for @both
         * the check must be stronger not to generate errors, but if the slicer
         * isn't forced to, it won't generated stupid code like @both would
         * (with `alias) *)
        if node.calls_private <> None || node.calls_client_bypass <> None then
          aux node None
        else
          (* FIXME: we can have warnings anyway with @sliced_expr and @insert_server_value because the check above is not enough *)
          aux node (Some {wish=Force; side=Both})
    | None ->
        (*
          if node.calls_private <> None then ( (* should have a different value for functions and not functions maybe ?
          * @publish max_int = ... should be insert_server_valued, but not @publish f() = ... *)
          aux node (Some {wish=Force; side=Server})
          ) else if node.calls_client_bypass <> None then (
          aux node (Some {wish=Force; side=Client})
          ) else if node.has_sliced_expr then (
          aux node (Some {wish=Force; side=Both})
          ) else (
        (* optimization: if a function needs functionalities present on one side only
          * then put the function only on this side. This way you switch side sooner, and you factorize
          * remote calls. This cannot increase the number of remote calls, and in the cases where it is not decreased
          * the side of the code decreases *)
          match node.needs_the_client, node.needs_the_server with
          | true, false ->
          aux node (Some {wish=Force; side=Client})
          | false, true ->
          aux node (Some {wish=Force; side=Server})
          | _ ->
          aux node (Some {wish=Force; side=Both})
          )*)
        if node.calls_private = None then(*|| node.privacy = Published then*)
          aux node (Some {wish=Force; side=Both})
        else
          aux node (Some {wish=Force; side=Server}) in
  aux node annot

(* to preserve the behaviour that we had before the early lambda lifting
 * by default, a function is sliced as if all local functions had not been lifted
 * IF it is not annotated
 * If it is annotated, it is treated as if the user had lambda lifted the code by hand *)
let node_is_annotated info =
  match info.privacy with
  | Visible -> (
      (* no @publish nor @server_private *)
      match info.user_annotation with
      | None -> (* no @client, @server, @both *) false
      | _ -> true
    )
  | _ -> true

let enclosing_info_if_not_toplevel_and_not_annotated env info =
  if info.lambda_lifted = [] || node_is_annotated info then None
  else (
    let orig =
      try
        (* a local function is sliced as the its innermost
         * enclosing function that is annotated
         * (or the toplevel one by default) *)
        List.find
          (fun ident ->
             let info = IdentTable.find env.informations ident in
             node_is_annotated info
          ) info.lambda_lifted
      with Not_found -> List.last info.lambda_lifted in
    let orig_info = IdentTable.find env.informations orig in
    Some orig_info
  )

let inline_informations_lambda_lifted env =
  IdentTable.iter
    (fun _ info ->
       match info.expr with
       | External _ -> ()
       | Local _ ->
           match enclosing_info_if_not_toplevel_and_not_annotated env info with
           | None -> ()
           | Some orig_info ->
               (* merging @sliced_expr, @call_*_bypass
                * because these are the only properties that would
                * be different if the the lifted functions were inlined
                * I think (they depend on the field expr) *)
               orig_info.has_sliced_expr <- orig_info.has_sliced_expr || info.has_sliced_expr;
               orig_info.calls_client_bypass <- (
                 match orig_info.calls_client_bypass with
                 | None -> info.calls_client_bypass
                 | Some _ as v -> v
               );
               orig_info.calls_server_bypass <- (
                 match orig_info.calls_server_bypass with
                 | None -> info.calls_server_bypass
                 | Some _ as v -> v
               );
               (* we add a dependency from the original to the lifted one
                * because if the local function is not used, then there is no dependency
                * (and the outer function will be put on both sides, so will the inner function
                * and if it is server private, resolveRemoteCalls will break)
                * example of such a problem if you remove this:
                * @server_private x = 1
                * g() =
                *   f() = x
                *   @fail
                *)
               G.add_edge env.call_graph orig_info info
    ) env.informations

let choose_sides env =
  let graph = env.call_graph in
  let groups = SCC.scc ~size:1000 graph in
  List.iter
    (fun group ->
       if List.exists is_external group then
         assert (match group with [_] -> true | _ -> false)
       else (
       (* first step: looking at who needs (transitively) the server or the client *)
         List.iter
           (fun node ->
              (* this value doesn't take into account all the recursive calls *)
              node.needs_the_server <- node.calls_server_bypass <> None || node.privacy = Private || G.exists_succ
                (fun node ->
                   node.needs_the_server ||
                     (match node.on_the_client, node.on_the_server with
                      | Some None, Some (Some b) ->
                          assert (b = `expression); (* if not on the client, must be on the server *)
                          true
                      | _ -> false)) graph node;
              node.needs_the_client <- node.calls_client_bypass <> None || G.exists_succ
                (fun node ->
                   node.needs_the_client ||
                     (match node.on_the_client, node.on_the_server with
                      | Some Some a, Some None ->
                          assert (a = `expression);
                          true
                      | _ -> false)) graph node;
           ) group;
         if List.exists (fun node -> node.needs_the_server) group then
           List.iter (fun node -> node.needs_the_server <- true) group;
         if List.exists (fun node -> node.needs_the_client) group then
           List.iter (fun node -> node.needs_the_client <- true) group;
         (* FIXME the value of needs_the_* is not correct when you have recursive bindings
          * with some but not all bindings annotated *)
         (* we should first look at annotated declarations, then compute this set
          * and then take of unannotated declaration *)

         (* second step (completely independent): complain if a sliced_expr calls someone private *)
         List.iter
           (fun node ->
              if node.calls_private <> None && node.has_sliced_expr then
                OManager.serror "@[<v>%a@]@\n@[<4>  This declaration contains a @@sliced_expr but it uses server private values:@\n%a@]"
                  pp_pos node
                  (pp_private_path pp_pos) node
           ) group;

         (* third step: dispatch according the annotation *)
         List.iter (fun node ->
                      match enclosing_info_if_not_toplevel_and_not_annotated env node with
                      | Some _ -> (* this is treated below *) ()
                      | None -> look_at_user_annotation env pp_pos node node.user_annotation
                   ) group
       )
    ) groups;

  List.iter
    (fun group ->
       List.iter
         (fun node ->
            match enclosing_info_if_not_toplevel_and_not_annotated env node with
            | Some node_i ->
                (* never publish those for now at least, because it adds type
                 * variables in unwanted places like the runtime of the serialization *)
                let relax = function
                  | None -> assert false
                  | Some (Some `expression)
                  | Some None as v -> v
                  | Some (Some `alias)
                  | Some (Some `insert_server_value) ->
                      (* avoids many useless insert_server_values
                       * should be solved cleanly when we have an actual slicing strategy for
                       * local functions *)
                      Some None in
                node.on_the_server <- relax (node_i.on_the_server :> client_code_kind option option);
                node.on_the_client <- relax node_i.on_the_client;
            | None -> ()
         ) group
    ) groups


(*------------------------------------*)
(*--------- ast utilities ------------*)
(*------------------------------------*)

(**
   Make a directive

   @param dir The directive constructor
*)
let make_dir ?annotmap_old ~inner dir annotmap e =
  let annotmap_old = Option.default annotmap annotmap_old in
  let full = QmlAnnotMap.find (Q.QAnnot.expr e) annotmap_old in
  let typ = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap_old in
  let a = Annot.next () in
  let annotmap = QmlAnnotMap.add a full annotmap in
  let label = Annot.make_label a (Q.Pos.expr e) in
  annotmap, Q.Directive (label, dir, (if inner then [e] else []), (if inner then [] else [typ]))

let directive_call = function
  | `comet_call  -> fun a e -> make_dir `comet_call ~inner:true a e
  | `ajax_call b -> fun a e -> make_dir (`ajax_call b) ~inner:true a e

let directive_publish ident dir annotmap expr =
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap in
  let pos = Q.Pos.expr expr in
  let annotmap, expr = QmlAstCons.TypedExpr.ident ~pos annotmap ident ty in
  match dir with
  | `comet_publish ->
      make_dir `comet_publish ~inner:true annotmap expr
  | `ajax_publish b ->
      make_dir (`ajax_publish b) ~inner:true annotmap expr

(* builds (fun x1 x2 ... -> @comet_call(client_name)(x1,x2,...))
   the type of client_name is refreshed so that ei can propagate type vars
   to the remote call but not to the original implementation
*)
let eta_expand comet_call_or_ajax_call ~gamma ~expr_for_annot ~annotmap_old ~annotmap ~tsc client_name =
  let arity = get_arity_of_functional_type gamma annotmap_old expr_for_annot in
  (*let tsc = QmlTypes.process_scheme gamma tsc in*)
  let annotmap, for_annot = QmlAstCons.TypedExpr.shallow_copy_new ~annotmap_old annotmap expr_for_annot in
  let e = Q.Ident (Q.Label.expr for_annot, client_name) in
  let annot = Q.QAnnot.expr e in
  let ty =
    match tsc with
    | None ->
        (* if the type is not polymorphic, we do not care about using the same typevars
         * because ei will not propagate anything in the first place *)
        QmlAnnotMap.find_ty annot annotmap
    | Some tsc ->
        let _quant, ty, () = QmlGenericScheme.export_unsafe tsc in
        ty in
  (*let ty = QmlAnnotMap.find_ty annot annotmap in
  let ty = QmlTypes.type_of_type gamma ty in*)
  let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
  let annotmap = QmlAnnotMap.remove_tsc annot annotmap in
  let annotmap, e = directive_call comet_call_or_ajax_call annotmap e in
  let annot = Q.QAnnot.expr e in
  (* don't forget to put the typescheme for ei *)
  let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
  let annotmap = QmlAnnotMap.add_tsc_inst_opt annot tsc annotmap in
  let idents = List.init arity
    (fun i ->
       let (ty, _) = QmlTypes.type_of_type gamma (QmlAstCons.Type.typevar (Q.TypeVar.next ())) in
       let ident = Ident.refresh ~map:(fun name -> name ^ "_eta_" ^ string_of_int i) client_name in
       ident,ty
    ) in
  let annotmap,exprs =
    List.fold_left_map
      (fun annotmap (ident,ty) ->
         QmlAstCons.TypedExpr.ident annotmap ident ty) annotmap idents in
  let annotmap, e = QmlAstCons.TypedExpr.apply gamma annotmap e exprs in
  let annotmap, e = QmlAstCons.TypedExpr.lambda annotmap idents e in
  (* don't forget to put the typescheme for ei *)
  let annot = Q.QAnnot.expr e in
  let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
  let annotmap = QmlAnnotMap.add_tsc_opt annot tsc annotmap in
  annotmap, e

(* renaming all the variables in addition to inserting directives everywhere
 * this renaming can't be easily done with QmlAlphaConv and renaming isn't really
 * hard to do on already renamed code, so we do it by hand *)
let insert_directives_expr
    ~infos
    ~(side:[`server | `client])
    ~rename
    ~rename_other
    ~tsc
    ~annotmap e =
  let rec aux tra annotmap e = (* need to go down, because we need to know if we are under a @fun_action *)
    match e with
    (* inserting remote calls *)
    | Q.Ident (label, j) -> (
        try
          let new_j = IdentMap.find j rename in
          let tsc_inst_opt = try IdentMap.find j tsc with Not_found -> None in
          let annotmap = QmlAnnotMap.add_tsc_inst_opt_label label tsc_inst_opt annotmap in
          annotmap, Q.Ident (label, new_j)
        with
        | Not_found ->
            try
              let new_j = IdentMap.find j rename_other in
              let e = Q.Ident (label, new_j) in
              let annotmap = QmlAnnotMap.remove_tsc_inst_label label annotmap in
              (* we are on the client and calling the server *)
              let call =
                match side with
                | `server -> `comet_call
                | `client ->
                    let info = IdentTable.find infos j in
                    let sync = variant_of_async info.async in
                    `ajax_call sync in
              let annotmap, e = directive_call call annotmap e in
              assert (IdentMap.mem j tsc);
              let tsc_inst_opt = IdentMap.find j tsc in
              let annotmap = QmlAnnotMap.add_tsc_inst_opt (Q.QAnnot.expr e) tsc_inst_opt annotmap in
              annotmap, e
            with Not_found ->
              annotmap, e
      )

    | Q.Directive (_, `sliced_expr, [client;server], []) -> (
        match side with
        | `client -> aux tra annotmap client
        | `server -> aux tra annotmap server
      )

    (* when we meet a `fun_action directive, the function identifier is always the one of the client *)
    | Q.Directive (label, (`fun_action None as a), [e'], b) -> (
        match side with
        | `client -> tra annotmap e (* nothing special, we are already on the client *)
        | `server ->
            let annotmap, e' =
              match e' with
              | Q.Apply (label_apply, Q.Ident (label_ident, i), el) ->
                  let annotmap, el = List.fold_left_map (aux tra) annotmap el in
                  let i = IdentMap.find i rename_other in
                  annotmap, Q.Apply (label_apply, Q.Ident (label_ident, i), el)

              | _ ->
                  (*
                    these expressions are created by lambda lifting and must have these forms
                  *)
                  assert false
            in

            let e = Q.Directive (label, a, [e'], b) in
            annotmap, e
      )

    | Q.Directive (_, `fun_action _, _, _) -> assert false

    (* nothing to do *)
    | _ -> tra annotmap e
  in

  QmlAstWalk.Expr.traverse_foldmap aux annotmap e

let is_present side info =
  let on_the_ =
    match side with
    | `server -> (info.on_the_server :> client_code_kind option option)
    | `client -> info.on_the_client in
  match on_the_ with
  | None -> assert false
  | Some None -> false
  | Some (Some _) -> true

let is_a_lambda info =
  let rec aux = function
      (* this check should be kept consistent with the one in qmlUncurry presumably *)
    | Q.Coerce (_, e, _)
    | Q.Directive (_, #Q.type_directive, [e], _) -> aux e
    | Q.Lambda _ -> true
    | _ -> false in
  match info.expr with
  | External _ -> false (* do not generate stubs for functions from other packages! *)
  | Local e -> aux e

(* if a lambda is not present on our side, we pretend that is it
 * just to be able to name the stub a(x) = @comet_call(a)(x)
 * this way, when we have:
 * @client f(x,y) = ... @typeof(x) ...
 * g = f
 * then, g will be an alias in both code which is necessary
 * or else the two codes have the same set of functions
 * but not the same set of closures (and you end up serializing non exiting functions)
 *)
let name_stub side info =
  is_a_lambda info && (
    match side with
    | `client -> info.publish_on_the_server
    | `server -> info.publish_on_the_client
  )

let split_code ~gamma:_ ~annotmap_old env code =
  let _chrono_insert = Chrono.make () in
  let _chrono_copy = Chrono.make () in
  let _chrono = Chrono.make () in

  let update_map_with_tsc ~side i info map =
    let expr =
      match info.expr with
      | Local e -> e
      | External _ -> assert false in
    let tsc_opt = Option.map QmlTypes.Scheme.refresh (QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr expr) env.annotmap) in
    (match side with
     | `client -> (
         match info.client_ident with
         | `undefined -> info.client_ident <- `tsc tsc_opt
         | `ident ident -> info.client_ident <- `ident_tsc (ident, tsc_opt)
         | `tsc _ | `ident_tsc _ -> assert false
       )
     | `server -> (
         match info.server_ident with
         | `undefined -> info.server_ident <- `tsc tsc_opt
         | `ident ident -> info.server_ident <- `ident_tsc (ident, tsc_opt)
         | `tsc _ | `ident_tsc _ -> assert false
       ));
    IdentMap.safe_add i tsc_opt map in
  let renaming_server, tsc_server =
    IdentTable.fold
      (fun i info (map,tsc_map) ->
         (* we check if we have a server ident before checking that [if_present `server info]
          * because we may be in the case where [name_stub_server is true] *)
         match info.server_ident with
         | `undefined ->
             if is_present `server info || name_stub `server info then (
               let ident = Ident.refresh i in
               info.server_ident <- `ident ident;
               let tsc_map =
                 if info.publish_on_the_client then update_map_with_tsc ~side:`server i info tsc_map
                 else tsc_map in
               (IdentMap.safe_add i ident map, tsc_map)
             ) else
               (map, update_map_with_tsc ~side:`server i info tsc_map)
         | `tsc tsc_opt ->
             (map, IdentMap.safe_add i tsc_opt tsc_map)
         | `ident ident ->
             (IdentMap.safe_add i ident map, tsc_map)
         | `ident_tsc (ident, tsc_opt) ->
             (IdentMap.safe_add i ident map, IdentMap.safe_add i tsc_opt tsc_map)
      ) env.informations (IdentMap.empty, IdentMap.empty) in
  let renaming_client, tsc_client =
    IdentTable.fold
      (fun i info (map,tsc_map) ->
         match info.client_ident with
         | `undefined ->
             if is_present `client info || name_stub `client info then (
               let ident = Ident.refresh i in
               info.client_ident <- `ident ident;
               let tsc_map =
                 if info.publish_on_the_server then update_map_with_tsc ~side:`client i info tsc_map
                 else tsc_map in
               (IdentMap.safe_add i ident map, tsc_map)
             ) else
               (map, update_map_with_tsc ~side:`client i info tsc_map)
         | `tsc tsc_opt ->
             (map, IdentMap.safe_add i tsc_opt tsc_map)
         | `ident ident ->
             (IdentMap.safe_add i ident map, tsc_map)
         | `ident_tsc (ident, tsc_opt) ->
             (IdentMap.safe_add i ident map, IdentMap.safe_add i tsc_opt tsc_map)
      ) env.informations (IdentMap.empty,IdentMap.empty) in
  let rename_server i = IdentMap.find i renaming_server in
  let rename_client i = IdentMap.find i renaming_client in
  let alpha_conv_server = QmlAlphaConv.create_from_maps ~map:renaming_server ~revmap:IdentMap.empty in
  let renaming_map_server = QmlRenamingMap.from_map renaming_server in
  let renaming_map_client = QmlRenamingMap.from_map renaming_client in
  let find_server_name name = IdentMap.find_opt name renaming_server in
  let find_client_name name = IdentMap.find_opt name renaming_client in

  let insert_server annotmap e =
    insert_directives_expr
      ~infos:env.informations
      ~side:`server
      ~rename:renaming_server ~rename_other:renaming_client
      ~tsc:tsc_server
      ~annotmap e in
  let insert_client annotmap e =
    insert_directives_expr
      ~infos:env.informations
      ~side:`client
      ~rename:renaming_client ~rename_other:renaming_server
      ~tsc:tsc_client
      ~annotmap e in

  let annotmap,rev_code_client,rev_code_server,publish_rev_code_client,publish_rev_code_server,client_published,server_published =
    List.fold_left
      (fun (annotmap,rev_code_client,rev_code_server,publish_rev_code_client,publish_rev_code_server,client_publish,server_publish) code_elt ->
         match code_elt with
         | Q.NewVal (label,iel)
         | Q.NewValRec (label,iel) ->
             let rebuild =
               match code_elt with
               | Q.NewVal _ -> (fun x -> Q.NewVal (label, x))
               | Q.NewValRec _ -> (fun x -> Q.NewValRec (label, x))
               | _ -> assert false in
             let annotmap,more_server =
               List.fold_left_filter_map
                 (fun annotmap (i,e) ->
                    match IdentTable.find env.informations i with
                    | {on_the_server=Some (Some `expression); _} ->
                        #<If:SLICER_TIME> _chrono_copy.Chrono.start () #<End>;
                        let annotmap, e = QmlAstCons.TypedExpr.copy_new ~annotmap_old annotmap e in
                        #<If:SLICER_TIME> _chrono_copy.Chrono.stop () #<End>;
                        #<If:SLICER_TIME> _chrono_insert.Chrono.start () #<End>;
                        let annotmap, e = insert_server annotmap e in
                        #<If:SLICER_TIME> _chrono_insert.Chrono.stop () #<End>;
                        annotmap, Some (rename_server i, e)
                    | {on_the_server=Some (Some `alias | None); _} -> (
                        try
                          let server_name = rename_server i in
                          let client_name = rename_client i in
                          (* need to take the tsc last, because sometimes find will fail (on dbgen inserted idents)
                           * but in this cas, rename_server or rename_client would have failed earlier
                           * (because these idents are server private anyway) *)
                          assert (IdentMap.mem i tsc_server);
                          let tsc = IdentMap.find i tsc_server in
                          let annotmap, e =
                            eta_expand `comet_call ~gamma:env.gamma ~expr_for_annot:e ~annotmap_old ~annotmap ~tsc client_name in
                          annotmap, Some (server_name, e)
                        with Not_found ->
                          annotmap, None
                      )
                    | {on_the_server=None; _} -> assert false)
                 annotmap iel in
             let annotmap,more_client =
               List.fold_left_filter_map
                 (fun annotmap (i,e) ->
                    match IdentTable.find env.informations i with
                    | {on_the_client=Some (Some `expression); _} ->
                        #<If:SLICER_TIME> _chrono_copy.Chrono.start () #<End>;
                        let annotmap, e = QmlAstCons.TypedExpr.copy_new ~annotmap_old annotmap e in
                        #<If:SLICER_TIME> _chrono_copy.Chrono.stop () #<End>;
                        #<If:SLICER_TIME> _chrono_insert.Chrono.start () #<End>;
                        let annotmap, e = insert_client annotmap e in
                        #<If:SLICER_TIME> _chrono_insert.Chrono.stop () #<End>;
                        annotmap, Some (rename_client i,e)
                    | {on_the_client=Some (Some `insert_server_value); _} ->
                        let annotmap, e = make_dir ~annotmap_old ~inner:false (`insert_server_value (rename_server i)) annotmap e in
                        annotmap, Some (rename_client i,e)
                    | {on_the_client=Some (Some `alias | None); _} -> (
                        try
                          let client_name = rename_client i in
                          let server_name = rename_server i in
                          assert (IdentMap.mem i tsc_client);
                          let tsc = IdentMap.find i tsc_client in
                          let info = IdentTable.find env.informations i in
                          let sync = variant_of_async info.async in
                          let annotmap, e =
                            eta_expand (`ajax_call sync) ~gamma:env.gamma ~expr_for_annot:e ~annotmap_old ~annotmap ~tsc server_name in
                          annotmap, Some (client_name, e)
                        with Not_found ->
                          annotmap, None
                      )
                    | {on_the_client=None; _} -> assert false)
                 annotmap iel in
             let rev_code_server = if more_server = [] then rev_code_server else rebuild more_server :: rev_code_server in
             let rev_code_client = if more_client = [] then rev_code_client else rebuild more_client :: rev_code_client in
             (* FIXME: enough duplication! *)
             let annotmap, publish_rev_code_client, client_publish =
               List.fold_left
                 (fun ((annotmap, rev_code_client, client_publish) as acc) (i,_e) ->
                    let info = IdentTable.find env.informations i in
                    if info.publish_on_the_client then
                      let new_i = rename_client i in
                      let e = snd (List.find (fun (j,_) -> Ident.equal new_i j) more_client) in
                      let client_publish = IdentMap.add new_i None client_publish in
                      let annotmap, e = directive_publish new_i `comet_publish annotmap e in
                      let label = Annot.nolabel "QmlSimpleSlicer.rev_code_client" in
                      (annotmap, Q.NewVal (label, [Ident.refresh ~map:(fun s -> "skel_"^s) new_i, e]) :: rev_code_client, client_publish)
                    else acc
                 ) (annotmap, publish_rev_code_client, client_publish) iel in
             let annotmap, publish_rev_code_server, server_publish =
               List.fold_left
                 (fun ((annotmap, rev_code_server, server_publish) as acc) (i,_e) ->
                    let info = IdentTable.find env.informations i in
                    if info.publish_on_the_server then
                      let new_i = rename_server i in
                      let e = snd (List.find (fun (j,_) -> Ident.equal new_i j) more_server) in
                      let server_publish = IdentMap.add new_i None server_publish in
                      let sync = variant_of_async info.async in
                      let annotmap, e = directive_publish new_i (`ajax_publish sync) annotmap e in
                      let label = Annot.nolabel "QmlSimpleSlicer.rev_code_server" in
                      (annotmap, Q.NewVal (label, [Ident.refresh ~map:(fun s -> "skel_"^s) new_i, e]) :: rev_code_server, server_publish)
                    else acc
                 ) (annotmap, publish_rev_code_server, server_publish) iel in
             (annotmap,rev_code_client,rev_code_server,publish_rev_code_client,publish_rev_code_server,client_publish,server_publish)
         | _ -> assert false)
      (QmlAnnotMap.empty,[],[],[],[],IdentMap.empty,IdentMap.empty) code in
  #<If:SLICER_TIME> _chrono.Chrono.start () #<End>;
  let client = {
    code = List.rev (publish_rev_code_client @ rev_code_client);
    published = client_published;
    renaming = renaming_map_client;
  } in
  let server = {
    code = List.rev (publish_rev_code_server @ rev_code_server);
    published = server_published;
    renaming = renaming_map_server;
  } in
  let res =
    client,
    server,
    find_client_name,
    find_server_name,
    alpha_conv_server,
    annotmap in
  #<If:SLICER_TIME>
    let conv = _chrono.Chrono.read () in
    let copy = _chrono_copy.Chrono.read () in
    let insert = _chrono_insert.Chrono.read () in
    Printf.printf "  copy:%fs\n  insert:%fs\n  conv:%fs\n" copy insert conv
  #<End>;
  res

let update_gamma ~rename_server ~rename_client gamma =
  QmlTypes.Env.Ident.fold
    (fun ident tsc new_gamma ->
       let new_gamma =
         match rename_server ident with
         | None -> new_gamma
         | Some server_ident -> QmlTypes.Env.Ident.add server_ident tsc new_gamma in
       let new_gamma =
         match rename_client ident with
         | None -> new_gamma
         | Some client_ident -> QmlTypes.Env.Ident.add client_ident tsc new_gamma in
       QmlTypes.Env.Ident.remove ident new_gamma) gamma gamma

let update_typer_env ~alpha_conv_server ~rename_server ~rename_client ~typer_env ~annotmap =
  let _chrono = Chrono.make () in
  (* updating ident -> tsc map with renamed (and duplicated) identifiers *)
  let gamma = typer_env.QmlTypes.gamma in
  #<If:SLICER_TIME> _chrono.Chrono.start () #<End>;
  let gamma = update_gamma ~rename_server ~rename_client gamma in
  (* updating the db schema with renamed Expression and new annots *)
  #<If:SLICER_TIME> Printf.printf "  gamma: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  let schema = typer_env.QmlTypes.schema in
  let _ = alpha_conv_server in
  (* FIXME: won't work if dbgen goes after slicing, but in the meantime this is very slow
  let annotmap,schema = QmlDbGen.Schema.foldmap_expr
    (fun annotmap e ->
       let annotmap,e = QmlAstCons.TypedExpr.copy_new ~annotmap_old:typer_env.QmlTypes.annotmap annotmap e in
       let e_renamed = QmlAlphaConv.expr alpha_conv_server e in
       annotmap, e_renamed
    ) annotmap schema in*)
  #<If:SLICER_TIME> Printf.printf "  schema: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  {typer_env with
     QmlTypes.gamma = gamma;
     QmlTypes.annotmap = annotmap;
     QmlTypes.schema = schema}

let pp_constraint_ f c =
  Format.pp_print_string f (
    match c with
    | `expression -> "Expression"
    | `alias -> "Eta expansion"
    | `insert_server_value -> "Insert_server_value"
  )
let show_annotations env =
  let client, server_visible, server_not_visible =
    IdentTable.fold
      (fun i info (client, server_visible, server_not_visible) ->
         let client =
           match info.on_the_client with
           | Some (Some v) -> IdentMap.add i v client
           | _ -> client in
         let server_visible, server_not_visible =
           match info.on_the_server with
           | None -> assert false
           | Some None -> server_visible, server_not_visible
           | Some (Some v) ->
               if info.publish_on_the_server then
                 IdentSet.add i server_visible, server_not_visible
               else
                 server_visible, IdentMap.add i v server_not_visible in
         (client, server_visible, server_not_visible)
      ) env.informations (IdentMap.empty,IdentSet.empty,IdentMap.empty) in
  let show_set set =
    let elts = IdentSet.elements set in
    let names = List.map Ident.original_name elts in
    let sorted_names = List.sort compare names in
    List.iter (Format.printf "  %s\n") sorted_names in
  let show_map p map =
    let l = IdentMap.to_list map in
    let l = List.map (fun (a,b) -> (Ident.original_name a,b)) l in
    let l = List.sort compare l in
    List.iter (fun (a,b) -> Format.printf "  %s->%a\n" a p b) l in
  Format.printf "Server private:\n"; show_map pp_constraint_ server_not_visible;
  Format.printf "Server public:\n"; show_set server_visible;
  Format.printf "Client:\n"; show_map pp_constraint_ client;
  Format.printf "%!"

let whole_check env code =
  let check_binding (i,_) =
     let node = IdentTable.find env.informations i in
     let _ = check_node ~emit:true node in
     ()
  in
  QmlAstWalk.Code.iter_binding check_binding code

let dump_annotations env code =
  match ObjectFiles.compilation_mode () with
  | `init -> ()
  | `compilation | `linking | `prelude ->
  let filename =
    Filename.concat (ObjectFiles.get_compilation_directory_or_current_dir ()) "slicer.dump" in
  let channel = open_out filename in
  let f = Format.formatter_of_out_channel channel in
  QmlAstWalk.Code.iter_binding (fun (i,_) ->
    let info = IdentTable.find env.informations i in
    let both = match info with
      | {on_the_server = Some (Some `expression); on_the_client = Some (Some `expression); _} -> true
      | _ -> false
    in
    let fprintf form = Format.fprintf f form in
    (* if boring (i.e. both) we print nothing *)
    (if both then () else
      fprintf "@[<v>%a@]@\n@[<2>  %s is %s@]@\n"
      pp_pos info
      (Ident.original_name i)
      (match info with
       | {on_the_server = None; _}
       | {on_the_client = None; _}
       | {on_the_server = Some None; on_the_client = Some None; _} -> assert false
       | {on_the_server = Some None; on_the_client = Some (Some k); _} ->
           (match k with
            | `expression -> "client only"
            | `insert_server_value | `alias -> assert false)
       | {on_the_server = Some (Some k); on_the_client = Some None; _} ->
           (match k with
            | `expression -> "server only"
            | `alias -> assert false)
       | {on_the_server = Some (Some s); on_the_client = Some (Some c); _} ->
           match s, c with
           | `alias, (`alias | `insert_server_value) -> assert false
           | `alias, `expression -> "server(alias) and client"
           | `expression, `expression -> "server and client"
           | `expression, `alias -> "server and client(alias)"
           | `expression, `insert_server_value -> "server and client(insert_server_value)")
      (* TODO: reimplement the printing of the distant calls *)
      (*
        name , location , ...
        [name , side , ...]
        [name , server_api]
        [name , client_api]
        [name , remote_call , ...]
        [name , private_call, ...]
      *)
    );
    let (|>) a f = f a in
    let side_of info =  match Option.get(info.on_the_server),Option.get(info.on_the_client) with
      | Some `expression, Some `expression -> "both"
      | _ , Some `expression  -> "client"
      | Some `expression, None -> "server"
      | _ -> "unknown"
    in
    let name i =
      Printf.sprintf "%s[%s]"
        (Ident.original_name i)
        (Ident.get_package_name i)
    in
    let side = side_of info in
    let remote_call =
      G.succ env.call_graph info
      |> List.filter (fun (i:information) ->
        let side_of_i = side_of i in side_of_i <> side && side_of_i <> "both"
        )
      |> List.map (fun i->name i.ident)
    in
    if side<>"both" || remote_call <> [] then (
      let get_bypass o = match o with
        | None -> []
        | Some bp -> [Format.sprintf "%a" BslKey.pp bp]
      in
      let get_private = match info.calls_private with
        | Some (Local infobis) when not(Ident.equal infobis.ident info.ident)-> [name infobis.ident]
        | Some (External _) -> ["EXTERNAL"]
        | _ -> []
      in
      let private_call = get_private
                       @ (get_bypass info.calls_client_bypass)
                       @ (get_bypass info.calls_server_bypass)
      in
      let name =  name info.ident in
      fprintf "%s , location , %a\n" name pp_pos info;
      if side<>"both" then fprintf "%s , side , %s\n" name side;
      if info.publish_on_the_server then  fprintf "%s , server_api\n" name;
      if info.publish_on_the_client then  fprintf "%s , client_api\n" name;
      if remote_call <> [] then
      fprintf "%s , remote_call , %a\n" name (Format.pp_list " " Format.pp_print_string) remote_call;
      if private_call <> [] then
      fprintf "%s , private_call, %a\n" name (Format.pp_list " " Format.pp_print_string) private_call;
      ()
      )
  ) code;
  Format.pp_print_flush f ();
  close_out channel

module S =
struct
  type t = information list
  let pass = "pass_Slicing"
  let pp f l =
    Format.fprintf f "@[<v>%a@]" (Format.pp_list "@ " pp_info) l
end

module R =
struct
  include ObjectFiles.Make(S)
  let save env =
    let current_package = ObjectFiles.get_current_package () in
    let externalize_info info =
      info.expr <- External current_package;
      match info.calls_private with
      | None -> ()
      | Some (Local _) -> info.calls_private <- Some (External current_package)
      | Some (External _) -> assert false in
    let t = IdentTable.fold
      (fun _ident info acc ->
         match info.expr with
         | External _ -> acc
         | Local _ -> externalize_info info; info :: acc)
      env.informations [] in
    save t
  let refresh_opt _side _info package = function
    | None -> None
    | Some tsc ->
        let tsc2 = QmlRefresh.refresh_typevars_from_tsc package tsc in
        (*Format.printf "@[<2>REFRESH SLICER %s %s: %a -> %a@]@." _side (Ident.to_string _info.ident) QmlPrint.pp_base#tsc tsc QmlPrint.pp_base#tsc tsc2;*)
        Some tsc2
  let load env =
    let informations = env.informations in
    let call_graph = env.call_graph in
    iter_with_name
      (fun package infos ->
         List.iter
           (fun info ->
              (* BEWARE: do not modify this info, or else you screw the value memoized in objectFiles *)
              (* damned, cannot simply copy a record *)
              let info = {info with ident = info.ident} in
              (match info.server_ident with
               | `ident _ -> ()
               | `undefined -> assert false
               | `tsc tsc_opt -> info.server_ident <- `tsc (refresh_opt "SERVER" info package tsc_opt)
               | `ident_tsc (ident, tsc_opt) -> info.server_ident <- `ident_tsc (ident, refresh_opt "SERVER" info package tsc_opt));
              (match info.client_ident with
               | `ident _ -> ()
               | `undefined -> assert false
               | `tsc tsc_opt -> info.client_ident <- `tsc (refresh_opt "CLIENT" info package tsc_opt)
               | `ident_tsc (ident, tsc_opt) -> info.client_ident <- `ident_tsc (ident, refresh_opt "CLIENT" info package tsc_opt));
              IdentTable.add informations info.ident info;
              G.add_vertex call_graph info;
           ) infos
      )
end

let process_code ~test_mode ~dump ~typer_env ~stdlib_gamma ~bymap ~code =
  let env = empty_env bymap typer_env in
  let _chrono = Chrono.make () in
  #<If:SLICER_TIME> _chrono.Chrono.start () #<End>;
  R.load env;
  #<If:SLICER_TIME> Printf.printf "load_env: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  let env, code = initialize_env ~env code in
  #<If:SLICER_TIME> Printf.printf "initialize_env: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  inline_informations_lambda_lifted env;
  #<If:SLICER_TIME> Printf.printf "inline_informations_lambda_lifted: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  propagate_server_private env;
  #<If:SLICER_TIME> Printf.printf "propagate_server_private: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  analyse_side_effects env code;
  #<If:SLICER_TIME> Printf.printf "analyse_side_effects: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  choose_sides env;
  #<If:SLICER_TIME> Printf.printf "choose_sides: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  whole_check env code;
  if dump then (
    dump_annotations env code
  );
  if test_mode then (
    OManager.flush_errors (); (* not to dump the annotations when an error happened, for compatibility with the previous version of the slicer *)
    show_annotations env;
    exit 0
  );
  let client,
      server,
      rename_client,
      rename_server,
      alpha_conv_server,
      annotmap =
    split_code ~gamma:env.gamma ~annotmap_old:env.annotmap env code in
  #<If:SLICER_TIME> Printf.printf "split_code: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  R.save env;
  let typer_env = update_typer_env ~alpha_conv_server ~rename_server ~rename_client ~typer_env ~annotmap in
  let stdlib_gamma = update_gamma ~rename_server ~rename_client stdlib_gamma in
  #<If:SLICER_TIME> Printf.printf "update_typer_env: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  stdlib_gamma, typer_env,client,server
