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
(* depends *)
module Format = BaseFormat
module List = BaseList

(* shorthands *)
module Q = QmlAst

(* *)

(*
  Separation.
  We store all the identifiers containing a corresponding coding directive.
*)

type version = string

type deprecated_argument =
  | Use of string
  | Hint of string

let pp_deprecated_argument fmt = function
  | Use use -> Format.fprintf fmt "Use @{<bright>%s@} instead" use
  | Hint hint -> Format.pp_print_string fmt hint

type deprecated_infos = {
  version : string option ; (* Optional version since which the construction is deprecated *)
  argument : deprecated_argument ;
  pos : FilePos.pos ;
}

type todo_infos = FilePos.pos

type env = {
  deprecated : deprecated_infos IdentMap.t ;
  todo : todo_infos IdentMap.t ;
}

module S =
struct
  type t = env
  let pass = "pass_CodingDirectives"

  let pp_map pp fmt map =
    let iter ident info =
      Format.fprintf fmt "%s : %a@\n" (Ident.original_name ident) pp info
    in
    IdentMap.iter iter map

  let pp_argument fmt = function
    | Use s -> Format.fprintf fmt "use=%S" s
    | Hint s -> Format.fprintf fmt "hint=%S" s

  let pp_deprecated fmt infos =
    Format.fprintf fmt "%a, %a" FilePos.pp infos.pos pp_argument infos.argument

  let pp_todo = FilePos.pp

  let pp fmt env =
    Format.fprintf fmt "DEPRECATED@\n@[<2>" ;
    pp_map pp_deprecated fmt env.deprecated ;
    Format.fprintf fmt "@]TODO@\n@[<2>" ;
    pp_map pp_todo fmt env.todo ;
    Format.fprintf fmt "@]";
    ()
end

module R = ObjectFiles.Make(S)

(*
  Warnings
*)
let wclass =
  let doc = "Coding directives warnings" in
  let name = "coding" in
  WarningClass.create ~name ~doc ~err:false ~enable:true ()

let wdeprecated =
  let doc = "deprecated constructions" in
  let name = "deprecated" in
  WarningClass.create ~parent:wclass ~name ~doc ~err:false ~enable:true ()

let wtodo =
  let doc = "unimplemented" in
  let name = "todo" in
  WarningClass.create ~parent:wclass ~name ~doc ~err:false ~enable:true ()

let warning_set =
  WarningClass.Set.create_from_list [
    wclass ;
    wdeprecated ;
    wtodo ;
  ]

(*
  Code processing
*)

type 'infos packaged_infos = {
  infos : 'infos ;
  package : ObjectFiles.package ;
}

let process_code _gamma annotmap code =

  (* structure to store everything, previous and current packages *)
  let deprecated : deprecated_infos packaged_infos IdentTable.t = IdentTable.create 64 in
  let todo : todo_infos packaged_infos IdentTable.t = IdentTable.create 64 in

  (* structure to store only the current package *)
  let this_deprecated  : deprecated_infos IdentTable.t = IdentTable.create 64 in
  let this_todo : todo_infos IdentTable.t = IdentTable.create 64 in

  (* separation : load *)
  let iter package env =
    let iter table ident infos =
      let p_infos = {
        infos ;
        package ;
      } in
      IdentTable.add table ident p_infos
    in
    let table table = IdentMap.iter (iter table) in
    table deprecated env.deprecated ;
    table todo env.todo ;
    ()
  in
  R.iter_with_name ~packages:true ~deep:false iter ;

  (* process the code *)
  (* 1/2 : collect directives, and remove them *)
  let annotmap, code =
    let package = ObjectFiles.get_current_package () in
    let foldmap_expr ident annotmap expr =
      let foldmap annotmap = function
        | Q.Directive (label, `deprecated, args, _) -> (
            let bad_args ?(label=label) () =
              let context = QmlError.Context.label label in
              QmlError.error context (
                "Invalid arguments for directive @{<bright>@@deprecated@}@\n"^^
                "@[<2>@{<bright>Hint@}:@\n"^^
                "argument should be of type @{<bright>%s@},@ and "^^
                "strings arguments should be literals only@]"
              )
                Opacapi.Types.Deprecated.argument
            in
            match args with
            | [ hint ; sub_expr ] -> (
                let (argument, version) =
                  (* WARNING! This is super fragile... watch the order of fields *)
                  match hint with
                  | Q.Record (_, [ "use", Q.Const (_, Q.String use)]) ->
                    (Use use, None)
                  | Q.Record (_, [ "hint", Q.Const (_, Q.String hint)]) ->
                    (Hint hint, None)
                  | Q.Record (_, [ ("use", Q.Const (_, Q.String use)); ("version", Q.Const (_, Q.String version))])
                  | Q.Record (_, [ ("version", Q.Const (_, Q.String version)); ("use", Q.Const (_, Q.String use))]) ->
                    (Use use, Some version)
                  | Q.Record (_, [ ("hint", Q.Const (_, Q.String hint)); ("version", Q.Const (_, Q.String version))])
                  | Q.Record (_, [ ("version", Q.Const (_, Q.String version)); ("hint", Q.Const (_, Q.String hint))]) ->
                    (Hint hint, Some version)
                  | _ ->
                      let label = QmlAst.Label.expr hint in
                      bad_args ~label ()
                in
                let pos = QmlAst.Pos.expr expr in
                let infos = {
                  argument ;
                  version ;
                  pos ;
                } in
                let pinfos = {
                  infos ;
                  package ;
                } in
                let () =
                  IdentTable.add deprecated ident pinfos ;
                  IdentTable.add this_deprecated ident infos ;
                in
                (*
                  Support for EI:
                  get the tsc of the @deprecated directive,
                  and put it on the sub expression
                *)
                let annotmap =
                  let sub_label = QmlAst.Label.expr sub_expr in
                  let tsc_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
                  let annotmap = QmlAnnotMap.add_tsc_opt_label sub_label tsc_opt annotmap in
                  annotmap
                in
                annotmap, sub_expr
              )
            | _ ->
                (*
                  QmlDirective ensure the correct typing of this directive
                *)
                assert false
          )
        | Q.Directive (label, `todo, args, tys) ->
            let infos = QmlAst.Pos.expr expr in
            let pinfos = {
              infos ;
              package ;
            } in
            let () =
              IdentTable.add todo ident pinfos ;
              IdentTable.add this_todo ident infos ;
            in
            let expr = Q.Directive (label, `fail, args, tys) in
            annotmap, expr

        | expr -> annotmap, expr
      in
      QmlAstWalk.Expr.foldmap foldmap annotmap expr
    in
    let code_elt annotmap elt =
      let val_ ~rec_ label vals =
        let foldmap annotmap ((ident, expr) as tuple) =
          let annotmap, fexpr = foldmap_expr ident annotmap expr in
          annotmap,
          if expr == fexpr
          then
            tuple
          else
            (ident, fexpr)
        in
        let annotmap, fvals = List.fold_left_map_stable foldmap annotmap vals in
        annotmap,
        if vals == fvals
        then
          elt
        else
          if rec_
          then
            Q.NewValRec (label, fvals)
          else
            Q.NewVal (label, fvals)
      in
      match elt with
      | Q.NewVal (label, vals) -> val_ ~rec_:false label vals
      | Q.NewValRec (label, vals) -> val_ ~rec_:true label vals

      (*
        Coding directive are not authorized on db values
      *)
      | Q.NewDbValue (_, dbdef) ->
          let fmap () = function
            | Q.Directive (label, ((`deprecated | `todo) as variant), _, _) ->
                let context = QmlError.Context.label label in
                QmlError.error context (
                  "The directive @{<bright>%s@} is not authorized in a database definition."
                )
                  (QmlPrint.directive variant)
            | expr -> (), expr
          in
          let (), _ = QmlAst.Db.foldmap_expr fmap () dbdef in
          annotmap, elt

      (*
        Other declaration are kept as is
      *)
      | elt -> annotmap, elt
    in
    List.fold_left_map_stable code_elt annotmap code
  in

  (* 2/2 : produces warnings *)
  let () =
    let iter (toplevel_ident, expr) =
      let iter tra = function
        | Q.Directive (_, `module_, _, _) ->
            (* This is for avoiding a warning in case of a module explosion *)
            ()
        | Q.Ident (label, ident) ->
            (* deprecated *)
            Return.set_checkpoint (
              fun break ->
                let { infos ; package } =
                  try
                    IdentTable.find deprecated ident
                  with Not_found -> Return.return break ()
                in
                let pos = Annot.pos label in
                let context = QmlError.Context.pos pos in
                QmlError.warning ~wclass:wdeprecated context (
                  "This is a @{<bright>deprecated@} construction%s, see annotation in:@\n"^^
                    "Package:%s,@\n"^^
                    "%a@\n"^^
                      "@[<2>@{<bright>Hint@}:@\n"^^
                    "%a@]"
                )
                  (match infos.version with None -> "" | Some version -> " (since version " ^ version ^ ")")
                  (fst package)
                  FilePos.pp infos.pos
                  pp_deprecated_argument infos.argument
            );
            (* todo *)
            Return.set_checkpoint (
              fun break ->
                let { infos ; package } =
                  try
                    IdentTable.find todo ident
                  with Not_found -> Return.return break ()
                in
                let pos = Annot.pos label in
                let context = QmlError.Context.pos pos in
                QmlError.warning ~wclass:wtodo context (
                  "This construction is @{<bright>not implemented@}, see annotation in:@\n"^^
                    "Package:%s,@\n"^^
                    "%a@\n"^^
                    "This will fail at runtime."
                )
                  (fst package)
                  FilePos.pp infos
            )

        | e -> tra e
      in
      (*
        Contamination rule:
        a deprecated function is authorized to use a deprecated construction
      *)
      if not (IdentTable.mem deprecated toplevel_ident)
      then
        QmlAstWalk.Expr.traverse_iter iter expr
    in
    List.iter (QmlAstWalk.Top.iter_name_expr iter) code
  in

  (* separation : store *)
  let () =
    let env =
      let map table =
        IdentTable.fold IdentMap.add table IdentMap.empty
      in
      let deprecated = map this_deprecated in
      let todo = map this_todo in
      {
        deprecated ;
        todo ;
      }
    in
    R.save env
  in

  annotmap, code
