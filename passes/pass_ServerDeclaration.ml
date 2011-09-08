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
module P = SurfaceAstPassesTypes
module S = SurfaceAst
module CS = SurfaceAstCons.StringCons
module C = SurfaceAstCons.ExprIdentCons
module List = Base.List

(* could be put after renaming and then the toplevel map would give the answer
 * without having to go throught all the toplevel names *)
let server_appears code =
  OpaWalk.CodeTopPattern.exists_nonrec (OpaWalk.Pattern.appears_str "server") code

module S1 =
struct
  type t = bool (* no_server *)
  let pass = "check_server_entry_point"
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module R1 = ObjectFiles.Make(S1)

let pass_check_server_entry_point ~options env =
  let warn, no_server, env =
    match options.OpaEnv.no_server with
    | Some no_server -> false, no_server, env
    | None ->
        begin
          (* check stdlib *)
          if server_appears env.P.lcodeNotUser then (
            OManager.error "I found a @{<bright>server@} declaration at toplevel on the @{<bright>stdlib@}@\n"
          );

          (* walk throught the code and replace server declarations *)
          let make_entry_point ~label e =
            let entry_point_dir = CS.D.server_entry_point ~label e in
            CS.C.newval_ignore ~label entry_point_dir in
          let fun_fl (has_server, acc) = function
            | S.NewVal ([(S.PatVar {S.ident="server";_}, _), e],_), label ->
                (* [server = $e$] becomes [_ = add_server(e)] *)
            (* this special case makes the code a little more readable *)
                (true, make_entry_point ~label e :: acc)
            | S.NewVal (pel,b), label ->
                (* [(server,mlk) = $e$] becomes [(fresh,mlk) = $e$ _ = add_server(fresh)]
                 * [(server,server) = e] won't generate the error "non linear pattern" *)
                let gen () = SurfaceAstCons.Fresh.name "server" in
                let new_names, pel =
                  List.fold_left_map
                    (fun acc (p,e) ->
                       let acc, p =
                         OpaWalk.Pattern.foldmap
                           (fun acc -> function
                            | (S.PatVar ({S.ident="server";_} as id),label) ->
                                let fresh = gen () in
                                let id = {id with S.ident=gen()} in
                                fresh :: acc, (S.PatVar id, label)
                            | (S.PatAs (p,({S.ident="server";_} as id)), label) ->
                                let fresh = gen () in
                                let id = {id with S.ident=gen()} in
                                fresh :: acc, (S.PatAs (p, id), label)
                            | p -> acc, p)
                           acc p in
                       acc, (p,e))
                    [] pel in
                let new_declarations =
                  List.rev_map (fun name -> make_entry_point ~label (CS.E.var ~label name)) new_names in
                (has_server || new_names <> [], List.rev_append new_declarations ((S.NewVal (pel,b),label) :: acc))
            | v -> (has_server, v :: acc) in
          let has_server,rev_code_parcouru = List.fold_left fun_fl (false,[]) env.P.lcodeUser  in
          let code_parcouru = List.rev rev_code_parcouru in

          if has_server then
            true, false, {env with P.lcodeUser = code_parcouru}
          else
            true, true, env
        end in
  R1.save no_server;
  let no_server =
    match ObjectFiles.compilation_mode () with
    | `init ->
        true
    | `linking ->
        (* no server if no packages defines a server *)
        let no_server = R1.fold ~packages:true ~deep:true (&&) no_server in
        if no_server && warn then OManager.unquiet "@{<red>WARNING : The \"server\" value is MISSING, the executable will NOT start a server@}";
        no_server
    | `prelude -> no_server
    | `compilation -> no_server in
  {options with OpaEnv.no_server = Some no_server}, env

let pass_resolve_server_entry_point ~options lcode =
  if options.OpaEnv.no_server = Some true then
    lcode
  else
    let aux ((e,label) as v)  =
      match e with
      | S.Directive (`server_entry_point, [apply_service], _) ->
          SurfaceAstCons.with_label label (fun () ->
            let id_addser = OpaMapToIdent.val_ Opacapi.Server_private.add_service in
            let add_ser = C.E.ident id_addser in
            C.E.apply add_ser apply_service
          )
      | _ -> v in
    OpaWalk.Code.map_down aux lcode


let pass_adding_server ~options lcode =
  if Option.get options.OpaEnv.no_server
  || not options.OpaEnv.stdlib
  || (match ObjectFiles.compilation_mode () with
      | `compilation | `init -> true
      | `linking | `prelude -> false)
  then (* it allows us to use --force-server and --no-stdlib
        * for example to call the slicer even in --no-stdlib
        * even without server *)
    lcode
  else
    let final =
      SurfaceAstCons.with_builtin_position (fun () ->
        let run_server_id = OpaMapToIdent.val_ Opacapi.Server_private.run_services in
        let run_server = C.E.ident run_server_id in
        let app = C.E.applys run_server [] in
        let start_server = OpaMapToIdent.val_start_server_add () in
        let declaration_toplevel = C.C.newval start_server app in
        declaration_toplevel
    ) in
    lcode @ [final]
