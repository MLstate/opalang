(*
    Copyright © 2011, 2012 MLstate

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
module SA = SurfaceAst
module C = SurfaceAstCons.ExprIdentCons
module O = OpaEnv

let infos_ident =
  [

    "cps_client_mode", (fun options -> C.E.bool options.O.cps_client);

    "release", (fun options -> C.E.bool options.O.compile_release);
    "opa_filenames", (fun options -> C.E.list
                        (List.map C.E.string options.O.filenames));

    (* transformed into a [Date.date] using [int2time] *)
    "compilation_date", (fun _ -> C.E.int (Time.in_milliseconds (
                           #<If:DIFFING>
                             Time.zero
                           #<Else>
                             Time.now ()
                           #<End>
                        ))
                        );

    "opa_git_version",      (fun _ -> C.E.int BuildInfos.opa_git_version);
    "opa_git_version_hash", (fun _ -> C.E.string BuildInfos.opa_git_sha);
    "opa_date",             (fun _ -> C.E.string BuildInfos.date);
    "opa_is_release",       (fun _ -> C.E.bool BuildInfos.is_release);
  ]

let infos_ident_names =
  List.map fst infos_ident

let process_code ~options code =
  let idents = Hashtbl.create 1 in
  let lookup annot key =
    try Hashtbl.find idents key
    with Not_found -> (
      let ident = SurfaceAstCons.ExprIdent.ns_fresh ~label:annot key in
      if List.mem key infos_ident_names then
        Hashtbl.add idents key ident
      else (
        let context = OpaError.Context.annot annot in
        OpaError.serror context
          "The compile time key %s doesn't exists.@\n%a"
          key
          (HintUtils.pp_suggestion infos_ident_names) key
      );
      ident
    ) in
  let def key ident =
    let value = (List.assoc key infos_ident) options in
    C.C.newval ident value in
  let code =
    OpaWalk.Code.map_up
      (function
       | (SA.Directive (`compiletime s, [], []),annot) -> (SA.Ident (lookup annot s), annot)
       | (SA.Directive (`compiletime _, _, _),_) -> assert false
       | e -> e) code in
  SurfaceAstCons.with_builtin_position
    (fun () ->
       Hashtbl.fold (fun key ident code -> def key ident :: code) idents code
    )
