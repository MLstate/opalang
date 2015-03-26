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

module SA = SurfaceAst
module SAP = SurfaceAstPassesTypes
module SAC = SurfaceAstCons.ExprIdentCons

(** Generate a call to String.flatten with the given expression list as parameter, need with_label. *)
let computeString parts =
  match parts with
  | hd :: [] -> hd
  | hd :: _ ->
    SurfaceAstCons.with_same_pos hd (fun () ->
      let flatten = SAC.E.ident (OpaMapToIdent.val_ Opacapi.String.flatten) in
      let parts = SAC.E.list parts in
      SAC.E.applys flatten [parts]
    )
  | _ -> failwith "Empty computed string list"

(** Replace all the strings in a source code. *)
let computeAllStrings code =
  OpaWalk.Code.map_down (fun ((e,label) as v) ->
    match e with
    (* "..." ==> String_flatten(...) *)
    | SA.Directive (`string, [], _) -> SAC.E.string ~label ""
    | SA.Directive (`string, parts, _) -> computeString parts
    | _ -> v
  ) code

(** Apply the transformation to both user code and generated code. *)
let process ~options:_ env =
  let lcodeNotUser = computeAllStrings env.SAP.lcodeNotUser in
  let lcodeUser    = computeAllStrings env.SAP.lcodeUser    in
  { env with SAP.
    lcodeNotUser = lcodeNotUser;
    lcodeUser    = lcodeUser
  }
