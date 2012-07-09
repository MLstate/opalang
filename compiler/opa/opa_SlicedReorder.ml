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

(* -- *)

let reorder_code ?(val_=OpaMapToIdent.val_noerr ~side:`server) roots roots_addon lcode  =
  QmlDependencies.reorder val_ roots roots_addon Reordering.create_group_list lcode

let reorder_in_new_qml ?val_ lcode =
  reorder_code ?val_ [] IdentMap.empty lcode

(*FIXME : This reorder is hackie because put the init server at end of
  code and hope that all initialisations values are before init
  server. The really fix is to really create dependencies beetween
  init server and some init values like css. *)
let perform ~client ~server =
  let client =
    reorder_in_new_qml
      ~val_:(OpaMapToIdent.val_noerr ~side:`client)
      client in
  let start_server_at_end =
    match OpaMapToIdent.val_start_server () with
    | None -> server
    | Some iserver ->
        let rec aux = function
          | ((QmlAst.NewVal (_, vlist)) as d)::tail
          | ((QmlAst.NewValRec (_, vlist)) as d)::tail ->
              if
                List.exists
                  (fun (i, _) -> Ident.equal iserver i)
                  vlist
              then tail@[d]
              else d::(aux tail)

          | h::t -> h::(aux t)
              (* FIXME: use standard modules for reporting internal Error *)
          | [] -> failwith "Unconsistent beetwen server code and OpaMapToIdent\n No start_server on server_code but val_start_server is setted to Some..."
        in aux server
  in let server =
    reorder_in_new_qml
      ~val_:(OpaMapToIdent.val_noerr ~side:`server)
      start_server_at_end
  in (client, server)
