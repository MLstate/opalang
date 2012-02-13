(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(*
    @author Louis Gesbert
**)

open Cps.Ops
module D = Badop.Dialog

module F (Bk: Badop.S) = struct
  type database = Bk.database
  type transaction = Bk.transaction
  type revision = Bk.revision

  let open_database options k = Bk.open_database options @> k
  let close_database db k = Bk.close_database db @> k
  let status db k = Bk.status db @> fun st -> Badop.Layer("Workaround", st) |> k


  module Tr = struct
    let start db errk k = Bk.Tr.start db errk @> k
    let start_at_revision db rev errk k = Bk.Tr.start_at_revision db rev errk @> k
    let prepare tr k = Bk.Tr.prepare tr @> k
    let commit tr k = Bk.Tr.commit tr @> k
    let abort tr k = Bk.Tr.abort tr @> k
  end

  type 'which read_op = 'which Bk.read_op
  type 'which write_op = 'which Bk.write_op

  let follow_path tr path ?(no_follow_last=false) k =
    let rec aux origin pathlst k = match pathlst with
      | [] ->
          origin |> k
      | [key] when no_follow_last ->
          Path.add origin key |> k
      | key::pathlst ->
          let path = Path.add origin key in
          Bk.read tr path (Badop.Stat (D.query ()))
          @> function
          | `Answer (Badop.Stat (D.Response (real_path, _, _))) ->
              #<If:BADOP_DEBUG$minlevel 10>
                if path <> real_path then
                  Printf.eprintf ">> follow_path: following %s => %s\n"
                    (Path.to_string path) (Path.to_string real_path)
              #<End>;
              aux real_path pathlst @> k
          | `Answer _ -> assert false
          | `Absent | `Linkto _ ->
              #<If:BADOP_DEBUG$minlevel 10>
                Printf.eprintf ">> follow_path: stopping at %s/( %s )\n"
                (Path.to_string origin) (Path.to_string (Path.of_list (key::pathlst)))
              #<End>;
              Path.concat path (Path.of_list pathlst) |> k
    in
    aux Path.root (Path.to_list path)
    @> fun path2 ->
      #<If:BADOP_DEBUG$minlevel 10>
        Printf.eprintf ">> create_path: %s finally got to %s\n" (Path.to_string path) (Path.to_string path2)
      #<End>;
      path2 |> k

  let read tr path read_op k =
    follow_path tr path
      ~no_follow_last:(match read_op with
                       | Badop.Stat _ | Badop.Revisions _ -> true
                       | Badop.Contents _ | Badop.Children _ | Badop.Search _ -> false)
    @> fun path -> Bk.read tr path read_op @> k

  let create_path tr path ?(no_follow_last=false) k =
    let rec aux tr origin pathlst k = match pathlst with
      | [] -> (tr,origin) |> k
      | [key] when no_follow_last -> (tr, Path.add origin key) |> k
      | key::pathlst ->
          let path = Path.add origin key in
          Bk.read tr path (Badop.Stat (D.query ()))
          @> function
          | `Answer (Badop.Stat (D.Response (real_path, _, _))) ->
              #<If:BADOP_DEBUG$minlevel 10>
                if path <> real_path then
                  Printf.eprintf ">> create_path: following %s => %s\n"
                    (Path.to_string path) (Path.to_string real_path)
              #<End>;
              aux tr real_path pathlst @> k
          | `Answer _ -> assert false
          | `Absent | `Linkto _ ->
              if pathlst = [] then (tr,path) |> k
              else
                (#<If:BADOP_DEBUG$minlevel 10>
                   Printf.eprintf ">> create_path: %s doesn't exist, create\n" (Path.to_string path)
                 #<End>;
                 Bk.write tr path (Badop.Set (D.query Badop.Data.Unit))
                 @> function
                 | Badop.Set (D.Response tr) -> (* todo: do not continue checking subpaths for exist/link *)
                     aux tr path pathlst @> k
                 | _ -> assert false)
    in
    aux tr Path.root (Path.to_list path)
    @> fun (tr,path2) ->
      #<If:BADOP_DEBUG$minlevel 10>
        Printf.eprintf ">> create_path: %s finally got to %s\n" (Path.to_string path) (Path.to_string path2)
      #<End>;
      (tr,path2) |> k

  let write tr path write_op k =
    match write_op with
    | Badop.Clear _ ->
        follow_path tr path ~no_follow_last:true
        @> fun path -> Bk.write tr path write_op @> k
    | _ ->
        create_path tr path
          ~no_follow_last:(match write_op with
                           | Badop.Link _ | Badop.Clear _ -> true
                           | Badop.Set _ | Badop.Copy _ -> false)
        @> fun (tr,path) -> Bk.write tr path write_op @> k

  let write_list trans path_op_list k =
    let wr trans (path, op) k =
      write trans path op (fun resp -> Badop.Aux.result_transaction resp |> k)
    in
    Cps.List.fold wr trans path_op_list k

  let node_properties db config k = Bk.node_properties db config @> k

  module Debug = struct
    let revision_to_string = Bk.Debug.revision_to_string
  end
end
