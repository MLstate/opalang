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
open Base

(*
  We test strictly than the val_ function is called only on
  identifiers registred in opacapi, using the opacapi interface.
  The physical test asserts than the string corresponding to the
  identifier is not duplicated in the code containing the insertion.
*)
let opacapi_check s =
  let is_in_opacapi =
    try
      let ss = Hashtbl.find Opacapi.table s in
      s == ss
    with
    | Not_found -> false
  in
  if not is_in_opacapi
  then (
    #<If:OPACAPI_LOOSE>
      ()
    #<Else>
      OManager.printf "OPACAPI violation, on ident %S@\n" s ;
      OManager.printf "You should use opacapi for inserting identifiers from the stdlib@." ;
      assert false
    #<End>
  )

(* ******************************************************************)
(* Side type and utils **********************************************)
(* ******************************************************************)
type side = [`client | `server]

let other_side = function
  | `client -> `server
  | `server -> `client

(* ******************************************************************)
(* Link name <-> ident **********************************************)
(* ******************************************************************)
(*
  Two functions that is used to insert types or variables
  Since everything is renamed with the surfaceAst, you cannot insert a
  coercion to list in the AST (list is not going to be defined)
*)

(* Reference to the maps*)
let r_var = ref (StringMap.empty : Ident.t StringMap.t)
let r_var_client = ref (StringMap.empty : Ident.t StringMap.t)
let r_type = ref (StringMap.empty : Ident.t StringMap.t)

(* registering the references to be able to save them *)
let () =
  PassTracker.register_global_ref r_var;
  PassTracker.register_global_ref r_var_client;
  PassTracker.register_global_ref r_type

let get_rmap = function
  | `server -> r_var
  | `client -> r_var_client
let print_side = function
  | `server -> "server"
  | `client -> "client"

let val_noerr ?(side=`server) s =
  opacapi_check s ;
  StringMap.find s !(get_rmap side)

let pp_stringmap f map =
  StringMap.iter
    (fun k _v ->
       Format.fprintf f "%S " k)
    map

let val_ ?(side=`server) s =
  try val_noerr ~side s
  with Not_found ->
    OManager.i_error
      "OpaMapToIdent %s: Not found: %S\nIt contains:@\n%a@\n"
      (print_side side) s
      pp_stringmap
      !(get_rmap side)

let typ s =
  opacapi_check s ;
  try StringMap.find s !r_type
  with Not_found ->
    OManager.i_error
      "OpaMapToIdent: Type not found: %S\nIt contains:@\n%a@\n"
      s
      pp_stringmap
      !r_type

let val_opt ?(side=`server) s =
  opacapi_check s ;
  StringMap.find_opt s !(get_rmap side)

let val_add ?(side=`server) s =
  let new_s = Ident.next s in
  let r_var = get_rmap side in
  r_var := StringMap.safe_add s new_s !r_var;
  new_s

let val_unsafe_add ?(side=`server) s =
  let new_s = Ident.next s in
  let r_var = get_rmap side in
  r_var := StringMap.add s new_s !r_var;
  new_s

let set_val_map ?(side=`server) v = (get_rmap side) := v

let set_typ_map v = r_type := v

let get_val_map ?(side=`server) () = !(get_rmap side)

let iter_val_map ?(side=`server) = fun f -> StringMap.iter f !(get_rmap side)

let map_val_map ?(side=`server) = fun f -> StringMap.map f !(get_rmap side)

let fold_val_map ?(side=(`server)) = fun f -> StringMap.fold f !(get_rmap side)

(* Special add and get for start_server value *)
let start_server = ref None
let str_start_server = "``"
let val_start_server () =
  Option.map
    (fun i -> val_ i)
    !start_server

let val_start_server_add () =
  match !start_server with
  | Some _ -> failwith("start_server")
  | None ->
      let ident = Ident.next "run_services" in
      start_server := (Some str_start_server);
      set_val_map (StringMap.add str_start_server ident (get_val_map ()));
      ident
(** Hack for opacapi - To be cleanned by introduce dependencies
    beetween initialisations values (css, etc...) and init server *)
let _ = Opacapi.(!!) str_start_server

let get_toplevel_vars () = StringMap.elts !r_var

let filter f =
  r_var := StringMap.filter_val f !r_var;
  r_var_client := StringMap.filter_val f !r_var_client

let reset () =
  set_val_map ~side:`server StringMap.empty;
  set_val_map ~side:`client StringMap.empty;
  set_typ_map StringMap.empty;
  start_server := None
