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

(* depends*)
module List = BaseList

(* shorthands *)
module F = DbIo

#<Debugvar:DEBUG_DB$flag "migration">

let debug fmt =
  #<If> Printf.printf ("[35m[Migration] [33m"^^fmt^^"[0m\n%!")
  #<Else> Printf.ifprintf stdout fmt
  #<End>

let print fmt =
  Logger.info ("[Migration] "^^fmt)

let error fmt =
  Logger.error ("[Migration] "^^fmt)


(* -- *)

let cc0 = '\000'
let cc1 = '\001'
let cc2 = '\002'
let cc3 = '\003'
let cc4 = '\004'
let cc5 = '\005'
let cc6 = '\006'

let c0 = F.WChar cc0
let c1 = F.WChar cc1
let c2 = F.WChar cc2
let c3 = F.WChar cc3
let c4 = F.WChar cc4
let c5 = F.WChar cc5
let c6 = F.WChar cc6

(**
   Abstraction for factorizing migration operations
*)
module Migration :
sig

  (**
     The abstract type of a new version
  *)
  type version

  (**
     get the version from an int
  *)
  external make_version : int -> version = "%identity"

  (**
     Type returned by the init function.
     Needed to finalize the migration
  *)
  type initial

  val initialize : version -> F.t -> F.file list -> initial

  (**
     For asserting that the finalize function was call.
     <!> Binary format dependent (config).
  *)
  type final

  val finalize : F.t -> initial -> final

end =
struct
  type version = int
  external make_version : int -> version = "%identity"

  type seek = {
    in_ : int ;
    out : int ;
  }

  type initial = {
    seek : ( F.file * seek ) list ;
    version : version ;
  }

  (* Backup function.
   * Copy old db files, adding the version on their name
   * ex : config_file -> config_file_vers17
   *)
  let backup version fm lst =
    List.iter
      (fun f ->
         let fname = F.get_name fm f in
         let res = File.copy fname (Printf.sprintf "%s_vers%d" fname (version - 1)) in
         if res <> 0 then
           error "Can not backup the file %s" fname) lst


  let initialize version fm lst =
    backup version fm lst ;
    let seek = List.rev_map (
      fun file ->
        let in_ = F.position_in fm file in
        let out = F.position_out fm file in
        let seek = {
          in_;
          out ;
        } in
        F.seek_in fm file 0;
        F.seek_out fm file 0;
        file, seek
    )
      (List.uniq_unsorted (F.Config :: lst)) in
    {
      seek ;
      version ;
    }


  type final = unit
  let finalize fm init =
    (* CONFIG *)
    (* /!\ be careful, if the config binary format changes *)
    let conf = F.Config in
    F.seek_out fm conf 0 ;
    F.add_int fm conf init.version ;

    (* SEEK *)
    List.iter (
      fun (file, seek) ->
        F.seek_in fm file seek.in_ ;
        F.seek_out fm file seek.out ;
    ) init.seek ;

    ()

end

(*** Utils ***)

let get_length_write w =
  let rec aux acc = function
    | x::y ->
        let size =
          match x with
          | F.WChar _ -> 1
          | F.WInt _ -> 4
          | F.WString s -> 4 + String.length s
          | F.WFloat _ -> 8
          | F.WInt32 _ -> 4
          | F.WInt64 _ -> 8
        in
        aux (size+acc) y
    | [] -> acc
  in aux 0 w
