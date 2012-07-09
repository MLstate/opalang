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

type t = Dbm.t

type flags = DB_create | DB_rdwr | DB_rdonly

exception DB_error of string

let _conv_flag = function
  | DB_create -> Dbm.Dbm_create
  | DB_rdwr -> Dbm.Dbm_rdwr
  | DB_rdonly -> Dbm.Dbm_rdonly
let _conv_flags = List.map _conv_flag

let _conv_exn f a = try f a with Dbm.Dbm_error str -> raise (DB_error str)

let opendb file flags perms = _conv_exn (Dbm.opendbm file (_conv_flags flags)) perms
let find t k = _conv_exn (Dbm.find t) k
let replace t k v = _conv_exn (Dbm.replace t k) v
let remove t k = _conv_exn (Dbm.remove t) k
let iter f t = _conv_exn (Dbm.iter f) t
let close t = _conv_exn Dbm.close t
