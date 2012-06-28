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
(* cf mli *)

(* shorthands *)
module Q = QmlAst

(* -- *)
type t =
    {
      database : Q.code_elt list;
      new_type : Q.code_elt list;
      new_db_value : Q.code_elt list;
      new_val : Q.code_elt list;
    }

let empty =
  {
    database = [];
    new_type = [];
    new_db_value = [];
    new_val = [];
  }

let add_elt env elt = match elt with
| Q.Database _   -> { env with database = elt::env.database }
| Q.NewDbValue _ -> { env with new_db_value = elt::env.new_db_value }
| Q.NewType _    -> { env with new_type = elt::env.new_type }
| Q.NewVal _
| Q.NewValRec _  -> { env with new_val = elt::env.new_val }

let add = List.fold_left add_elt

let get t = (* tail without @ *)
  let rec rev acc = function
    | [] -> acc
    | t::q -> rev (t::acc) q in
  let acc = [] in
  let acc = rev acc t.new_val in
  let acc = rev acc t.new_db_value in
  let acc = rev acc t.new_type in
  rev acc t.database

module Get =
struct
  let all = get
  let database t = List.rev t.database
  let new_type t = List.rev t.new_type
  let new_db_value t = List.rev t.new_db_value
  let new_val t = List.rev t.new_val
end

(** need a Rev-Get for custom concat (e.g. in a filter Ast) *)
module RevGet =
struct
  let database t = t.database
  let new_type t = t.new_type
  let new_db_value t = t.new_db_value
  let new_val t = t.new_val
end
