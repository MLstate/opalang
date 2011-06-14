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
type 'a answer = [ `Absent | `Answer of 'a | `Linkto of Badop.path ]
type database = {
  session : Session_light.t;
  file : string;
  mutable node_config : Badop.Node_property.config;
}
type transaction = { db : database; tr : Transaction_light.t; }
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val open_database : Badop.options -> (database -> 'a) -> 'a
val close_database : database -> (unit -> 'a) -> 'a
val status : database -> (Badop.status -> 'a) -> 'a
module Tr :
  sig
    val start : database -> (transaction -> 'a) -> 'a
    val start_at_revision : database -> 'a -> (transaction -> 'b) -> 'b
    val prepare : transaction -> (transaction * bool -> unit) -> unit
    val commit : transaction -> (bool -> 'a) -> 'a
    val abort : transaction -> (unit -> 'a) -> 'a
  end
type revision = Revision.t
type 'a read_op = ('a, revision) Badop.generic_read_op
val read :
  transaction ->
  Badop.path ->
  (Badop_lib.Dialog.query, Revision.t) Badop.generic_read_op ->
  ([> `Absent
    | `Answer of (Badop_lib.Dialog.response, Revision.t) Badop.generic_read_op ] ->
   'a) ->
  'a
type 'a write_op = ('a, transaction, revision) Badop.generic_write_op
val write :
  transaction ->
  Path.t ->
  (Badop_lib.Dialog.query, transaction, 'a) Badop.generic_write_op ->
  ((Badop_lib.Dialog.response, transaction, 'a) Badop.generic_write_op -> 'b) -> 'b
val write_list :
  transaction ->
  (Path.t * (Badop_lib.Dialog.query, transaction, 'a) Badop.generic_write_op) list ->
  (transaction -> unit) -> unit
val node_properties : database -> Badop.Node_property.config -> (unit -> 'a) -> 'a
module Debug :
  sig
    val revision_to_string : Revision.t -> string
    val path_to_string : Path.t -> string
  end
