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

(**
    @author Henri Binsztok,
    @author Gregoire Makridis
*)

  type t

  (* database opening / closing *)
  val open_db : ?readonly:bool -> ?dot:bool -> ?weak:bool -> ?rev:int -> ?restore:bool -> string -> t * bool
  val close_db : ?donothing:bool -> t -> unit
  val is_empty : t -> bool

  (* revisions and timestamps *)
  val get_rev : t -> Revision.t
  val get_timestamp_from_rev : t -> Revision.t -> Time.t

  (* transactions *)
  val new_trans : ?read_only:(bool * Revision.t option) -> t -> Transaction.t
  val abort_or_rollback : t -> Transaction.t -> unit
  val try_trans_prepare : t -> Transaction.t -> (Transaction.t * bool) Cps.t
  val really_commit : t -> Transaction.t -> bool

  (* db writes *)
  val set : t -> Transaction.t -> Path.t -> DataImpl.t -> Transaction.t
  val remove : t -> Transaction.t -> Path.t -> Transaction.t
  val set_link : t -> Transaction.t -> Path.t -> Path.t -> Transaction.t
  val set_copy : t -> Transaction.t -> Path.t -> (Path.t * Revision.t option) -> Transaction.t

  (* db reads *)
  val get : t -> Transaction.t -> Path.t -> DataImpl.t
  val full_search : Transaction.t -> string list -> Path.t -> Keys.t list
  val get_all_rev_of_path : Transaction.t -> Path.t -> Revision.t list
  val get_last_rev_of_path : Transaction.t -> Path.t -> Revision.t (* fast *)
  val get_children : t -> Transaction.t -> ?rev:Revision.t
    -> Keys.t option * int -> Path.t
    -> (Path.t * Revision.t) list
  val stat :
    Transaction.t -> Path.t
    -> Path.t * Revision.t option * [`Data|`Link|`Unset]
