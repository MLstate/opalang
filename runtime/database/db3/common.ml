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
(*
    @author1 Henri Binsztok,
    @author2 Gregoire Makridis
**)

(** {6 DATA} *)

module type DATA =
sig

  (**
     TODO: documentation
  *)

  type t

  val empty : t

  (**
     returns a debug string (summarized)
  *)
  val to_string : t -> string

  (**
     returns the full contents as string
  *)
  val get_string : t -> string

  (** {6 Projections} *)

  (**
     Mathieu Wed Mar 16 11:14:27 CET 2011

     Old situation:

     Projections was there for historical design.  There was 2 different
     representation of datas, one is for being used when we manipulate the
     database in memory ([t]), the other representation is used by the IO
     module, for writing and reading datas in the physical database (db-files).

     Simplification:

     We will try now to simplify this design, and to merge the 2 representations
     for avoiding the cost of projections.
     We will apply the following steps to the code :

     1)
     The 2 fonctions [write/read] of all modules implementing this interface
     will be replaced by the [identity] (module by module)

     2)
     The interface will be changed, so that t = io, and the 2 fonction exported
     as the identity

     3)
     The rest of the code using [read/write] will be changed for removing the
     call to these functions. They will become unused.

     4)
     The interface will be changed, removing the function [write/read], and the type [io]
  *)

  (** {6 Index} *)

  (**
     Indexation. Build a dictionnary entry.
     Count how many time a word appears in a data.
  *)
  val index_fun : t -> int StringMap.t
end

module type COMMON =
sig

  (**
     A private type for representing positive int.
     Used for Eid, Uid, Revision.
  *)
  type t

  (**
     Used to be [t -> t option], returning [None]
     if [t] is max_int.
     But nobody was handling the case [None] ([assert false])
     so, now the assert is in this function.
  *)
  val succ : t -> t

  (**
     Return the previous index, or [None] if the index is [0]
  *)
  val pred : t -> t option

  val make : int -> t
  external value : t -> int = "%identity"
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val max : t -> t -> t
  val min : t -> t -> t
end

(*
  Shared implementation for :
  - eid
  - uid
  - revision

  Do not coerce there into COMMON,
  it would makes eid, uid and revision unifiable.
*)
module IntCommon =
struct
  (* if you patch this, make sure that this module doesn't
   * use needlessly the SLOW polymorphic comparison
   * operators *)
  type t = int

  (*
    About the two following functions

    Nobody catches the exception:

    {[
    exception CommonMake
    ]}

    It is replaced by an assert false.

    People using succ and pred used to write stuff like:
    {[
    match succ uid with
    | Some uid -> uid
    | _ -> assert false
    ]}
    So, the assert false is now there directly, and no option is allocated.
  *)

  let succ i =
    if i < max_int
    then succ i
    else assert false

  let pred i =
    if i > 0
    then Some (pred i)
    else None

  let make i =
    (* checking that i <= max_int seems somewhat useless *)
    if i >= 0 then i
    else assert false

  external value : t -> int = "%identity"
  let to_string = string_of_int
  let equal : int -> int -> bool = (=)
  let compare : int -> int -> int = Pervasives.compare
  (* Pervasives.max and Pervasives.min are not specialized when coerced
   * so we redefine them with a specialized type *)
  let max (i:int) j = if i >= j then i else j
  let min (i:int) j = if i <= j then i else j
end
