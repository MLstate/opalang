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

(**
   Stable Topologic Sort, based on string indexation.
   @author Mathieu Barbin
*)

(** {6 Argument} *)

(** A module for manipulating nodes of your graph *)
module type ItemType =
sig
  (** The type of a node of your graph. *)
  type t

  (**
     Indexing nodes of your graph.
     The function [index] should be injective.
  *)
  val index : t -> string

  (**
     Given a node of the graph, return the list of index
     of the direct parents.
  *)
  val depends : t -> string list
end

(** {6 Functor} *)

(** The module you'll get for proceding to the topologic sort *)
module type S =
sig

  (**
     The type of the nodes. This is the same type as
     defined in the module in argument of [Make].
  *)
  type t

  (**
     raised if one of the [t] is in a cyclic dependency loop.
  *)
  exception CyclicDep of t

  (**
     raised if 2 [t] of the input list of [sort] have
     the same index.
  *)
  exception IndexConflict of t * t

  (**
     raised by computing the transitive_dependencies, in case
     the given [t] in not previously in the list given for
     computing the env
  *)
  exception Not_found_index of string

  (**
     Stable topologic sort.
     The function return a sorted list wrt a topologic order of elements of [t].
     The second elt returned is the list of [not_referenced] nodes.
     For each non referenced node [index] (we have only the index), we give the list
     of [nodes] which have declared index as a depends.
  *)
  val sort : t list -> t list * (string * t list) list

  type env
  val compute : t list -> env

  (**
     Like sort, in case you do not want to recompute everything
     e.g. from a previous env.
  *)
  val get_order : env -> t list * (string * t list) list

  (**
     Not reflexive.
  *)
  val transitive_dependencies : env -> t -> t list

end

module Make : functor (Elemt : ItemType) -> S with type t = Elemt.t
