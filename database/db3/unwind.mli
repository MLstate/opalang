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
    @author1 Henri Binsztok,
    @author2 Gregoire Makridis
    @author3 Mikolaj Konarski
**)

(** Some general remarks, mostly about specification of path unwinding.

    For conflict detection we need a normal form of paths,
    so for all writes to the database, as entered into the query map,
    copy nodes should not be unwound, but always look as if a physical
    copy was performed, whether it is or not for a given node. For reading,
    copies should be unwound, because reading should not create a physical
    copy and the virtual copy resides at the nodes pointed to by the fully
    unwound path.

    When trasaction conflict detection is over and write queries
    are finally applied, the copy nodes should not be just
    unwound, but unwound until the first copy and then physical copying
    should take place, as lazily as possible, until the end of path
    or the first link. This unwinding, coupled with physical copying
    of as few nodes, as possible is not performed in this file,
    but in the query application functions elsewhere. If a link inside
    a copy is encountered, for correctness of conflic detection and for speed
    it's much better if it's eliminated (unwound) already in this file,
    during path unwinding. Then links inside copies never appear
    during query application. For a test case showing how conflict detection
    fails if we do not unwind a link inside copy already during path unwinding,
    see ml/badop/04-transactions.ml and it's bug report in git history.
    Following such links is also essential for the new semantics
    of badop [Stat] operation.

    A realted design question we faced at one point was:
    Does a link inside copy point to the old revision
    (the revision of the copy) or the current revision?
    We've concluded that the revision has to be the current revision,
    because if we stayed within the old revision and, when writing
    at the path, continued to physically copy nodes,
    it would overwrite many nodes along the path
    (with nodes from the old revision) outside of the subtree
    at the original copy node. This would be very unintuitive.

    Note for comparison, that non-link paths inside copy nodes
    always refer to old revision and this does not lead to unexpected
    overwrites, because the nodes are copied in the new place,
    under the new path, and not at the path the are read from.
    This difference between copy and link nodes suggests that
    only copy nodes can be used for recursive types, or else
    copying data of these types will not work (links escape from the copies).

    Another design decision: we do not and cannot ensure
    the property that no links have to be unwound below a link
    (that the link constructor argument does not point to nodes with links).
    because a link can be added to a path already in query map
    and the path in the link constructor is not changed by that.
    The same with copy nodes and in fact, when used for recursive types,
    copy nodes often point to other copy nodes.
*)

(** Every node is either affected by a query (via operations
   on the query list for that node or via [tr_remove_list]
   or it is unaffected and the can be looked up in the low-level db.
*)
type node_or_query =
  | Node of Node.t
  | Query of QueryMap.query list

(** [follow_path db remove_list node_opt path_start path_end map qlist] follows
    a path, at once in db and query map, until copy or link is encountered.
    As soon as the query map is left, the following is handed down
    to [Hldb.follow_path[.
    Assumption: [path_start] belongs to query map (for root we assume it does).

    @param db  the database to inspect
    @param remove_list  the list of paths considered removed
    @param node_opt  the node to start traversing at, at [path_start]
    @param path_start  the path at which the node or query map can be found
    @param path_end  the path to walk along (as a[ Keys.t list])
    @param map  the current query map, at [path_start]
    @param qlist  the current query list, at [path_start] ([] for root)

    @return The node at which a copy or link was encountered
    or the query list that cointains the copy or link, if any,
    and the remaining suffix of the path.
*)
val follow_path :
  Hldb.t -> Path.t list -> Node.t option -> Path.t -> Keys.t list
  -> QueryMap.t -> QueryMap.query list -> Keys.t list * node_or_query

(** Every node is either affected by a query (via operations
   on the query list for that node or via a remove list
    (if the node is outside the actual query map, this case is represented
    by an empty query list) or it is unaffected and then can be
    looked up in the low-level db.
*)
type unwind_mode =
    { stop_at_copy : bool;
      stop_at_last_link : bool }

(** [follow_link ~unwind_mode db remove_list map path] follows a path,
    unwinding links and/or copies in ways specified in [unwind_mode],
    taking into account both the low level db content and the query_map
    and remove_list from the current transaction (given in the first 3 args).
    If there are links inside copies, they always escape from the revision
    of the copy to the current revision, affected by the query map,
    even if the [unwind_mode] specifies that copies should not be investigated.
    In the result, no links remain on the unwound path,
    except possibly the last This is necessary for transaction conflict
    detection (paths with and without links would wrongly not be
    detected as conflicting), but also optimizes the database access
    (no costly jumping links in any later operations using the unwound path).

    @param unwind_mode  the way copies and links should be traversed
    @param db  the database to inspect
    @param remove_list  the list of paths considered removed
    @param map  the current query map, corresponding to the root of db
    @param path  the path to walk along

    @return The path unwound according to [unwind_mode] and the node
    or the query list at the end of the path.
*)
val follow_link :
  unwind_mode:unwind_mode
  -> Hldb.t -> Path.t list -> QueryMap.t -> Path.t -> Path.t * node_or_query
