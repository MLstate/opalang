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
(**
   Revision indexes.
*)

(**
   Database revision are comparable with "versions" of the database.
   For each new transaction, the revision index of the database is
   incremented during the commit.

   Revision are used for indexing several historic versions of
   nodes. We can e.g. get back to, or access a previous version of a node
   using a previous revision index.
*)

include Common.COMMON
module Map : BaseMapSig.S with type key = t
