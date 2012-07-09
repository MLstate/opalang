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
   Revision Node uniq identifiers.
*)

(**
   An [uid] is an uniq index for identifying a node in the database,
   coupled with a revision index.

   [uid] are uniq for each couple ([revision * eid]).
   For a node identified by its [eid], there may be several
   revision, and each revision of a node is identified
   by an uniq [uid]
*)

include Common.COMMON
module Map : BaseMapSig.S with type key = t
