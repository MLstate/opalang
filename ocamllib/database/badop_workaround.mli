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
(** This module ensures consistency when using the dispatcher prototype by
    * making sure all parents exists when writing a node
    * reading from the root to detect links

    NB: there is a huge overcost. This is not meant to last, but to have a
    working dispatcher ASAP and then focus on optimisations.
*)

module F : functor (Backend: Badop.S) -> Badop.S
