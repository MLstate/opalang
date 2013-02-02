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
module String : (OrderedTypeSig.S with type t = string)

module StringList : (OrderedTypeSig.S with type t = string list)

module Float : (OrderedTypeSig.S with type t = float)

module Int : (OrderedTypeSig.S with type t = int)

module IntList : (OrderedTypeSig.S with type t = int list)

module Int32 : (OrderedTypeSig.S with type t = int32)

module Int64 : (OrderedTypeSig.S with type t = int64)

module Char : (OrderedTypeSig.S with type t = char)
