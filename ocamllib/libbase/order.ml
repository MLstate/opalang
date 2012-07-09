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
module String : (OrderedTypeSig.S with type t = string) =
struct
  type t = string
  let compare = String.compare
end

module StringList : (OrderedTypeSig.S with type t = string list) =
struct
  type t = string list
  let compare = Pervasives.compare
end

module Float : (OrderedTypeSig.S with type t = float) =
struct
  type t = float
  let compare (a:float) = Pervasives.compare a
end

module Int : (OrderedTypeSig.S with type t = int) =
struct
  type t = int
  let compare (a:int) = Pervasives.compare a
end

module IntList : (OrderedTypeSig.S with type t = int list) =
struct
  type t = int list
  let compare (a:int list) = Pervasives.compare a
end

module Int32 : (OrderedTypeSig.S with type t = int32) =
struct
  type t = int32
  let compare = Int32.compare
end

module Int64 : (OrderedTypeSig.S with type t = int64) =
struct
  type t = int64
  let compare = Int64.compare
end

module Char : (OrderedTypeSig.S with type t = char) =
struct 
  type t = char
  let compare = Char.compare
end
