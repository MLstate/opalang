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
