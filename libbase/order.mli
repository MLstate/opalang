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
module String : (OrderedTypeSig.S with type t = string)

module StringList : (OrderedTypeSig.S with type t = string list)

module Float : (OrderedTypeSig.S with type t = float)

module Int : (OrderedTypeSig.S with type t = int)

module IntList : (OrderedTypeSig.S with type t = int list)

module Int32 : (OrderedTypeSig.S with type t = int32)

module Int64 : (OrderedTypeSig.S with type t = int64)

module Char : (OrderedTypeSig.S with type t = char)
