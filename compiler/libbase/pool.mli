(*
    Copyright © 2011, 2012 MLstate

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
(** A pool of element (for short lives element with high allocation cost).
   It diverges to standard allocation (pure GC) when the pool too small.
   Performance gains (vs standard) are not huge (at most 15% on persistant db actors)
   Detection of unused element is based on finalise (i.e. depends on GC),
   which can explain that the gain is small compared to pure GC. (element are not recycled immediatly)
   Can retain unsued memory (pool are originally bounded to 256 Mb per pool).
   By default the pool size is zero (disabled) (set MLSTATE_BUFFER_POOL to activate).
   Element must be heap allocated and must not be an array of float or a record of float.
*)
module Pool : functor (Egg:Egg.Egg) -> sig
  type pool
  type size = int
  val bound_cardinal_and_total_size : pool -> cardinal:int -> total_size:size -> unit
  val create : unit -> pool
  val alloc : pool -> ?hint:size -> unit -> Egg.egg
end

module BufferEgg : Egg.Egg with type egg = Buf.t
