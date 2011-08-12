/*
    Copyright Â© 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
 * @author Rudy Sicard 2011
 */

/**
 * Utilities to detect cycle in sequence or recursive computation.
 */

/**
  * {1 Interface of a cycle detector implementation}
  */
type CycleDetector('t,'elmt) =
{{
  empty : 't
  push  : 'elmt, 't -> 't
  detected : 't -> bool
}}

/**
  * {2 Interface of a cycle detector implementation based on equal function parameter}
  */
type CycleDetectorFunctor('t) = {{
  create : forall('elmt) . ('elmt,'elmt->bool) -> CycleDetector('t,'elmt)
}}

/**
  * {3 Implementation based on Toirtoise and Hare algorithm }
  */
@abstract
type TortoiseAndHare.t('a) =
{
  detected : bool
  tortoise_step : int
  hare : list('a) // last values first
  tortoise : list('a)// first value last
}

TortoiseAndHare = {{
 create(equal:'a,'a->bool) = {{
  empty = {
    detected = false
    tortoise_step = 0 // so on first push nothing we only fill hare
    hare = []
    tortoise = []
    }:TortoiseAndHare.t('a)

  push(h:'a,t:TortoiseAndHare.t('a)):TortoiseAndHare.t('a) =
   old_hare = t.hare
   hare = [h|old_hare]
   tortoise_step = t.tortoise_step+1
   if Bitwise.land(tortoise_step,1)==0 then (
     match t.tortoise
     [] ->
       do @assert(old_hare != [])
       push(h,{t with tortoise=List.rev(old_hare) hare = []}) // only case of empty old_hare
     [tor|tortoise] ->
       t = ~{t with tortoise_step hare tortoise}
       if equal(tor,h) then ~{t with detected=true} else t
   )
   else ~{t with tortoise_step hare}

  detected(t:TortoiseAndHare.t('a)) = t.detected
}}
}} : CycleDetectorFunctor
