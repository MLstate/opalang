/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
