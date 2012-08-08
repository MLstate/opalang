/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
    @author AUDOUIN Maxime
    @author ???
**/

/*
 * <!> totally untested
 */

/**
 * {1 About this module}
 *
 * Generic iterators
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

@opacapi
type iter('a) =
  { next: -> option(('a, iter('a))) }

/**
 * {1 Interface}
 */

Iter = {{

  /**
   * {2 Constructors}
   */

  cons(elt, i) =
    { next() = some((elt, i)) } : iter

  empty = { next() = none } : iter

  of_list(li) =
    { next() =
        match li
        | [] -> none
        | [e | l] -> some((e, of_list(l)))
    } : iter

  unfold(first, f) =
    { next() =
        match f(first) : option
        | {some=res} -> some((res.f1, unfold(res.f2, f)))
        | {none} -> none
    } : iter

  init(first, f, cond) =
    { next() =
        if cond(first) then
          some((first, init(f(first), f, cond)))
        else
          none
    } : iter


  /**
   * {2 Methods}
   */

  is_empty(s) = to_list(take(1, s)) == []


  /**
   * Returns from an iterator a new iterator where iteration will be cached.
   *
   * For each calls to the [next] function the original iteration (from [iter])
   * is computed, then its cached (the original iteration will never be called).
   *
   * @param iter An iterator
   * @return A cached iterator from [iter]
   */
  cache(iter:iter('a)):iter('a) =
    ref = Reference.create({unevaluated})
    {next = ->
      match Reference.get(ref) with
      | {unevaluated} ->
        result = iter.next()
        result = Option.map((a, n) -> (a, cache(n)), result)
        @atomic(match Reference.get(ref) with /* CAS */
          | {unevaluated} ->
            do Reference.set(ref, ~{result})
            result
          | ~{result} -> result)
      | ~{result} -> result
    }

  /**
   * Lazy
   */
  map(f, iter : iter) =
    { next() =
        match iter.next() : option
        | {some=res} -> some((f(res.f1), map(f, res.f2)))
        | {none} -> none
    } : iter

  /**
   * Lazy
   */
  filter(f, iter: iter) =
    { next() =
        match iter.next() : option
        | {some=res} ->
          if f(res.f1) then
            some((res.f1, filter(f, res.f2):iter))
          else
            (filter(f, res.f2):iter).next()
        | {none} -> none
    } : iter

  fold(f, i:iter, acc) =
     match i.next() : option
     | {some=res} -> fold(f, res.f2, f(res.f1, acc))
     | {none} -> acc

  iter(f:'a->void, i:iter) =
    match i.next()
    | {none} -> void
    | {some=res} -> do f(res.f1); iter(f, res.f2)

  /**
   * Lazy
   */
  take(n, i: iter) =
    { next() =
        if n == 0 then none else
        match i.next() : option
        | {some=res} -> some((res.f1, take(n-1, res.f2)))
        | {none} -> none
    } : iter

  skip(n, i: iter) =
    if n == 0 then i else
    match i.next() : option
    | {some=res} -> skip(n-1, res.f2 : iter)
    | {none} -> empty

  /**
   * Lazy
   */
  append(e1:iter, e2: iter) =
    { next() =
        match e1.next() : option
        | {some=res} -> some((res.f1, append(res.f2, e2)))
        | {none} -> e2.next()
    } : iter

  seq(x: int, y: int) =
    if x > y then init(x, _ + 1, _ <= y) : iter(int)
             else init(x, _ - 1, _ >= y) : iter(int)


  transform(f, it) =
    map((
      x -> match x : option with
        | {none} -> error("transform")
        | {some} -> some),
      filter(
        _ != none,
        map(f, it)))


   zip(it1 : iter, it2 : iter) =
     { next() =
      match (it1.next() :option, it2.next() : option) with
      | ({none}, _) -> {none}:option
      | (_, {none}) -> {none}:option
      | ({some=i1}, {some=i2}) ->
        some((
          (i1.f1, i2.f1),
          zip(i1.f2, i2.f2)
        ))} : iter

  index(it) = zip(it, init(0, _+1, (_ -> true)) )

  count(it:iter) =
    rec f(it:iter, c) =
      match it.next() :option with
      | {none} -> c
      | {~some} -> f( some.f2, c+1)
    f(it, 0)


  take_while(f, it : iter) =
    { next() =
      match it.next() : option with
      | {none} -> {none} : option
      | {some=s} -> if f(s.f1)
        then {some = (s.f1, take_while(f, s.f2) ) }:option
        else {none} : option
    } : iter

  skip_while(f, it : iter) =
    rec aux(it: iter) =
      match it.next() : option with
      | {none} -> empty
      | {some=s} -> if f(s.f1)
        then aux(s.f2)
        else it
    aux(it)


  exists(f, it) = not(is_empty(skip_while(not @ f, it)))

  find(f, it) = match skip_while(not @ f, it).next() : option
    | {none} -> {none} : option
    | {some=s} -> {some=s.f1} : option


  flatten(itits : iter) =
    rec g(it_hd : iter, it_tl:iter) =
    { next() =
       match it_hd.next() : option
       | {none} -> flatten(it_tl).next()
       | {~some} -> {some = (some.f1, g(some.f2, it_tl) ) } : option
    } : iter('a)
    match itits.next() :option
    | {none} -> empty
    | {some = (it_hd,it_tl)} ->
      g(it_hd, it_tl)

  distinct_by(f, it:iter) =
    { next() =
      match it.next() : option with
      | {none} -> {none} : option
      | {~some} -> {some = (some.f1, distinct_by(f, filter( x -> f(x) != f(some.f1), some.f2 ))) } : option
    } : iter

  min_by(f, iter:iter) = match iter.next() :option with
    | {none} -> {none}:option
    | {~some} -> {some=fold(
        (v, m -> if f(v) <= f(m) then v else m),
        some.f2,
        some.f1)
        }:option
  //
  max_by(f, iter:iter) = match iter.next() :option with
    | {none} -> {none}:option
    | {~some} -> {some=fold(
        (v, m -> if f(v) >= f(m) then v else m),
        some.f2,
        some.f1)
        }:option

  distinct(it:iter) = distinct_by(x -> x, it)
  min(it:iter) = min_by(x -> x, it)
  max(it:iter) = max_by(x -> x, it)


  to_list(iter:iter) =
    rec aux(acc,iter:iter) =
      match iter.next() : option
      | {none} -> List.rev(acc)
      | {some=(v,iter)} -> aux([v|acc],iter)
    aux([],iter)

}}

`--` = Iter.seq


StringIter =
{{
  to_iter(s) =
    l = String.length(s)
    Iter.map( ( String.get(_, s) ), Iter.seq(0, l-1))

  to_rev_iter(s) =
    l = String.length(s)
    Iter.map( ( String.get(_, s) ), Iter.seq(l-1, 0))

}}
