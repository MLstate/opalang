/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @author Valentin Gatien-Baron (documentation)
 * @destination public
 * @category Data
 * @stability Stable
 */

/**
 * {1 About this module}
 *
 * Contains the 'option' data type.
 * This data type allows you to represent arbitrary type with the ability
 * to indicate lack of value.
 * It is a type-safe(!) version of null/void values in many imperative languages.
 * It is most useful to create functions that may fail, or values that need a
 * dummy case.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type either('a,'b) = {left:'a} / {right:'b}

@opacapi
type option('a) = { none } / { some : 'a }

/**
 * {1 Interface}
 */

@expand `?`(o, d) = match o : option with
    | { ~some } -> some
    | { none } -> d

@workable
Option = {{
  /**
   * A shorthand for [{none}]
   */
  none = {none} : option('a)

  /**
   * Wraps a value in the [some] case of an option
   */
  some = (v -> {some = v}) : 'a -> option('a)

  /**
   * [default(d,o)] returns the value contained in [o] if any, or [d]
   */
  default(d, o) = o ? d

  /**
   * [default(d,o)] returns the value contained in [o] if any, or [d()]
   */
  lazy_default = (d, o -> match o : option with
    | { ~some } -> some
    | { none } -> d()
  ) : (-> 'a), option('a) -> 'a

  /**
   * [lazy_switch(some_cb,none_cb,o)] returns
   * [lazy_default(none_cb,map(some_cb,o))]
   */
  lazy_switch = (some_callback, none_callback, o ->
    match o with
      | { ~some } -> some_callback(some)
      | { none } -> none_callback()
  ) : ('a -> 'b), (-> 'b), option('a) -> 'b

  /**
   * [switch(some_cb,none,o)] returns
   * [default(none,map(some_cb,o))]
   */
  switch = (some_callback, if_none, o ->
    match o with
      | { ~some } -> some_callback(some)
      | { none } -> if_none
  ) : ('a -> 'b), 'b, option('a) -> 'b

  /**
   * For continuation writing, when trying to get a some case
   */
  lazy_try = (callbacks, o ->
    match o with
    | {some = _ } -> o
    | {none} ->
      match callbacks with
      | [] -> none
      | [thunk|callbacks] -> lazy_try(callbacks,thunk())
  ) : list(-> option('a)), option('a) -> option('a)

  /**
   * [map(f,o)] returns [{none}] when [o] is [{none}], or [{some=f(a)}] when
   * [o] is [{some=a}]
   */
  map = (f, o -> match o : option with
    | { some = item } -> some(f(item))
    | { none = _ } -> none
  ) : ('a -> 'b), option('a) -> option('b)

  /**
   * [iter(f,o)] calls [f] on the content of [o], if it has any
   */
  iter = (f, o -> match o : option with
    | { some = item } -> f(item)
    | { none } -> void
  ) : ('a -> void), option('a) -> void

  /**
   * [bind(f, o)] returns [none] if [o] is [none] or [f(a)] if [o] is [some(a)]
   */
  bind = (f, o -> match o : option with
    | { some = item } -> f(item)
    | { none = _ } -> none
  ) : ('a -> option('b)), option('a) -> option('b)

  /**
   * Works like [get], except that you can choose the error message
   */
  get_msg = (fun_msg, o -> match o : option with
    | { ~some } -> some
    | { none = _ } -> error(fun_msg())
  ) : (-> string), option('a) -> 'a

  /**
   * [get(o)] returns the value of [o]
   * If [o] is [none], raises a runtime error.
   */
  @expand
  get(o:option('a)) = match o with
    | {none} -> @fail("Option.get called on \{none}")
    | {some = s} -> s

  /**
   * [is_some(o)]
   * @return [{true}] if [o] is a [{some=_}]
   */
  is_some(o : option('a)) : bool =
    match o with
    | { some = _ } -> true
    | _ -> false

  /**
   * [is_none(o)]
   * @return [{true}] if [o] is [{none=_}]
   */
  is_none(o : option('a)) : bool =
    match o with
    | { none = _ } -> true
    | _ -> false

  ordering(using: order('a, 'b), x:option('a), y:option('a)): Order.ordering =
     match (x, y) with
          | ({none}, {none}) -> {eq}
          | ({none}, _) -> {lt}
          | (_, {none}) -> {gt}
          | ({some=a}, {some=b}) -> Order.compare(a,b,using)

  make_order(using: order('a, 'b)): order(option('a), 'b) =
     Order.make(ordering(using, _, _))

  merge( f:('a,'a -> 'a), x:option('a), y:option('a) ): option('a) =
     match (x, y) with
     | (_, {none}) -> x
     | ({none}, {some=_}) -> y
     | ({some=a}, {some=b}) -> {some=f(a,b)}


}}

@opacapi @workable some(x)    = Option.some(x)
@opacapi none    = Option.none
default = Option.default
