/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @author Adam Koprowski (re-factored from okko)
 * @destination public
 * @category Data
 * @stability Stable
 */

/**
 * {1 About this module}
 *
 * Contains the [outcome] data type, which represents an outcome of
 * some action, which can be either successful (and carry the result
 * value) or it can be a failure (and carry the description of the
 * problem).
 *
 * {1 Where should I start ?}
 *
 * So the [outcome] type has two constructors:
 *   - [{success=value}], with some [value]
 *   - [{failure=problem}], with indication of the [problem].
 * and it is parametrized by the types of: [value] and [problem].
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * Type [outcome('a, 'b)] is a type allowing to encode a result
 * being either a success of type ['a] ([{success='a}]) or a
 * failure, with additional information of type ['b] ([{failure='b}]).
**/
type outcome('a, 'b)=
    { success: 'a }
  / { failure: 'b }

/**
 * {1 Interface}
 */

Outcome =
{{

  /**
   * Works like [get], except that you can choose the error message
   */
  get_msg = (fun_msg, s -> match s : outcome with
    | { ~success } -> success
    | { failure = e } -> error("{fun_msg()}\nReason: {e}")
  ) : (-> string), outcome('a, 'b) -> 'a

  /**
   * @param outcome
   * @return true iff the outcome indicates a failure
   */
  is_failure(outcome : outcome('a, 'b)) : bool = match outcome with
    | {failure=_} -> true
    | {success=_} -> false

  /**
   * @param outcome
   * @return true iff the outcome indicates a success
   */
  is_success(outcome : outcome('a, 'b)) : bool = match outcome with
    | {failure=_} -> false
    | {success=_} -> true

  /**
   * [get(o)] returns the success value of [o]
   * If [o] is [failure], it exits with an error
   */
  get = get_msg(-> "Outcome.get called on \{failure}",_) : outcome('a, 'b) -> 'a

  /**
   * [map(f_success, f_failure)(v)] maps an outcome [v].
   *
   * @param f_success mapping function to be used for a success value
   * @param f_failure mapping function to be used for a failure value
   * @param v an outcome to be mapped
   * @return Value of [v] mapped with [f_success] or [f_failure] depending
   *         on whether [v] is a success/failure value. More precisely:
   *         [{success=f_success(s)}] if [v == {success=s}] and
   *         [{failure=f_failure(f)}] if [v == {failure=f}]
   */
  map(f_success: 's1 -> 's2, f_failure: 'f1 -> 'f2)(v : outcome('s1, 'f1))
    : outcome('s2, 'f2) =
    match v with
    | {success=s} -> {success=f_success(s)}
    | {failure=f} -> {failure=f_failure(f)}

  /**
   * Maps successful outcome (returning unchanged failure outcomes).
   * See also {!Outcome.map}.
   *
   * @param f_success
   * @param v an outcome to be mapped
   * @return for success value [v] its result is modified with
   *         [f_success]; for failure value [v] it is returned
   *         unchanged
   */
  map_success(f_success: 's1 -> 's2)(v : outcome('s1, 'f)) : outcome('s2, 'f) =
    map(f_success, identity)(v)

  /**
   * Maps outcome indicating a failure (returning unchanged successful outcomes).
   * See also {!Outcome.map}.
   *
   * @param f_success
   * @param v an outcome to be mapped
   * @return for success value [v] its result is modified with
   *         [f_success]; for failure value [v] it is returned
   *         unchanged
   */
  map_failure(f_failure: 'f1 -> 'f2)(v : outcome('s, 'f1)) : outcome('s, 'f2) =
    map(identity, f_failure)(v)

}}
