/*
    Copyright © 2011 MLstate

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
/**
 * {1 About this module}
 *
 * Contains the [outcome] datatype, which represents an outcome of
 * some action, which can be either successful (and carry the result
 * value) or it can be a failure (and carry the description of the
 * problem).
 * So the [outcome] type has two constructors:
 *   - [{success=value}], with some [value]
 *   - [{failure=problem}], with indication of the [problem].
 * and it is parametrized by the types of: [value] and [problem].
 *
 * @author Adam Koprowski (re-factored from okko)
 * @destination public
 * @category Data
 * @stability Stable
 */

/**
 * Type [outcome('a, 'b)] is a type allowing to encode a result
 * being either a success of type ['a] ([{success='a}]) or a
 * failure, with additional information of type ['b] ([{failure='b}]).
**/
type outcome('a, 'b)=
    { success: 'a }
  / { failure: 'b }

Outcome =
{{

  /**
   * Works like [get], except that you can choose the error message
   */
  get_msg = (fun_msg, s -> match s : outcome with
    | { ~success } -> success
    | { failure = _ } -> error(fun_msg())
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
