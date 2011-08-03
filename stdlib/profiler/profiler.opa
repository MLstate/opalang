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
 * @author Adam Koprowski, April 2010
 *
 *
 * {1 About this module}
 *
 * A simple module for pseudo-profiling; allows to call functions,
 * measuring execution time and printing summary in the end.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Server_profiler interface}
 */

/**
 * Perform profiling on the server
 */
Server_profiler = {{

  @private
  @server
  data = Reference.create(StringMap.empty : stringmap((int, Duration.duration)))

  init() =
    Reference.set(data, StringMap.empty)

  execute(f, label) =
    t_beg = Date.now()
    res = f()
    t_end = Date.now()
    old_data = Reference.get(data)
    execution_time = Date.between(t_beg, t_end)
    (c, t) = StringMap.get(label, old_data) ? (0, Duration.empty)
    new_entry = (c + 1, Duration.add(t, execution_time))
    new_data = StringMap.add(label, new_entry, old_data)
    do Reference.set(data, new_data)
    res

  summarize() =
    show(label, (c, t)) =
      print("[{label}]: called {c}x, time: {Duration.to_string(t)}\n")
    StringMap.iter(show, Reference.get(data))

}}

/*
/**
 * Perform profiling on the client
 */
client Client_profiler = {{

  data = Client_reference.create(StringMap.empty : stringmap((int, Duration.duration)))

  init() =
    Client_reference.set(data, StringMap.empty)

  execute(f, label) =
    t_beg = Date.now()
    res = f()
    t_end = Date.now()
    old_data = Client_reference.get(data)
    execution_time = Date.between(t_beg, t_end)
    (c, t) = StringMap.get(label, old_data) ? (0, Duration.empty)
    new_entry = (c + 1, Duration.add(t, execution_time))
    new_data = StringMap.add(label, new_entry, old_data)
    do Client_reference.set(data, new_data)
    res

  summarize() =
    show(label, (c, t)) =
      print("[{label}]: called {c}x, time: {Duration.short_string_of(t)}\n")
    StringMap.iter(show, Client_reference.get(data))

}}
*/

/**
 * {1 Profile interface}
 */

/**
 * [Profile] helps to monitor function execution time
 */
/*
 * TODO fix the previous module and merge with this one
 */
Profile = {{

  /** [measure(n)(f)] returns a pair: f() and the float representing the number of
      seconds needed to execute f, n times. The timing information should have
      better than millisecond precision, hence making it suitable for execution
      profiling. */
  measure(n)(f)=
    get_time = %%BslTime.get_accurate_time%% :  -> float
    t_beg = get_time()
    rec aux(n, v) =
      if n == 0 then
        t_end = get_time()
        (t_end - t_beg, v)
      else
        aux(n-1, f())
     aux(n-1, f())

  /** [instrument(n,report)(f)] returns the value of f() and call report with the time needed to execute f, n times */
  instrument(n, report)(f)=
    (t, r) = measure(n)(f)
    do report(t) : void
    r

}}
