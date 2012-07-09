/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
Profile = CoreProfiler
