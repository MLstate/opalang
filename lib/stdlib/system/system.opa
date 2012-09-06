/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin server
import-plugin unix

/**
 * {1 Types defined in this file}
**/

/**
 * [{ WEXITED : int }]
 * The process terminated normally by exit ;
 * the argument is the return code.
 *
 * [{ WSIGNALED : int }]
 * The process was killed by a signal;
 * the argument is the signal number.
 *
 * [{ WSTOPPED : int }]
 * The process was stopped by a signal;
 * the argument is the signal number.
**/
type System.process_status =
   { WEXITED : int }
 / { WSIGNALED : int }
 / { WSTOPPED : int }

type System.wait_flag = external

type System.pid = int

type System.process = {
     kill : -> void
     pid  : System.pid
}

type System.process.out = {
     stdout: string
     stderr: string
     error : option(string)
}


/**
 * Binding with module System
 * <!> Not for casual user
**/
@server_private
System = {{

#<Ifstatic:OPA_BACKEND_QMLJS>
#<Else>
  /**
   * {2 Wait Flags}
  **/

  WNOHANG = %%bslunix.wnohang%% : System.wait_flag
  WUNTRACED = %%bslunix.wuntraced%% : System.wait_flag

  /**
   * {2 Fork}
  **/

  /**
   * Fork.
   * Discouraged, used for quick tests only
  **/
  fork = %%bslunix.fork%% : -> System.pid

  /**
   * Wait until one of the children processes die,
   * and return its pid and termination status.
  **/
  wait = %%bslunix.wait%% : -> (System.pid, System.process_status)

  /**
   * Same as {!System.wait} , but waits for the child process whose pid is given.
   * A pid of -1 means wait for any child.  A pid of  0  means  wait  for  any
   * child  in  the same process group as the current process.
   * Negative pid arguments represent process groups.
   * The list of options indicates whether waitpid should return immediately
   * without waiting, or also report stopped children.
  **/
  waitpid_flags = %%bslunix.waitpid_flags%%
   : caml_list(System.wait_flag), System.pid -> (System.pid, System.process_status)

  /**
   * Same as {!System.waitpid_flags} but with no flags
  **/
  waitpid = %%bslunix.waitpid%% : System.pid -> (System.pid, System.process_status)
#<End>

  /**
   * Quit the server process immediately.
   */
  exit = %% BslSys.exit %% : int -> 'a

  /**
   * Register a function to be executed whenever the server will exit
   * Execution will take place following reverse order of registration
   */
  at_exit = @may_cps(%% BslScheduler.at_exit %%) : (-> void) -> void

  /**
   * gethostname() returns the current hostname (see system gethostname)
   */
  gethostname = %% BslSys.gethostname %% : -> string

  /**
   * gethostbyname() returns the ip (v4) for the given hostname using 'hosts' (see system gethostbyname)
   */
  gethostbyname = @may_cps(%%BslSys.gethostbyname%%) : string -> option(ip)

  /**
   * gethostsbyname() returns the ips (v4) for the given hostname using 'hosts' (see system gethostbyname)
   */
  gethostsbyname = @may_cps(%%BslSys.gethostsbyname%%) : string -> list(ip)

  /**
   * Get the current process memory usage.
   * @return the memory usage in bytes
   */
  get_memory_usage = %%bslSys.get_memory_usage%% : -> int

  /**
   * Get the curent process launch date.
   * @return the GMT launch date of the application
   */
  gmt_launch_date = Date.now_gmt()

  /**
   * [exec(command, input)]
   * acts like: echo input | command > output
   *
   * Primitive for calling an external command, and returning the string
   * built from the resulting stdout of the command, given an input to
   * produce on the stdin of the process.
   * In case of error, return the error message instead of the process output.
   * @return raw result
   */
  exec = @may_cps(%%bslSys.process.exec%%) : string, string -> string


  @private
  async_shell_exec = %%bslSys.process.async_shell_exec%% : string, string, (System.process.out->void) -> System.process

  shell_exec(command,input) =
    m = Mutex.create()
    r = Reference.create(none)
    do Mutex.lock(m)
    p = (async_shell_exec(command,input, _)){ out ->
      do Reference.set(r, some(out))
      Mutex.unlock(m)
    }
    result() =
      do Mutex.lock(m)
      r = Reference.get(r) ? do Mutex.unlock(m) error("shell_exec no result")
      do Mutex.unlock(m)
      r
    ~{p result}

}}
