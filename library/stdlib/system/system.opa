/*
    Copyright © 2011, 2012 MLstate

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

/**
 * Binding with module System
 * <!> Not for casual user
**/
System = {{

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

}}
