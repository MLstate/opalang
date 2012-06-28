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

/**
 * Logging support for MongoDB driver
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * Module [MongoLog] contains support for debugging and logging for the MongoDB driver.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * Log functions for MongoDB driver.
 *
 * We can choose various logging methods but these apply globally
 * to all Mongo instances in a running program.
 **/

/**
 * Type values for MongoLog, we can log to stdout, stderr or to
 * the OPA Logger functions, or we can switch logging off altogether.
 **/
type Mongo.logtype = {stdout} / {stderr} / {logger} / {nomongolog}

MongoLog = {{

  /**
   * This Mutable value defines globally throughout the running
   * program which logging type we are using.
   **/
  logtype = Mutable.make({logger})

  @private log_(from, what, logfn, str, v) =
    do match logtype.get() with
      | {stdout} -> println("{what}{if from=="" then "" else "({from})"}: {str}")
      | {stderr} -> prerrln("{what}{if from=="" then "" else "({from})"}: {str}")
      | {logger} -> logfn(from,str)
      | {nomongolog} -> void
    v

  /** The usual logging functions **/
  info(from, str, v) = log_(from,"Info",Log.info,str,v)
  debug(from, str, v) = log_(from,"Debug",Log.debug,str,v)
  warning(from, str, v) = log_(from,"Warning",Log.warning,str,v)
  error(from, str, v) = log_(from,"Error",Log.error,str,v)

  /** A special fatal log which logs a message and terminates the running program. **/
  fatal(from, str, v) =
    do match logtype.get() with
      | {stdout} -> println("Fatal{if from=="" then "" else "({from})"}: {str}")
      | {stderr} -> prerrln("Fatal{if from=="" then "" else "({from})"}: {str}")
      | {logger} -> Log.fatal(from,str)
      | {nomongolog} -> void
    @fail("{v}")

}}

// End of file log.opa
