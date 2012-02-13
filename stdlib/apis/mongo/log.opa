/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package stdlib.apis.mongo

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
