/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
