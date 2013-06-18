/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Log module
 *
 * This module provides a low-level API to manage warning, error, notice, ...
 *
 * @author Nicolas Pelletier, 2010
 * @author David Rajchenbach-Teller, 2010-2011 (full rewrite)
 * @target PUBLIC
 * @stability UNKNOWN
 */

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

@private
ServerLog = {{
  @private wrap(l)(topic:string, x:string) = l("{topic} {x}"):void
  @publish fatal   = wrap(Logger.critical)
  @publish error   = wrap(Logger.error   )
  @publish warning = wrap(Logger.warning )
  @publish notice  = wrap(Logger.notice  )
  @publish info    = wrap(Logger.info    )
  @publish debug(t, x) = wrap(Logger.debug   )(t, "{x}")
}}

@private
Clientlog = {{
  fatal   = %% BslSyslog.fatal %%
  error   = %% BslSyslog.error %%
  warning = %% BslSyslog.warning %%
  notice  = %% BslSyslog.notice %%
  info    = %% BslSyslog.info %%
  debug(t, x) =
    __hack_for_ei = @typeof(x)
    %% BslSyslog.debug %%(t, x)
}}

/**
 * All parameters are the same:
 * @param topic
 * @param message
 * Example:
 * [Log.fatal("topic", "message")]
 */
@workable
Log =
  @sliced_expr({
  server = ServerLog

  client = Clientlog
})
