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
  debug   = %% BslSyslog.debug %%
}}

/**
 * All parameters are the same:
 * @param topic
 * @param message
 * Example:
 * [Log.fatal("topic", "message")]
 */
Log =
  @sliced_expr({
  server = ServerLog

  client = Clientlog
})