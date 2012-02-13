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

/**
 * All parameters are the same:
 * @param topic
 * @param message
 * Example:
 * [Log.fatal("topic", "message")]
 */

Log = {{
  fatal   = %% BslSyslog.fatal %%
  error   = %% BslSyslog.error %%
  warning = %% BslSyslog.warning %%
  notice  = %% BslSyslog.notice %%
  info    = %% BslSyslog.info %%
  debug   = %% BslSyslog.debug %%
}}

/**
 * {1 Deprecated. Use Log.}
 */

@deprecated({use="Log"}) Syslog = Log
