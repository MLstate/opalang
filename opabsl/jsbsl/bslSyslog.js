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

////////////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARED BEETWEEN THE CLIENT JAVASCRIPT AND NODE BSL //
////////////////////////////////////////////////////////////////////////////

/**
 * Implementation note: we need to check [console] at each call, as debuggers can be (de)activated dynamically 
 */

##register fatal: string, string -> void
##args(topic, value)
{
   if (console)
       console.error("[Opa]", "Fatal error", topic, value);
}

##register error: string, string -> void
##args(topic, value)
{
   if (console)
       console.error("[Opa]", topic, value);
}

##register warning: string, string -> void
##args(topic, value)
{
   if (console)
       console.warn("[Opa]", topic, value);
}

##register notice: string, string -> void
##args(topic, value)
{
   if (console)
       console.log("[Opa]", topic, value);
}

##register info: string, string -> void
##args(topic, value)
{
   if (console)
       console.info("[Opa]", topic, value);
}

##register debug: string, 'a -> void
##args(topic, value)
{
   if (console)
       console.log("[Opa]", topic, value); // we do not use debug, because it is deprecated since Gecko 5.0
}
