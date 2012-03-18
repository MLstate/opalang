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

##register fatal: string, string -> void
##args(topic, value)
{
   if(window.console)
   {//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
      window.console.error("[OPA]", "Fatal error", topic, value);
      window.console.trace()
   }
}

##register error: string, string -> void
##args(topic, value)
{
   if(window.console)
   {//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
       window.console.error("[OPA]", topic, value);
       window.console.trace();
   }
}

##register warning: string, string -> void
##args(topic, value)
{
   if(window.console)
   {//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
       window.console.warn("[OPA]", topic, value);
       window.console.trace();
   }
}

##register notice: string, string -> void
##args(topic, value)
{
   if(window.console)//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
        window.console.log("[OPA]", topic, value)
}

##register info: string, string -> void
##args(topic, value)
{
   if(window.console)//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
         window.console.info("[OPA]", topic, value)
}

##register debug: string, 'a -> void
##args(topic, value)
{
   if(window.console)//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
         window.console.debug("[OPA]", topic, value)
}
