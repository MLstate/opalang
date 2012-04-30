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

//////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARING BEETWEEN THE JAVASCRIPT AND NODE BSL //
//////////////////////////////////////////////////////////////////////

##register fatal: string, string -> void
##args(topic, value)
{
   if(console)
   {//Implementation note: we need check [console] at each call, as debuggers can be (de)activated dynamically
      console.error("[OPA]", "Fatal error", topic, value);
      console.trace()
   }
}

##register error: string, string -> void
##args(topic, value)
{
   if(console)
   {//Implementation note: we need check [console] at each call, as debuggers can be (de)activated dynamically
       console.error("[OPA]", topic, value);
       console.trace();
   }
}

##register warning: string, string -> void
##args(topic, value)
{
   if(console)
   {//Implementation note: we need check [console] at each call, as debuggers can be (de)activated dynamically
       console.warn("[OPA]", topic, value);
       console.trace();
   }
}

##register notice: string, string -> void
##args(topic, value)
{
   if(console)//Implementation note: we need check [console] at each call, as debuggers can be (de)activated dynamically
        console.log("[OPA]", topic, value)
}

##register info: string, string -> void
##args(topic, value)
{
   if(console)//Implementation note: we need check [console] at each call, as debuggers can be (de)activated dynamically
         console.info("[OPA]", topic, value)
}

##register debug: string, 'a -> void
##args(topic, value)
{
   if(console)//Implementation note: we need check [console] at each call, as debuggers can be (de)activated dynamically
         console.debug("[OPA]", topic, value)
}
