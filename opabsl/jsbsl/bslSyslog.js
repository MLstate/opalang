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

##register fatal: string, 'a -> void
##args(topic, value)
{
   if(window.console)
   {//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
      window.console.error("[OPA]", "Fatal error", topic, value);
      window.console.trace()
   }
}

##register error: string, 'a -> void
##args(topic, value)
{
   if(window.console)
   {//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
       window.console.error("[OPA]", topic, value);
       window.console.trace();
   }
}

##register warning: string, 'a -> void
##args(topic, value)
{
   if(window.console)
   {//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
       window.console.warn("[OPA]", topic, value);
       window.console.trace();
   }
}

##register notice: string, 'a -> void
##args(topic, value)
{
   if(window.console)//Implementation note: we need check [window.console] at each call, as debuggers can be (de)activated dynamically
        window.console.log("[OPA]", topic, value)
}

##register info: string, 'a -> void
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
