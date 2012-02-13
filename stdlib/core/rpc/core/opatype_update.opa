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
 * {1 About this module}
 *
 * It updates the function OpaTsc.get to support fallback on the server OpaTsc.get from the client side
 *
 */

@server
@private
OpaTsc_server_get(ident) = OpaTsc_get.get(ident)

@client
UpdateOpaTsc = {{

/* Remark: evaluating the pros (less request) and the cons (send too much type or need client state)
   of pre-sending dependencies of the current asked type => don't do it */
get_fallback(ident) =
  add = %%BslValue.Tsc.add%%
  r = OpaTsc_server_get(ident)
  do Option.iter(def->add(ident,def),r)
  r

/* plug the new get function */
process_update =
  do OpaTsc_get.update_fallback(UpdateOpaTsc.get_fallback)
  void

}}
