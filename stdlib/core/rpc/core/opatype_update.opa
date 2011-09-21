/*
    Copyright © 2011 MLstate

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
