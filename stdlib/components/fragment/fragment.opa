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

package stdlib.components.fragment

/**
 * @category COMPONENT
 * @author Adam Koprowski, 2011
 * @stability EXPERIMENTAL
 */

type CFragment.action('state) =
  / { no_op }
  / { re_render : xhtml }
  / { change_state : 'state }
  / { re_render : xhtml; change_state : 'state }

@abstract type CFragment.fragment('msg) = channel('msg)

CFragment =
{{

  @private on_notification(id, handler, state, msg) =
    re_render(xhtml) = Dom.transform([#{id} <- xhtml])
    match handler(state, msg) with
    | {no_op} ->
        {unchanged}
    | {re_render=xhtml} ->
        do re_render(xhtml)
        {unchanged}
    | {change_state=new_state} ->
        {set=new_state}
    | {re_render=xhtml; change_state=new_state} ->
        do re_render(xhtml)
        {set=new_state}

  create(init_state : 'state, init_xhtml : xhtml,
         handler : 'state, 'msg -> CFragment.action
        ) : (xhtml, CFragment.fragment('t)) =
    id = Dom.fresh_id()
    xhtml = <span id={id}>{init_xhtml}</>
    session = Session.make(init_state, on_notification(id, handler, _, _))
    (xhtml, session)

  notify(fragment : CFragment.fragment('msg), msg : 'msg) : void =
    Session.send(fragment, msg)

}}
