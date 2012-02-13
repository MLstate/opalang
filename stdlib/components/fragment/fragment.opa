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
