/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
