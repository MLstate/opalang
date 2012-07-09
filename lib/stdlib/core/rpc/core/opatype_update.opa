/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
