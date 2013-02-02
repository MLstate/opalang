/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/**
 * {1 Types defined in this module}
 */

/**
 * The type of mutable element.
 * @param 'a The type contained in the element.
 * get returns the content of the mutable element.
 * set changes the content of the mutable element.
 */
type Mutable.t('a) = {get: -> 'a; set: 'a -> void}

/**
 * {1 About this module}
 *
 * It provides a way to have a mutable state.
 *
 * e.g.
 *
 * state = Mutable.create("initial state content")
 * println(state.get())
 * state.set("new state content")
 *
 */
Mutable =
{{
   make(init:'a): Mutable.t('a) =
        r = Reference.create(init)
        {{
           get() = Reference.get(r)
           set(x)= Reference.set(r, x)
        }}

}}

@opacapi
Mutable_make = Mutable.make

@opacapi Mutable_set(mutable,v) : Mutable.t('a) = mutable.set(v)
