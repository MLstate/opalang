/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Magic interaction with the type system.
 *
 * @category compiler
 * @destination private
 * @stability stable
**/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * A black type, e.g. for gathering heterogenous data inside a typed structure.
**/
type black = external

/**
 * {1 Interface}
 */

Magic = {{

  /**
   * Introduce a [Obj.magic] on the Ocaml generated code.
   * Not for casual user.
   * If you want to bypass the opa typer, you should use @unsafe_cast instead.
   * Introducing an @unsafe_cast has no impact on the generated code, the
   * directive is simply discarded by the low level code generator.
  **/
  id    = %%BslPervasives.Magic.id%%

  /**
   * Transforming a value into its black representation.
  **/
  black = %%BslPervasives.Magic.id%% :'a -> black
}}
