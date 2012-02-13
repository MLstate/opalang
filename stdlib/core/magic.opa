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
