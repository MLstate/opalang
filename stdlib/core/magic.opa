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
 * Magic interaction with the type system.
 *
 * @category compiler
 * @destination private
 * @stability stable
**/

/**
 * A black type, e.g. for gathering heterogenous data inside a typed structure.
**/
type black = external

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
