/*
    Copyright © 2011, 2012 MLstate

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
 * Binary data handling: for any sequence of bytes that is not to be considered
 * as text.
 *
 * @destination PRIVATE
 * @stability UNSTABLE
 */

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

//TODO:This should be [external]. However, for the moment, we rely on [static_file_include] and we just can't.
@opacapi
@abstract
type binary = external

/**
 * {1 Interface}
 */

string_of_binary(x:binary) = @unsafe_cast(x): string
binary_of_string(x:string) = @unsafe_cast(x): binary

@opacapi
bin_of_base64(x:string):binary = %%bslPervasivesServer.bin_of_base64%%(x)
