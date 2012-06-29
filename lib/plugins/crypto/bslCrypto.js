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
 * Compute the MD5 signature of a string.
 *
 * @param {!string} str A text of arbitrary length.
 * @return {!string} A 32 digits long hexadecimal string
 */
##register md5 \ `MD5.digest` : string -> string
//Implementation in md5.js -- files kept separate to simplify legalese

/**
 * Encode a string as Base 64
 *
 * @param {!string} str A text of arbitrary length.
 * @return {!string} A (longer) representation, encoded as base 64
 */
##register base64_encode \ `base64.encode` : string -> string
//Implementation in base64.js -- files kept separate to simplify legalese

/**
 * @param {!string} str A base64-encoded text
 * @return {!string} A decoded representation
 */
##register base64_decode \ `base64.decode` : string -> string
//Implementation in base64.js -- files kept separate to simplify legalese
