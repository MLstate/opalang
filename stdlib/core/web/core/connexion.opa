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

@abstract type user_id = string

/**
 * A simple handler for a family of urls.
 *
 * The usual way of defining a [simple_url_handler] is
 *
 * [
 *   parser "/my/first/url" -> some_result
 *       |  "/my/second/url"-> some_other_result
 *       |  some_grammar    -> yet_another_result
 * ]
 */
type simple_url_handler('result) = Parser.general_parser('result)


/**
 * A handler for a family of urls.
 *
 * This is the most general form of URL handlers. The usual way of defining a [url_handler] is
 * [
 *    parser  "/my_first_url" -> id -> some_result
 *         |  "/my_second_url"-> id -> some_other_result
 *         |   some_grammar   -> id -> yet_another_result
 * ]
 *
 * where [id] is the connexion id of the user.
 */
type url_handler('result)  =  Parser.general_parser(HttpRequest.request -> 'result)


user_id_of_string(s : string) : user_id = s


string_of_user_id(uid : user_id) : string = uid
