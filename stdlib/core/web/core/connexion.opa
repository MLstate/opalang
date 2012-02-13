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
 * {1 Types defined in this module}
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

/**
 * {1 Functions exported to the global namespace}
 */

user_id_of_string(s : string) : user_id = s


string_of_user_id(uid : user_id) : string = uid
