/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
