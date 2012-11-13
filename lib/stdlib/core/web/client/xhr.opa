/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Options for XHR request.
 */
type XHR.options('content) = {
    string method,
    option('content) content,
}

/**
 * Result of XHR request.
 * - [success] means that request has been sent and a result has been received (not
 *  necessarily with HTTP code 200)
 * - [failure] means that an error occurs.
 */
type XHR.result('content) = outcome(XHR.success('content), string)

/**
 * Describes a success of the XHR response.
 */
type XHR.success('content) = {
    int code,
    'content content,
    list(string) headers,
    string -> option(string) header_get,
}

/**
 * A WebClient based on the browser side.
 *
 * Note: If you want to request a non-relative URI you *should* set the
 * Access-Control-Allow-Origin header.
 */
module XHR {

    /**
     * Default options for XHR request.
     */
    XHR.options(string) default_options = {method : "GET", content : none}

    /**
     * Synchronous request with [options] to the [uri].
     */
    function XHR.result(string) request(Uri.uri uri, XHR.options(string) options){
        %%BslClientOnly.XHR.request%%(Uri.to_string(uri), options)
    }

    /**
     * Asynchronous request with [options] to the [uri].
     */
    function request_async(Uri.uri uri, XHR.options(string) options, (XHR.result(string) -> void) callback){
        %%BslClientOnly.XHR.request_async%%(Uri.to_string(uri), options, callback)
    }

    /**
     * Synchronous GET request to the [uri].
     */
    function XHR.result(string) get(Uri.uri uri){
        request(uri, default_options)
    }

    /**
     * Asynchronous GET request to the [uri].
     */
    function get_async(Uri.uri uri, callback){
        request_async(uri, default_options, callback)
    }

    /**
     * Synchronous POST request to the [uri].
     */
    function XHR.result(string) post(Uri.uri uri, content){
        request(uri, { default_options with method : "POST", ~content })
    }

    /**
     * Asynchronous POST request to the [uri].
     */
    function post_async(Uri.uri uri, content, callback){
        request_async(uri, { default_options with method : "POST", ~content }, callback)
    }

}
