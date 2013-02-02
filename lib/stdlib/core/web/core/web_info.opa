/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin server

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

type HttpRequest.request =
    { request : WebInfo.private.native_request;
      connexion : WebInfo.private.native_connection;
    }

type http_request = HttpRequest.request

type web_info = {
  cont:          WebInfo.private.native_response -> void        /**A continuation, used to return content to the user.*/
  http_request:  HttpRequest.request    /**The request.*/
}


@abstract type WebInfo.private.native = external
@abstract type WebInfo.private.native_connection = external
@abstract type WebInfo.private.native_request = external
@abstract type WebInfo.private.native_response = external
@abstract type WebInfo.private.native_ip = external

/**
 * {1 Interface}
 */

WebInfo = {{

    @private web_info_cont = %%BslNet.Http_server.web_info_cont%% : WebInfo.private.native -> (WebInfo.private.native_response -> void)
    @private web_info_request = %% BslNet.Http_server.web_info_request %% : WebInfo.private.native -> WebInfo.private.native_request
    @private web_info_conn    = %% BslNet.Http_server.web_info_conn    %% : WebInfo.private.native -> WebInfo.private.native_connection
    @private web_info_ip = %%BslNet.Http_server.ip_of_web_info%%

    of_native_web_info(winfo:WebInfo.private.native) =
      req : HttpRequest.request =
        { request = web_info_request(winfo);
          connexion = web_info_conn(winfo);
        }
      { cont         = web_info_cont(winfo)
        http_request = req
      } : web_info

    to_native_web_info(winfo:web_info) =
          reconstruct = %% BslNet.Http_server.web_info_reconstruct %%
          reconstruct(winfo.cont, winfo.http_request.request, winfo.http_request.connexion)

    get_conn_ip(conn) =
      IPv4.ip_of_string(web_info_ip(conn))

    simple_reply(winfo:web_info, msg, status) =
      winfo.cont(
        WebCoreExport.default_make_response(
          {volatile}, winfo.http_request.request, status,
            "text/plain", {next= -> {some=(binary_of_string(msg), {next=->{none}})}})
      )
}}

