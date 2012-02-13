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
}}

