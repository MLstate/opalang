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

