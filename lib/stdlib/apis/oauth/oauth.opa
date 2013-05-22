/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * OAuth modules
 *
 * This file provides methods needed to connect and communicate using OAuth.
 *
 * @category web
 * @author Nicolas Glondu, 2010
 * @destination
 * - private: OAuth_private
 * - public: OAuth
 */

import stdlib.apis.common
import stdlib.crypto
import stdlib.core.web.client

/**
 * Type of an OAuth signature
 */
type OAuth.signature_type =
     { PLAINTEXT } /** Plain text signature - Not recommended unless you have a https connection  */
   / { HMAC_SHA1 } /** HMAC-SHA1 signature */

type OAuth.method = {POST} / {GET} / {PUT:{mimetype:string file:binary}}

/**
 * OAuth configuration. All these parameters shoul be given
 * to you after subscribing to an OAuth service.
 * - [inlined_auth] Used to put the OAuth parameters with normal parameters
 * instead of in the authorization field. Should be set to [false] in most cases.
 * - [http_method] Method used for the request. The RFC recommands {POST} for the
 * authentication flow. You should consult the API of the accessed service to
 * know what to use.
 */
type OAuth.parameters = {
  consumer_key      : string
  consumer_secret   : string
  auth_method       : OAuth.signature_type
  request_token_uri : string
  access_token_uri  : string
  authorize_uri     : string
  http_method       : OAuth.method
  inlined_auth      : bool
  custom_headers    : list(string)
}

type OAuth.token = {
  token    : string
  secret   : string
  verifier : string
}

type OAuth.token_res = { success : OAuth.token } / { error : string }

@private OAuth_private(p:OAuth.parameters) =
{{

  /* Result format : oauth_token=requestkey&oauth_token_secret=requestsecret&... */
  build_result(res) : OAuth.token_res =
    data = API_libs.get_data(res)
    token = API_libs.get_field(data, "oauth_token")
    secret = API_libs.get_field(data, "oauth_token_secret")
    verifier = API_libs.get_field(data, "oauth_verifier")
    if token == "" then { error=res }
    else
      success = { ~token ~secret ~verifier } : OAuth.token
      { ~success }

  identity(res) = res

  method_to_string(m) =
    match m : OAuth.method with
    | {POST}  -> "POST"
    | {GET}   -> "GET"
    | {PUT=_} -> "PUT"

  /* ------------------- */
  /* Signature functions */
  /* ------------------- */

  build_base_string(uri, params, auth_params) =
    // params = List.map((k,v)->(k,API_libs_private.url_encoder(v)), params)
    aux0(acc, (pname, pval)) = Set.add((pname,pval), acc)
    setorder = List.fold_left(aux0, Set.empty, auth_params)
    setorder = List.fold_left(aux0, setorder, params)
    aux1((pname, pval), acc) =
      pval = API_libs_private.url_encoder(pval)
      if (acc == "") then "{pname}={pval}"
      else "{acc}&{pname}={pval}"
    str_params = Set.fold(aux1, setorder, "")
    str_params = API_libs_private.url_encoder(str_params)
    uri = API_libs_private.url_encoder(uri)
    "{method_to_string(p.http_method)}&{uri}&{str_params}"

  hmac_sha1_sign(secret, uri, params, auth_params) =
    base_string = build_base_string(uri, params, auth_params)
    do API_libs_private.apijlog("Base string: {base_string}")
    key = Binary.of_string("{p.consumer_secret}&{secret}")
    res = Crypto.Base64.encode(Crypto.HMAC.sha1(key, Binary.of_string(base_string)))
    res = API_libs_private.url_encoder(res)
    do API_libs_private.apijlog("Signature: {res}")
    res

  signature_to_string(s) =
    match s : OAuth.signature_type with
    | {PLAINTEXT} -> "PLAINTEXT"
    | {HMAC_SHA1} -> "HMAC-SHA1"

  sign_request(secret, uri, params, auth_params) =
    auth_params = List.cons(("oauth_signature_method",
                             signature_to_string(p.auth_method)), auth_params)
    signature = match p.auth_method : OAuth.signature_type with
      | {PLAINTEXT} ->
        API_libs_private.url_encoder("{p.consumer_secret}&{secret}")
        //"{API_libs_private.url_encoder(p.consumer_secret)}&{API_libs_private.url_encoder(secret)}"
      | {HMAC_SHA1} -> hmac_sha1_sign(secret, uri, params, auth_params)
    List.cons(("oauth_signature", signature), auth_params)

  /* ------------------ */
  /* Download functions */
  /* ------------------ */

  build_auth_text(uri:string, auth_params) =
    auth_params = List.cons(("OAuth realm", uri), auth_params)
    //auth_params = List.cons((String.replace("Dbg","","OAuth realm"), uri), auth_params) // Debug feature, to be deleted.
    aux((pname:string, pval:string), acc:string) =
      if acc == "" then "{pname}=\"{pval}\""
      else "{acc}, {pname}=\"{pval}\""
    List.fold(aux, auth_params, "")

  build_basic_params() =
    nonce = Random.int(Limits.max_int)+Limits.max_int/10
    timestamp = Date.in_milliseconds(Date.now()) / 1000
    [("oauth_consumer_key", p.consumer_key), ("oauth_timestamp", "{timestamp}"),
     ("oauth_nonce", "{nonce}"), ("oauth_version", "1.0")]

  get_res_2(more_auth, secret, uri, params) =
    auth_params = build_basic_params()
      |> List.append(_, more_auth)
      |> sign_request(secret, uri, params, _)

    aux(acc:string, (pname:string, pval:string)) =
      pval =
        if pname == "oauth_signature" && p.auth_method == {HMAC_SHA1} then pval
        else API_libs_private.url_encoder(pval)
      if (acc == "") then "{pname}={pval}"
      else "{acc}&{pname}={pval}"
    params_text =
      List.fold_left(aux, "", params)
      |> (if p.inlined_auth then
            List.fold_left(aux, _, auth_params)
          else identity)
    do API_libs_private.apijlog("Parameters: {params_text}")

    auth =
      if p.inlined_auth then {none}
      else {some=build_auth_text(uri, auth_params)}
    do API_libs_private.apijlog("Auth: \n{auth?""}")

    res = match p.http_method with
    | {GET} ->
      uri = if params_text == "" then uri else "{uri}?{params_text}"
      options = {WebClient.Get.default_options with
        ~auth custom_headers=p.custom_headers}
      do API_libs_private.apijlog("URI: \n{uri}")
      match Uri.of_string(uri) with
      | {none} -> error("_______")
      | {some=u} -> WebClient.Get.try_get_with_options(u, options)
      end
    | {PUT=~{mimetype file}} ->
      uri = if params_text == "" then uri else "{uri}?{params_text}"
      options = {WebClient.Put.default_options with
        ~auth ~mimetype custom_headers=p.custom_headers}
      do API_libs_private.apijlog("URI: \n{uri}")
      match Uri.of_string(uri) with
      | {none} -> error("_______")
      | {some=u} -> WebClient.Put.try_put_with_options(u, string_of_binary(file), options)
      end
    | {POST} ->
      do API_libs_private.apijlog("URI: \n{uri}")
      options = {WebClient.Post.default_options with
                    custom_headers=p.custom_headers
                    auth=auth
                    content=(if params_text == "" then {none}
                             else {some=params_text})}
      match Uri.of_string(uri) with
      | {none} -> error("_______")
      | {some=u} -> WebClient.Post.try_post_with_options(u, options)
      end
    // do API_libs_private.apijlog("Result: '''{res}'''")
    res

  get_res(more_auth, secret, uri, params, parse_fun) =
    match get_res_2(more_auth, secret, uri, params) with
    | {failure=_} -> parse_fun("")
    | {success=s} -> parse_fun(s.content)
    end

  get_request_token(callback) =
    more_auth =
      if (callback != "") then
        [("oauth_callback", callback)]
      else []
    get_res(more_auth, "", p.request_token_uri, [], build_result)

  get_access_token(request_key, request_secret, verifier) =
    more_auth = [("oauth_token", request_key)]
    more_auth =
      if (verifier != "") then
        List.cons(("oauth_verifier", verifier), more_auth)
      else more_auth
    get_res(more_auth, request_secret, p.access_token_uri, [], build_result)

  get_protected_resource(uri, params, access_key, access_secret) =
    more_auth = match access_key
      | "" -> []
      | _ -> [("oauth_token", access_key)]
    get_res(more_auth, access_secret, uri, params, identity)

  get_protected_resource2(uri, params, access_key, access_secret) =
    more_auth = match access_key
      | "" -> []
      | _ -> [("oauth_token", access_key)]
    get_res_2(more_auth, access_secret, uri, params)

}}

/**
 * OAuth module
 *
 * This module contains all the functons needed to establish a OAuth connection.
 * Note : Only PLAINTEXT and HMAC_SHA1 are currently available as signature methods. Some servers
 *      may not accept PLAINTEXT due to it's lack of security when used through a non-secured channel.
 *
 * @param parameters The [OAuth.parameters] provided by the server you're accessing
 */
OAuth(parameters:OAuth.parameters) = {{
  @private p = parameters

 /**
  * A function to request a token to a OAuth server
  *
  * First part of the OAuth flow. You should give your user a link using the key resulting
  * of this request and the authorize url of the OAuth server (using OAuth.build_authorize_url
  * or building this simple address yourself).
  *
  * @param callback_url Url where a user will be redirected after he gives access to your server. Note: Some server include this in their configuration, in this case, you should provide the same address or leave it empty.
  */
  get_request_token(callback_url : string) =
    OAuth_private(p).get_request_token(callback_url)

 /**
  * A function to build the url where an user should go to authorize your application
  *
  * Second part of the OAuth flow. Your user should use this url to authorize your application
  * on the OAuth server. After accepting your application, the user will be redirected to the
  * callback_url you gave to the server with a verifier and the same token.
  *
  * @param request_token A valid request token obtained with OAuth.get_request_token (each token is single use)
  */
  build_authorize_url(request_token : string) =
    "{p.authorize_uri}?oauth_token={request_token}"

 /**
  * A function to parse the data returned when the user is redirected to your website
  *
  * Part of the second part of the OAuth flow. You can include this directly into the
  * url parser of your website.
  *
  * @param connection_data Host of the OAuth server you want to access
  */
  connection_result(connection_data : string) =
    OAuth_private(p).build_result(connection_data)

 /**
  * A function to transform a request token into an access token
  *
  * Third and last part of the OAuth flow.
  *
  * @param request_token Request token to transform.
  * @param request_secret Secret associated to the token to transform.
  * @param verifier Verifier associated to the token to transform (leave empty if the OAuth server did not provide any verifier).
  */
  get_access_token(
    request_token : string,
    request_secret : string,
    verifier : string
  ) =
    OAuth_private(p).get_access_token(
      request_token, request_secret, verifier
    )

 /**
  * A function to access a protected resource using an access token
  *
  * Last part of the OAuth flow.
  *
  * @param uri Address of the protected resource
  * @param params A (key,value) list of parameters for the request
  * @param access_token Access token of your user
  * @param access_secret Access secret associated to the token
  */
  get_protected_resource(
    uri : string,
    params : list((string, string)),
    access_token : string,
    access_secret : string
  ) =
    OAuth_private(p).get_protected_resource(
      uri, params, access_token, access_secret
    )

  get_protected_resource_2(
    uri : string,
    params : list((string, string)),
    access_token : string,
    access_secret : string
  ) =
    OAuth_private(p).get_protected_resource2(
      uri, params, access_token, access_secret
    ) : outcome

}}
