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

import stdlib.core.{date}

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

/**
 * The status of a web request.
 *
 * You will need to use this type only if you wish to customize error messages.
 * In this case, you should create your error pages with function [error] rather than
 * function [html].
 * Use [WebUtils.web_response_of_code] to obtain a [web_response] from an int
 * representing the error code.
 */
type web_response =
    {continue}                        /** 100 */
  / {switching_protocols}             /** 101 */

  / {success}                         /** 200 - No error */
  / {created}                         /** 201 */
  / {accepted}                        /** 202 - Request accepted. The server may perform further actions. */
  / {non_authoritative_information}   /** 203 */
  / {no_content}                      /** 204 */
  / {reset_content}                   /** 205 */
  / {partial_content}                 /** 206 */

  / {multiple_choices}                /** 300 */
  / {address_moved}                   /** 301 - This page has moved permanently. */
  / {found}                           /** 302 */
  / {see_other}                       /** 303 */
  / {not_modified}                    /** 304 */
  / {use_proxy}                       /** 305 */
  / {address_redirected}              /** 307 - This URL has been temporarily redirected. */

  / {bad_request}                     /** 400 */
  / {unauthorized}                    /** 401 */
  / {payment_required}                /** 402 */
  / {forbidden}                       /** 403 - Request rejected for security reasons. */
  / {wrong_address}                   /** 404 - URL not found. */
  / {method_not_allowed}              /** 405 */
  / {not_acceptable}                  /** 406 */
  / {proxy_authentication_required}   /** 407 */
  / {request_timeout}                 /** 408 */
  / {conflict}                        /** 409 */
  / {gone}                            /** 410 */
  / {length_required}                 /** 411 */
  / {precondition_failed}             /** 412 */
  / {request_entity_too_large}        /** 413 */
  / {request_uri_too_large}           /** 414 */
  / {unsupported_media_type}          /** 415 */
  / {requested_range_not_satisfiable} /** 416 */
  / {expectation_failed}              /** 417 */

  / {internal_server_error}           /** 500 */
  / {not_implemented}                 /** 501 */
  / {bad_gateway}                     /** 502 */
  / {service_unavailable}             /** 503 */
  / {gateway_timeout}                 /** 504 */
  / {http_version_not_supported}      /** 505 */

type web_cache_control =  {volatile}    /** The resource changes at each request, remembering it on the client is pointless, possibly unsafe.
                                        This setting is always safe, but often suboptimal.
                                        Used mostly when defining real-time web services.*/

                    / {modified_on : Date.date}/** The resource may change at a future date. The client should remember it, but should also ask
                                                   the server if the resource has changed, just in case.

                                                   This setting is generally safe, and generally a good compromise between performance and safety.
                                                   This is the default setting for all resources for which it is known to be safe.

                                                   Used generally in combination of [Dynamic_resource].*/

                    / {check_for_changes_after: Duration.duration}
                                    /** The resource changes, but not very often. It can be remembered by the client for a limited time,
                                        hence saving valuable bandwidth and server resources.

                                        You should use this essentially for resources which are not critical, i.e. if you don't mind that
                                        the user sees a version a few minute or a few hours old, rather than the latest version.
                                        Set the duration appropriately -- and remember that the old version of the resource may still be
                                        remembered by the client even if you upgrade your server. If you require insurance that the
                                        version will be upgraded with your server, you should use this setting in combination with
                                        [Dynamic_resource.publish] or [Dynamic_resource.custom_publish] and an expiration of [{none}].*/

                    / {permanent}   /** Advanced setting: the resource never changes. It can be remembered by the client,
                                        hence saving valuable bandwidth and server resources.

                                        Note: Unless you wish your users to have and press the 'reload' button a few times
                                        to see the latest version of the resource, you should use this setting {e only} in combination
                                        with [Dynamic_resource.publish] or [Dynamic_resource.custom_publish] and an expiration of [{none}].*/

type web_server_status = external
type WebInfo.private.native_http_header = external

type Resource.cookie_attributes =
    { comment:string }
  / { domain:string }
  / { max_age:int }
  / { path:string }
  / { secure:void }
  / { version:int }

type Resource.cookie_def = {
  name : string ;
  value : string ;
  attributes : list(Resource.cookie_attributes) ;
}

type Resource.http_response_header =
    { set_cookie:Resource.cookie_def }
  / { age:int }
  / { location:string }
  / { retry_after: { date:string } / { delay:int } }
  / { server: list(string) }
  / { content_disposition : { attachment : string } }

type Resource.http_general_header =
    { lastm : web_cache_control }

type Resource.http_header = Resource.http_general_header / Resource.http_response_header

/**
 * {1 Interface}
 */

WebCoreExport =
{{

/**
 * The date at which this executable was launched
 */
@private @both_implem startup_date : option = { some = Date.now() }

@private ll_setcookie : string -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.set_cookie%%
@private ll_cache_control : string -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.cache_control%%
@private ll_expires_at : option(time_t) -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.expires_at%%
@private ll_lastm : time_t -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.last_modified%%
@private ll_pragma : string -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.pragma%%
@private ll_cdisp_attachment : string -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.cdisp_attachment%%
@private ll_location : string -> WebInfo.private.native_http_header = %%BslNet.ConvertHeader.location%%

/**
 * Escape header field content.
 *
 * @note : According to RFC2616 CTL characters can be escaped if it are
 * inside quoted string. But in real and cruel world, browsers
 * doesn't take care CTL escaping. Therefore just remove it from content
 */
@private escape_string_header(h) =
  // Remove CTLS see above note
  CTL = parser c=. -> if c > 31 && c != 127 then Text.from_character(c)
                       else Text.cons("")
  LWS = parser x=([\r][\n] ([ ]|[\t])+) -> x
  TEXT = parser | ~LWS -> LWS
                | ~CTL -> CTL
  field_content = parser x=TEXT* -> Text.ltconcat(x)
  Option.map(Text.to_string, Parser.try_parse(field_content, h))
  ? "unsafe field content - should never happends"

@private add_ll_header(header : Resource.http_header, lst : list(WebInfo.private.native_http_header)) =
  // /////////////////////////////////////////////////////////////////
  // /!\ BEWARE - IF YOU ADD AN HEADER CASE KEEP IN MIND THAT VALUE //
  // /!\ MUST BE ESCAPED                                            //
  // /////////////////////////////////////////////////////////////////
  match header with
  | ~{set_cookie} ->
    // Escape
    escape_set_cookie(sc) = { sc with
      name=escape_string_header(sc.name)
      value=escape_string_header(sc.value)
      attributes=List.map(
          | ~{comment} -> {comment = escape_string_header(comment)}
          | ~{domain} -> {domain = escape_string_header(domain)}
          | ~{path} -> {path = escape_string_header(path)}
          | e -> e
        , sc.attributes)
    }
    set_cookie = escape_set_cookie(set_cookie)
    [ ll_setcookie(cookie_def_to_string(set_cookie)) | lst ]

  | ~{location} ->
    // Escape
    [ ll_location(escape_string_header(location)) | lst ]

  | {content_disposition = ~{attachment}} ->
    // Escape
    [ ll_cdisp_attachment(escape_string_header(attachment)) | lst ]

  | ~{lastm} ->
      match lastm with
      | {volatile} -> [ ll_cache_control("no-cache") , ll_pragma("no-cache") | lst ]
      | ~{modified_on}  -> [ ll_cache_control("public") , ll_lastm(Date.ll_export(modified_on)) | lst ]
      | {permanent} ->
           match startup_date with
           | {none} -> [ ll_expires_at({none}) | lst ]
           | ~{some} ->
              [ ll_expires_at({none}), ll_cache_control("public"),
                ll_lastm(Date.ll_export(some)) | lst ]
           end
      | {check_for_changes_after = duration } ->
           now = Date.now()
           expiry = Date.advance(now, duration)
           te = Date.ll_export(_)
           [ ll_expires_at({some = te(expiry)}), ll_cache_control("public") ,
              ll_lastm(te(now)) | lst ]
      end
  // TODO - WHY THESE CASES ARE IGNORED??
  | _ -> lst

@private cookie_def_to_string(cd) =
  g = List.fold(x, acc -> Text.insert_right(acc, "; ") |> Text.insert_right(_, cookie_attribute_to_string(x)), cd.attributes, Text.cons(""))
  "{cd.name}={cd.value}{g}"

@private cookie_attribute_to_string(ca) =
  match ca with
  | ~{comment} -> "comment=" ^ comment
  | ~{domain} -> "domain=" ^ domain
  | ~{max_age} -> "max-age={max_age}"
  | ~{path} -> "path=" ^ path
  | {secure} -> "secure"
  | ~{version} -> "version={version}"

@private to_ll_headers(headers : list(Resource.http_header)) : list(WebInfo.private.native_http_header) =
  List.foldl(add_ll_header, headers, [])

/**
 * Prepare a low-level response
 */

make_response_with_headers(request : WebInfo.private.native_request,
                           status : web_response,
                           headers : list(Resource.http_header),
                           mime_type : string,
                           content : string) : WebInfo.private.native_response =
(
     cache_control = // Ugly and redundant, here for legacy reasons
        check(x) = match x with | { lastm = _ } -> true | _ -> false end
        match Option.get(List.find(check, headers)) with
        | ~{lastm} ->
            match lastm with
            | {volatile}  -> {none}
            | ~{modified_on} -> {some = Date.ll_export(modified_on)}
            | {permanent} -> Option.map(Date.ll_export, startup_date)
            | {check_for_changes_after = _ } -> {some = Date.ll_export(Date.now()) }
            end
        | _ -> {none}
        end

    respond = %% BslNet.Http_server.make_response %%
            : option(time_t), WebInfo.private.native_request, web_server_status, caml_list(WebInfo.private.native_http_header), string, string -> WebInfo.private.native_response
    to_caml_list : (WebInfo.private.native_http_header
                      -> WebInfo.private.native_http_header),
                   list(WebInfo.private.native_http_header)
                   -> caml_list(WebInfo.private.native_http_header) =
      %% BslNativeLib.opa_list_to_ocaml_list %%
    ll_headers = to_caml_list((x -> x), to_ll_headers(headers))
    answer = web_err_num_of_web_response(status)
    respond(cache_control, request, answer, ll_headers, mime_type, content)
)

default_make_response(cache_control: web_cache_control, request: WebInfo.private.native_request, status: web_response, mime_type: string, content: string) : WebInfo.private.native_response =
(
     make_response_modified_since = %% BslNet.Http_server.make_response_modified_since %%
                              : option(time_t), WebInfo.private.native_request, web_server_status, string, string -> WebInfo.private.native_response
     make_response_expires_at = %% BslNet.Http_server.make_response_expires_at %%
                              : option(time_t), option(time_t), WebInfo.private.native_request, web_server_status, string, string -> WebInfo.private.native_response
     match cache_control with
        | {volatile}  ->     //Possibility 1: should never be cached
             make_response_modified_since({none}, request, web_err_num_of_web_response(status), mime_type, content)
        | ~{modified_on}  -> //Possibility 2: should be retransmitted only if the client cache is out of date
             make_response_modified_since({some = Date.ll_export(modified_on)}, request, web_err_num_of_web_response(status), mime_type, content)
        | {permanent} ->     //Possibility 3: should be cached forever
             startup = Option.map(Date.ll_export, startup_date)
             make_response_expires_at({none}, startup, request, web_err_num_of_web_response(status), mime_type, content)
        | {check_for_changes_after = duration }    ->     //Possibility 4: should be cached for a given time
             now = Date.now()
             expiry = Date.advance(now, duration)
             te = Date.ll_export(_)
             make_response_expires_at({some = te(expiry)}, {some = te(now)}, request, web_err_num_of_web_response(status), mime_type, content)
)

/**
 * {2 Manipulating status}
 */

web_err_num_of_web_response =
 WebStatus = {{
   continue = %% BslNet.Requestdef.sc_Continue %% : web_server_status
   switching_protocols = %% BslNet.Requestdef.sc_SwitchingProtocols %% : web_server_status
   success = %% BslNet.Requestdef.sc_OK %% : web_server_status
   created = %% BslNet.Requestdef.sc_Created %% : web_server_status
   accepted = %% BslNet.Requestdef.sc_Accepted %% : web_server_status
   non_authoritative_information = %% BslNet.Requestdef.sc_Non_AuthoritativeInformation %% : web_server_status
   no_content = %% BslNet.Requestdef.sc_NoContent %% : web_server_status
   reset_content = %% BslNet.Requestdef.sc_ResetContent %% : web_server_status
   partial_content = %% BslNet.Requestdef.sc_PartialContent %% : web_server_status
   multiple_choices = %% BslNet.Requestdef.sc_MultipleChoices %% : web_server_status
   address_moved = %% BslNet.Requestdef.sc_MovedPermanently %% : web_server_status
   found = %% BslNet.Requestdef.sc_Found %% : web_server_status
   see_other = %% BslNet.Requestdef.sc_SeeOther %% : web_server_status
   not_modified = %% BslNet.Requestdef.sc_NotModified %% : web_server_status
   use_proxy = %% BslNet.Requestdef.sc_UseProxy %% : web_server_status
   address_redirected = %% BslNet.Requestdef.sc_TemporaryRedirect %% : web_server_status

   bad_request = %% BslNet.Requestdef.sc_BadRequest %% : web_server_status
   unauthorized = %% BslNet.Requestdef.sc_Unauthorized %% : web_server_status
   payment_required = %% BslNet.Requestdef.sc_PaymentRequired %% : web_server_status
   forbidden = %% BslNet.Requestdef.sc_Forbidden %% : web_server_status
   wrong_address = %% BslNet.Requestdef.sc_NotFound %% : web_server_status
   method_not_allowed = %% BslNet.Requestdef.sc_MethodNotAllowed %% : web_server_status
   not_acceptable = %% BslNet.Requestdef.sc_NotAcceptable %% : web_server_status
   proxy_authentication_required = %% BslNet.Requestdef.sc_ProxyAuthenticationRequired %% : web_server_status
   request_timeout = %% BslNet.Requestdef.sc_RequestTime_out %% : web_server_status
   conflict = %% BslNet.Requestdef.sc_Conflict %% : web_server_status
   gone = %% BslNet.Requestdef.sc_Gone %% : web_server_status
   length_required = %% BslNet.Requestdef.sc_LengthRequired %% : web_server_status
   precondition_failed = %% BslNet.Requestdef.sc_PreconditionFailed %% : web_server_status
   request_entity_too_large = %% BslNet.Requestdef.sc_RequestEntityTooLarge %% : web_server_status
   request_uri_too_large = %% BslNet.Requestdef.sc_Request_URITooLarge %% : web_server_status
   unsupported_media_type = %% BslNet.Requestdef.sc_UnsupportedMediaType %% : web_server_status
   requested_range_not_satisfiable = %% BslNet.Requestdef.sc_RequestedRangeNotSatisfiable %% : web_server_status
   expectation_failed = %% BslNet.Requestdef.sc_ExpectationFailed %% : web_server_status

   internal_server_error = %% BslNet.Requestdef.sc_InternalServerError %% : web_server_status
   not_implemented = %% BslNet.Requestdef.sc_NotImplemented %% : web_server_status
   bad_gateway = %% BslNet.Requestdef.sc_BadGateway %% : web_server_status
   service_unavailable = %% BslNet.Requestdef.sc_ServiceUnavailable %% : web_server_status
   gateway_timeout = %% BslNet.Requestdef.sc_GatewayTime_out %% : web_server_status
   http_version_not_supported = %% BslNet.Requestdef.sc_HTTPVersionNotSupported %% : web_server_status
 }}
  | {continue}                        -> WebStatus.continue
  | {switching_protocols}             -> WebStatus.switching_protocols

  | {success}                         -> WebStatus.success
  | {created}                         -> WebStatus.created
  | {accepted}                        -> WebStatus.accepted
  | {non_authoritative_information}   -> WebStatus.non_authoritative_information
  | {no_content}                      -> WebStatus.no_content
  | {reset_content}                   -> WebStatus.reset_content
  | {partial_content}                 -> WebStatus.partial_content

  | {multiple_choices}                -> WebStatus.multiple_choices
  | {address_moved}                   -> WebStatus.address_moved
  | {found}                           -> WebStatus.found
  | {see_other}                       -> WebStatus.see_other
  | {not_modified}                    -> WebStatus.not_modified
  | {use_proxy}                       -> WebStatus.use_proxy
  | {address_redirected}              -> WebStatus.address_redirected

  | {bad_request}                     -> WebStatus.bad_request
  | {unauthorized}                    -> WebStatus.unauthorized
  | {payment_required}                -> WebStatus.payment_required
  | {forbidden}                       -> WebStatus.forbidden
  | {wrong_address}                   -> WebStatus.wrong_address
  | {method_not_allowed}              -> WebStatus.method_not_allowed
  | {not_acceptable}                  -> WebStatus.not_acceptable
  | {proxy_authentication_required}   -> WebStatus.proxy_authentication_required
  | {request_timeout}                 -> WebStatus.request_timeout
  | {conflict}                        -> WebStatus.conflict
  | {gone}                            -> WebStatus.gone
  | {length_required}                 -> WebStatus.length_required
  | {precondition_failed}             -> WebStatus.precondition_failed
  | {request_entity_too_large}        -> WebStatus.request_entity_too_large
  | {request_uri_too_large}           -> WebStatus.request_uri_too_large
  | {unsupported_media_type}          -> WebStatus.unsupported_media_type
  | {requested_range_not_satisfiable} -> WebStatus.requested_range_not_satisfiable
  | {expectation_failed}              -> WebStatus.expectation_failed

  | {internal_server_error}           -> WebStatus.internal_server_error
  | {not_implemented}                 -> WebStatus.not_implemented
  | {bad_gateway}                     -> WebStatus.bad_gateway
  | {service_unavailable}             -> WebStatus.service_unavailable
  | {gateway_timeout}                 -> WebStatus.gateway_timeout
  | {http_version_not_supported}      -> WebStatus.http_version_not_supported

web_err_description_of_web_err_num(e) =
   of_int = %% BslNet.Requestdef.reason_phrase %% : int -> string
   int_of_inner = %% BslNet.Requestdef.status_code %% : web_server_status -> int
   of_int(int_of_inner(e))

web_err_description_of_web_response(e) = web_err_description_of_web_err_num(web_err_num_of_web_response(e))


}}
