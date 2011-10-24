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

import stdlib.core.{parser, map, web.core, xhtml, rpc.maxint}

/**
 * Management of web content.
 *
 * @category WEB
 * @author David Rajchenbach-Teller, 2010
 * @destination PUBLIC
 * @stability STABILIZING
 */

/**
 * {1 About this module}
 *
 * This modules provides the necessary functions to define and export resources, such as images,
 * web pages, error pages, etc. as well as customizing pages for display on platforms with specific
 * needs (e.g. PDAs, iPhone, etc.)
 *
 * {1 Where do I start?}
 *
 * To create a web page, use function [Resource.page] or its shortcut [html]. To create an image,
 * use function [Resource.image]. OPA will handle the details.
 *
 * {1 What if I need more?}
 *
 * To add new kinds of resources, such as videos or mp3, use [Resource.binary] or its cousin
 * [Resource.source]. To customize pages, use [Resource.full_page].
 */

/**
 * {1 Types defined in this module}
 */

/**
 * A web resource.
 *
 * Web resources are web pages, images, videos, etc. More generally, anything that can be accessed by a URL.
 * If you wish to provide secured web resources, module [Secure] provides facilities for securing web resources
 */
type resource = Resource.resource
@abstract type Resource.resource = resource_private

/**
 * Information given to the client on how to cache the resource
 */
type Resource.cache = {volatile}    /** The resource changes at each request, remember it on the client is pointless, possibly unsafe.
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


/**
 * Dynamic resource can be update after the compilation
 * TODO remove after debugging
 */
@abstract type dynamic_resource = dynamic_resource_private

/**
 * The type of an image.
 *
 * To convert an image into a resource, use function [Resource.image].
 */
type image = {jpg: binary} / {ico: binary} / {png: binary} / {gif: binary}

/**
 * A tool which may be used to customize an application for a specific platform.
 *
 * You may use function [customize_for_platform], in relation with this type,
 * to provide specific display for cellphones, netbooks, etc. -- or for buggy browsers.
 */
type platform_customization =
    user_compat -> option({custom_body:    option(xhtml);
                           custom_headers: option(xhtml);
                           custom_css:     list(string);
                           custom_js:      list(string)})

/*
{content: resource_content; expires_at: option(float)}

/**
 * The contents of a resource.
 *
 * You should never need to manipulate this.
 */
type resource_content = external
*/




Resource = {{

add_header(r : resource, h : Resource.http_header) =
  { r with rc_headers = [h | r.rc_headers] } : Resource.resource

add_headers(r : resource, hs : list(Resource.http_header)) =
  { r with rc_headers = List.append(hs, r.rc_headers) } : Resource.resource

base_url =
  commandline : CommandLine.family(option(string)) = {
    title = "Specify a base URL"
    init = none
    parsers = [{ CommandLine.default_parser with
      names = ["--base-url"]
      description = "Relative URLs will be relative to this parameter (which should be a relative URL path)"
      on_param(_) = parser base=UriParser.path -> {no_params = some("/{String.concat("/",base.path)}") }
    }]
  anonymous = [] ;
  }
  CommandLine.filter(commandline)

/**
 * {2 Constructors}
 */

/**
 * Generic constructor for html resource
 * to simplify page & full_page
 */
@private
html_constructor(title, doctype, headers, html, status, customizers, rc_lastm) =
 { rc_content =
               {~html ~customizers ~doctype
                 headers = match title
                           | "" -> headers
                           | _  -> headers <+> <title>{title}</title>}
   rc_status=status rc_headers=[{lastm = rc_lastm}]} : resource


/**
 * Build a web page.
 *
 * This function is sufficient for most uses. If you need to more control on the page, e.g. to insert platform-specific
 * changes or error codes, you should rather use function [full_page].
 *
 * @param title A human-readable title for the page. It will be displayed in the title page of browsers and in bookmarks.
 * @param body The contents of the page.
 */
page(title:string, body: xhtml): resource = styled_page(title, [], body)

/**
 * Build a web page, adding a set of stylesheets.
 *
 * This function is sufficient for most uses. If you need to more control on the page, e.g. to insert platform-specific
 * changes or error codes, you should rather use function [full_page].
 *
 * @param title A human-readable title for the page. It will be displayed in the title page of browsers and in bookmarks.
 * @param styles A list of addresses for CSS pages.
 * @param body The contents of the page.
 */
styled_page(title:string, styles:list(string), body: xhtml): resource = html_constructor(title, {none},
                   <>
                     {List.map(url -> <link rel="stylesheet" type="text/css" href="{url}" />, styles)}
                   </>
                   ,body,{success},Resource_private.default_customizers, {volatile})


/**An alias for [page]*/
html = page


/**
 * Build a web page.
 *
 * This function is a more powerful variant on [page].
 *
 * @param title The title of the page.
 * @param body The contents of the page.
 * @param headers Headers which should be added to the page. The CSS and JavaScript code required to execute properly
 * the client code will be added automatically by OPA.
 * @param status A http status for this page. For most pages, you will want to use [{success}].
 * @param customizers A list of platform-specific customizers, which you may use e.g. to provide styling or libraries specific
 * to cellphones or broken browsers. The empty list is a safe default until you start testing your application on exotic platforms.
 */
full_page(title: string, body:xhtml, headers:xhtml, status: web_response, customizers: list(platform_customization)):resource =
    html_constructor(title, {none}, headers, body, status, List.append(Resource_private.default_customizers, customizers), {volatile})
    // Do not cache pages, we always generate a fresh number

full_page_with_doctype(title: string, doctype, body:xhtml, headers:xhtml, status: web_response, customizers: list(platform_customization)):resource =
    html_constructor(title, {some=doctype}, headers, body, status, List.append(Resource_private.default_customizers, customizers), {volatile})

  /**
   * Create a resource which is recomputed at each time it is served.
   * @param compute The function was recompute the resource with the
   * with the current HttpRequest.request.
   */
  dynamic(compute) =
    rc_content = {dynamic = compute}
    rc_status  = {success}
    rc_headers = [{ lastm = {volatile}}]
    (~{rc_content rc_status rc_headers}):resource

/**
 * Dynamic Resources
 */

/**
 * Build a dynamic resource from the name of the file, its type and its status.
 * @param name
 * @param mimetype the mimetype of the resource
 *        ex : "image/png" "audio/mp3" "image/ico"
 * @param status A http status for this page. For most pages, you will want to use [{success}].
 *
 */
create_dynamic_resource_status(name:string, mimetype:string, status:web_response) =
    Resource_private.private_create_dynamic_resource_status(name,mimetype, status) : dynamic_resource

/**
 * Build a dynamic resource from the name of the file and its type. Same as
 * [create_dynamic_resource_status] but with status set to [{success}]
 * @param name
 * @param mimetype the mimetype of the resource
 *        ex : "image/png" "audio/mp3" "image/ico"
 *
 */
create_dynamic_resource(name:string, mimetype:string) =
    Resource_private.private_create_dynamic_resource(name,mimetype) : dynamic_resource

/**
 * Update a dynamic resource if it changed
 */
update_dynamic(resource:dynamic_resource) =
    Resource_private.private_update_dynamic(resource) : dynamic_resource

/**
 * Update a dynamic resource status if it changed
 */
update_dynamic_status(resource:dynamic_resource,status:web_response) =
    Resource_private.private_update_dynamic_status(resource,status) : dynamic_resource


/**
 * Convert a dynamic resource to a resource
 */
dynamic_to_resource(dresource:dynamic_resource) =
    Resource_private.private_dynamic_to_resource(dresource) : resource

/*
/**
 * Update a dynamic resource in the database if it changed
 * @param path the path of the database containing a dynamic resource
 * see dynamic_resources.opa
 */
update_dynresource_in_db(path:ref_path('engine,dynamic_resource))=
    (rc = Db.read(path) : dynamic_resource
    rc_new = update_dynamic(rc)
    if eq(rc_new,rc) then void
    else  Db.write(path, rc_new)
    ): void

*/


/**
 * Construct a SOAP resource.
 *
 * @param soap A response defined in XML. As per SOAP definition, this request {e should} look like
 * [@xmlns(
         <soap:Header> some_header </soap:Header>
         <soap:Body>   some_body </soap:Body>
 * )]
 * where [some_header] and [some_body] are protocol-dependent information expected by the client
 * for this resource.
 */
soap(soap: xml): resource =
(
  shared_xml_header = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  text = shared_xml_header^Xmlns.to_string(soap)
  source(text, "application/xml+soap")
)

/**
* Build some xmlns content
* @param xml The contents of the xml resource
*/
xml(xml: xml) =
 { rc_content = {xml=xml};
   rc_status  = {success}
   rc_headers = [{ lastm = {modified_on = Date.now() }}]
 } : resource

/*
/**
* Build some soap content from xmlns
*
* This function is a more powerful variant on [soap]
*
* @param soap The contents of the soap resource
* @param status A http status for this resource. For most resources, you will want to use [{success}].
*/
full_soap(soap : xml, status: web_response) =
 { rc_content = {~soap};
   rc_status = {success}
   rc_headers = [{ lastm = {modified_on = Date.now()}}]
 } : resource
*/

/**
 * Build an image from some content.
 */
image(img: image) =
  Resource_private.resource_of_image(img) : resource


/**
 * Build a dynamic image from some content.
 */

dyn_image(img: image) =
  Resource_private.dynamic_resource_of_image(img) : resource

/**
 * Build a css from some content.
 */
build_css(css : string) =
  {
    rc_content = {css = css};
    rc_status = {success}
    rc_headers = [{ lastm = {modified_on = Date.now()}}]
  } : resource

/**
 * Build some pure text content.
 */
raw_text(t: string) =
  { rc_content = {txt = t};
    rc_status = {success}
    rc_headers = [{ lastm = {modified_on = Date.now()}}]
  } : resource


/**
 * Build some pure text content.
 */
json(t: RPC.Json.json) =
  { rc_content = {json = t};
    rc_status = {success}
    rc_headers = [{ lastm = {volatile}}]
  } : resource


/**
 * {2 Unknown resources}
 *
 * The following functions can be used to build resources for any other kind of content, e.g. audio, video, etc.
 */

/**
 * Build a resource from some binary content.
 *
 * e.g. to embed statically a mp3, use [binary(@static_source_content("my_file.mp3"), "audio/mp3")].
 */
binary(content: binary, mimetype: string) =
  { rc_content = {binary = content; mimetype = mimetype};
    rc_status = {success}
    rc_headers = [{lastm = {modified_on = Date.now()}}]
  } : resource

/**
 * Build a resource from some raw content.
 * Note: If you put status to [{success}], this is equivalent to [source].
 *
 * e.g. to embed some content, use [raw_response(my_content, "text/plain", {unauthorized})].
 */
raw_response(content: string, mimetype: string, status: web_response) =
  { rc_content = {source = content; mimetype = mimetype};
    rc_status = status
    rc_headers = [{ lastm = {modified_on = Date.now()}}]
  } : resource

/**
 * Build a resource containing only a status (empty content)
 *
 * e.g. to send a 409, use [raw_status({conflict})].
 */
raw_status(status: web_response) =
  raw_response("","", status)

/**
 * Change the status of a resource
 */
status_control(resource: resource, status: web_response) =
  { resource with rc_status = status } : resource

/**
 * Build a resource from some ascii content.
 *
 * e.g. to embed statically a source code, use [source(@static_source_content("my_file.py"), "text/python")].
 */
source(content: string, mimetype: string) =
  raw_response(content, mimetype, {success})


static_styled_page(title:string, styles:list(string), body: xmlns): resource =
  xml_content : xmlns =
    @xml(
      <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
          <title>{title}</title>
          <meta http-equiv="content-type" content="text/html; charset=utf-8" />
          {List.map(url -> @xml(<link rel="stylesheet" type="text/css" href="{url}" />), styles)}
        </head>
        <body>
          {body}
        </body>
      </html>
    )
  string_content = Xmlns.serialize_to_string_with_nsmap([], "http://www.w3.org/1999/xhtml", xml_content)
  raw_response(string_content, "text/html", {success})

static_page(title:string, body: xmlns): resource =
  static_styled_page(title, [], body)

cache_control(resource: resource, control) =
   {  rc_content = resource.rc_content;
      rc_status = {success}
      rc_headers = Resource_private.update_lastm(resource.rc_headers, control)
   } : resource

/**
 * Override the mime type of a resource
 *
 * Note: This is an unsafe operation, as some browsers base their security policy on the mime type.
 * Use with caution.
 */
override_type_unsafe(content: resource, mimetype:string) : resource =
  ~{rc_content rc_status rc_headers} = content
  { rc_content = {override_mime_type = mimetype; resource = rc_content}
    ~rc_status ~rc_headers} : resource

/**
 * {2 Platform-specific extensions}
 */

/**
 * A standard customizer to add an icon and a start-up image for applications running on iPhone OS.
 *
 * @param icon An icon for this application. This should be a URL to a png resource.
 * @param startup_image A startup image for this application. This should be a URL to a png resource
 */
iphone_customizer(icon: string, startup_image: string, custom_css : list(string), custom_js : list(string)) : platform_customization =
 | { environment = { iPhone }; renderer=_ } ->
    customization =
     {custom_body    =    {none};
      custom_headers =
        { some =
          <link rel="apple-touch-startup-image" href="{startup_image}" />
          <link rel="apple-touch-icon" href="{icon}" />
          <meta name="viewport" content="width=device-width; initial-scale=1.0; maximum-scale=1.0;" />
          <meta name="apple-mobile-web-app-capable" content="yes" />
          <meta name="apple-mobile-web-app-status-bar-style" content="black" />
       }
      ~custom_css
      ~custom_js
     }
     {some = customization}
  | _ -> {none}

/**
 * {2 Specialized constructors for errors and redirections}
 */

/**
 * Build a web page for displaying an error.
 *
 * See also [default_error_page] if you don't care about the appearance of the error page.
 */
error_page(title: string, body: xhtml, status: web_response) =
   full_page(title, body, empty_xhtml, status, []): resource

/**
 * Build the default web page for displaying an error.
 *
 * Note that this page is rather ugly. You should rather consider building your own
 * page for nicer error reporting, for instance with [error_page].
 */
default_error_page(status: web_response) =
   error_page("Error", <>Error: {WebCoreExport.web_err_description_of_web_response(status)}</>, status) : resource

/**
 * Build a redirection page.
 *
 * See also [default_redirection_page] if you don't care about the appearance of the redirection page.
 */
redirection_page(title: string, body: xhtml, status: web_response, delay: int, redirect_to: string) =
   full_page(title, body, <meta http-equiv="refresh" content={"{delay}; url={redirect_to}"}></meta>, status, []) : resource


/**
 * {3 Adding context}
 */

/**
 * Build a delegated result : this value makes the server does not give back anything to the client
 * this function makes the server call the function with a response function as parameter.
 * see also the test wget.opa
 *
 */
later(maker : ((resource -> void) -> void)) =
   { rc_content = {later=maker} : resource_private_content;
     rc_status  = {success}
     rc_headers = [{ lastm = {volatile}}]
   } : resource

export_data({~rc_content rc_status=_ rc_headers=_}: resource)=
  rec aux(rc_content : resource_private_content) =
    match rc_content with
      | {~html ~doctype ~headers customizers=_} ->
        body= <body id="Body">{html}</body>
        head = <head>{headers}</head>
        doctype = match doctype with {some=d} -> Resource_private.html_doctype_to_string(d) {none} -> Resource_private.shared_xhtml1_1_header
        page= Xhtml.of_string_unsafe(doctype) <+>
          <html xmlns="http://www.w3.org/1999/xhtml">{head}{body}</html>
        data=Xhtml.serialize_as_standalone_html(page)
        some({~data mimetype="text/html"})
      | ~{xml} ->
        xml = Resource_private.shared_xml_header^Xmlns.to_string(xml)
        some({data=xml mimetype="application/xml"})
      | ~{png} -> some({data=png mimetype="image/png"})
      | ~{jpg} -> some({data=jpg mimetype="image/jpg"})
      | ~{gif} -> some({data=gif mimetype="image/gif"})
      | ~{ico} -> some({data=ico mimetype="image/x-icon"})
      | ~{txt} -> some({data=txt mimetype="text/plain"})
      | ~{binary mimetype} -> some({data=binary ~mimetype})
      | ~{source mimetype} -> some({data=source ~mimetype})
      | ~{css} -> some({data=css mimetype="text/css"})
      | {~js} -> some({data=js mimetype="application/x-javascript"})
      | {~json} -> some({data=Json.serialize(json) mimetype="text/plain"})
      | {dynamic=_} -> none
      | ~{override_mime_type resource} -> Option.map(r -> {r with mimetype=override_mime_type}, aux(resource))
      | _ -> none
  aux(rc_content)
/*

   /**
    * Build a resource using some user-specific information.
    *
    * You can use this function to customize a page with information on the user,
    * such as a the user's e-mail address, the number of times the user has connected
    * to this page, etc.
    *
    * @param maker A function used to build the resource. This function will
    * receive information on the connexion and the user-specific information
    * associated with the given context and user.
    * @param context A user context containing information useful for building
    * this resource.
    *
    * @return A function which may be used in [Server.make] or [Server.secure].
    */
   in_context(maker : HttpRequest.request, 'a -> resource, context : user_context('a)) : (HttpRequest.request -> resource) =
    (connexion ->
      Resource.later(
        f ->
          Session.send(context,
                       {exec=(st,info -> f(maker(connexion,info)));
                        user=Server.get_user_unsafe(connexion)}))
    )

   /**
    * Build a resource (presumably, SOAP) using the contents of a request received
    * by the server.
    *
    * Use this function when you define a SOAP service, to decode the request passed
    * by client as a [xml] data structure.
    *
    * @param maker A function used to build the answer to the client. This function will
    * receive as argument the [xml] data structure obtained from the client.
    *
    * @note Security and performance notice: numerous Denial-of-Service attacks target SOAP
    * servers by saturating servers with complex XML data structures that are completely unrelated
    * to the actual capabilities of the server. For better protection against Denial-of-Service
    * attack, you should consider one of the following designs:
    * - authenticating clients before calling [in_soap_request]
    * - using a variant on this mechanism, such as [in_wsdl_request] (if you need XML), or
    * [in_rest_request] (for custom protocols), both of which let you define more resilient
    * services.
    */
   in_soap_request(maker: HttpRequest.request, outcome(xml,string) -> resource): (HttpRequest.request -> resource) =
     result(user) = maker(user, SOAP.get_request(user))
     result

*/
  /**
   * Build the default web page for redirecting the user to another address.
   *
   * Note that this page is rather ugly. You should rather consider building your own
   * page for nicer redirection reporting, for instance with [redirection_page].
   */
   default_redirection_page(redirect_to: string) =
     redirection_page("Redirection", <>This resource has moved to:{redirect_to}. You will be redirected automatically in a few seconds</>,
                      {address_moved}, 5, redirect_to) : resource


  /**
   * {2 Adding batches of files}
   */

  @private get_executable_id = %% BslInit.get_executable_id %%: -> string
  @private executable_id     = "/{get_executable_id()}/"
  @private executable_id_noslash = "/{get_executable_id()}"
  get_uri_of_permanent(name: string):string =
    //TODO: This [if] is a temporary compatibility hack (for URI cleanup)
     if String.length(name) >= 1 && String.get(0, name) == "/" then
        "{executable_id_noslash}{name}"
     else
        "{executable_id}{name}"

  get_uri_of_null =
    "{base_url ? ""}/_internal_/null"

 /**
  * {2 De-constructors}
  */

 get_content(resource:resource) : resource_private_content =
   resource.rc_content

 /**
  * {2 Deprecated}
  *
  * Prefer the more powerful [Server.resources] and [Server.permanent_resources].
  */

  @deprecated({hint = "use Server.permanent_resources instead"})
  add_permanent_to_simple_server(filemap : stringmap(Resource.resource), user_parser) =
    Resource_private.add_auto(filemap, user_parser, ((x : resource) -> x), executable_id)

  @deprecated({hint = "use Server.permanent_resources instead"})
  add_permanent_to_server(filemap : stringmap(Resource.resource), user_parser: Parser.general_parser(HttpRequest.request -> resource)) : Parser.general_parser(HttpRequest.request -> resource) =
    return_resource(x:resource)(_id:HttpRequest.request) = x
    Resource_private.add_auto(filemap, user_parser, return_resource, executable_id)


  /**
   * Complete given parser by adding automatic handling of files contained on given map
   *
   * @param filemap A map retrieved by the directive [@static_include_directory]
   * @param user_parser    A parser
   * @return a new parser including automatic treatment of files contained
   *            on directory given on argument to [@static_include_directory]
   */
  add_auto_simple_server(filemap : stringmap(Resource.resource), user_parser) =
    Resource_private.add_auto(filemap, user_parser, ((x : resource) -> x), "/"): Parser.general_parser(Resource.resource)

  add_auto_server(filemap : stringmap(Resource.resource), user_parser: Parser.general_parser(HttpRequest.request -> resource)) : Parser.general_parser(HttpRequest.request -> resource) =
    return_resource(x:resource)(_id:HttpRequest.request) = x
    Resource_private.add_auto(filemap, user_parser, return_resource, "/")

  /**
   * Adds an external javascript file (identified by its url) to the default_customizers of all resources.
   * @param url An url (as a string) to a javascript file
   * @usage This should be used in modules of the standard library not loaded by default (so not stdlib.*)
   * (Its interest is very limited in user code, since it's easier to customize resources directly)
   */
  register_external_js(url : string) : void = Resource_private.register_external_js(url)
  register_external_css(url : string) : void = Resource_private.register_external_css(url)
  unregister_external_js(url : string) : void = Resource_private.unregister_external_js(url)
  unregister_external_css(url : string) : void = Resource_private.unregister_external_css(url)
}}


/**
 * {1 Functions exported to the global namespace}
 */

/**
 * Build a web page.
 */
html(title:string, body:xhtml) = Resource.page(title:string, body: xhtml)

/**
 * Combine two chunks of xhtml.
 *
 * [a <+> b] is the same thing as <>{a}{b}</>
 */
`<+>`(a:xhtml, b:xhtml)=<>{a}{b}</>
