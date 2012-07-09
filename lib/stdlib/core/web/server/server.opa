/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.{security.ssl, parser, map, rpc.core, web.{core,context,resource}}

/**
 * Defining web services.
 *
 * This file provides the basic definitions of web services.
 *
 * @category web
 * @author Rudy Sicard, 2006-2012
 * @author David Rajchenbach-Teller, 2009-2010
 * @author Quentin Bourgerie, 2010-2011
 * @destination public
 * @stability stabilizing
 */

/**
 * {1 About this module}
 *
 * This module provides the necessary functions to define web servers / web services.
 *
 * {1 Where do I start?}
 *
 * It depends on the kind of application you intend to write. If your application fits
 * on one page, you should use the simplest server available, use [one_page_server]. This
 * server is well-suited to applications that behave like desktop applications rather
 * than web pages, e.g. Google Mail.
 *
 * If, on the other hand, you wish your application to manipulate several pages, you
 * should use [simple_server]. This server is well-suited to applications that are
 * spread among  several distinct pages, with links, images, etc. For instance, blogs.
 * Primitives for manipulating pages, images, etc. can be find in module [Resources].
 *
 * {1 What if I need more?}
 *
 * For advanced uses, a third and more powerful server is available, [Server.make].
 * In addition to the features of [simple_server], [Server.make] will let you access
 * information about the connexion. This function may be composed with the functions
 * of module [UserContext] and with function [Resource.in_context] to attach information
 * to each user, such as log-in credentials, user-specific data, etc.
 *
 * Finally, if your server needs secure communications, you should use function [Server.secure].
 * As [Server.make], this function will let you access and store information about users. In
 * addition, this function enforces https-encrypted communications and guarantees that all the
 * resources provided by the server are protected by some resource access policy. Of course, you
 * will need to define and provide a policy, for instance the one implemented in the [Login] module.
 *
 * Note that https-encrypted communications require an OpenSSL certificate, which you need to obtain
 * from certification authorities.
 */

/**
 * {1 Types defined in this module}
 */

/**
 * Configuration of the server.
 */
type Server.conf = {
  port       : int;
  netmask    : ip;
  encryption : Server.encryption;
  name       : string;
}

/**
 * Server resgitrable resources, that can be registered for all the web pages inside a server applications.
 * Warning: It will be imported for ALL applications inside the server.
 *
 * favicon: a list of favicons to register
 * css: a list of css files to register
 * js: a list of js files to register
 * or a list of string that will be guessed depending on the extension.
 */
type Server.registrable_resource =
  {favicon : list(Favicon.t)}
/ {css : list(string)}
/ {js : list(string)}
/ {doctype : Resource.doctype}
/ list(Server.registrable_resource)


type Server.handler =

  /** The most simple request handler. It replies to all incoming
      request. With a title page [title] and given body by the [page]
      function. Then add a set of [resources] typically with the
      directive [@static_include_directory] and define default [css]
      paths. */
  / {title: string; page: -> xhtml}

  /** The most configurable request handler. The [custom] parser takes
      as input the non-decoded uri from incoming requests and should
      compute corresponding resource. */
  / {custom : Parser.general_parser(resource)}

  /** Request handler that gives a resource for a given uri */
  / {dispatch : Uri.relative -> resource}

  /** Request handler on decoded incoming uri. This handler takes as
      input the decoded uri from incoming requests which through the
      [filter]. */
  / {filter : Server.filter; dispatch : Uri.relative -> resource}

  /** Request handler which performs on non-decoded uri. Returns
      resource which uri matches into the [bundle] map. */
  / {resources : stringmap(resource)}

  /** An empty request handler but useful for external resources
      registering. */
  / {register : Server.registrable_resource}

  /** Request handler which aggregates several request handlers. On
      incomming request all handlers (in the order of list) are tested
      until one succeed and returns a resource. */
  / list(Server.handler)


/**
 * The encryption used for the connexion
 * - no_encryption for http
 * - the ssl parameters for https
 *   A default value for these parameters is given by
 *   Server.ssl_default_param
 *
 */
type Server.encryption =
  { no_encryption }
/ { certificate: string/**Name of a file containing a SSL-issued certificate*/
    private_key: string/**Name of a file containing a SSL-issued private key*/
    password:    string/**A full-text password*/
 }
/ { secure_type: SSL.secure_type}


/**
 * Specification of a service.
 *
 * Generally, you will not create a value of this type yourself, but rather by using a function
 * such as [one_page_server], [simple_server], [Server.make] or [Server.secure].
 *
 * Services are entry points to OPA. To execute a service [foo], your program needs to contain [server = foo].
 * Note that a program can contain several definitions [server = foo].
 */
type service =
    { options : Server.private.options
    ; netmask : ip              /**This field lets you restrict which clients can use this service, for instance
                                   to enforce the fact that the service should only be used on an Intranet or only
                                   by other services running on the same computer.

                                   The default value of  0.0.0.0 is the most permissive: everybody who can connect
                                   to this computer can access the service. Conversely, netmask 255.255.255.255
                                   will only allow the server itself to use this service.

                                   For more information on netmasks, see e.g.
                                   http://en.wikipedia.org/wiki/Subnetwork
                                 */
    ; url_handler    : url_handler(resource)
    ; encryption : Server.encryption /* If you need http encryption, you should consider using function [Server.secure]. */
    ; server_name : string       /**The name of the server. Two server with the same name share their command-line options.*/
//    ; options: ServerArgs.flag(void)
    }


/**
 * The type of a resource whose generation must be protected by some application-specific security policy.
 *
 * For more information, see functions [Server.protect], [Server.public] and [Server.secure].
 */
@abstract type Server.secure_resource = HttpRequest.request -> resource

/**
 * A filter determining the realm of a server, i.e. a subdomain or a subpath.
 *
 * Use functions of module [Server.Filter] to construct values of this type.
 */
@abstract type Server.filter = Uri.relative -> option(Uri.relative)

/**
 * {1 Interface}
 */

/**
 * Server module
 */
Server = {{

  /**
   * Convert a Server.handler to an url parser
   */
  @private handler_to_parser(handler:Server.handler) =
    rec simple_to_parser(handler) =
      match handler with
      | ~{custom} -> custom
      | ~{title page} ->
        parser
        | .* -> Resource.page(title, page())
        end
      | ~{dispatch} -> simple_to_parser({~dispatch filter=Filter.anywhere})
      | ~{filter dispatch} -> parser
        | u0=UriParser.uri u1={
            match u0 with
            | ~{path=_ fragment=_ query=_ is_directory=_ is_from_root=_} as u0 ->
              Rule.succeed_opt(@unsafe_cast(filter)(u0))
            | _ -> Rule.fail
            end
          } -> dispatch(u1)
        end
      | ~{resources} -> parser
        | "/" r={Rule.of_map(resources)} -> r
        end

    rec check(l) =
    match l : list(Server.handler)
      [{dispatch=_}| [_|_] as tl]
      [{page=_ ...}| [_|_] as tl] ->
        error("Checking Server.handler: page or dispatch case are not in last position")
      [_|tl] -> check(tl)
      _ -> {success}

    rec flatten(handler:Server.handler):list(Server.handler) =
      match handler
      | ~{hd tl} -> flatten(hd) ++ List.flatten(List.map(flatten,tl))
      | {nil} -> []
      | _ -> [handler]

    rec register(r) =
      match r
      | ~{favicon} -> List.iter(f -> Resource.register_external_favicon(f), favicon)
      | ~{js} -> List.iter(f -> Resource.register_external_js(f), js)
      | ~{css} -> List.iter(f -> Resource.register_external_css(f), css)
      | ~{doctype} -> Resource.register_default_doctype(doctype)
      | ~{hd tl} -> List.iter(register, ~{hd tl})
      | {nil} -> void

    rec remove_register(l) =
      (List.filter(_,l)){
        | {register=r} -> do register(r); false
        | _ -> true
      }

    flat_handler = remove_register(flatten(handler))
    do check(flat_handler)
    match flat_handler with
    | [e] -> match e
      | {nil}{register=_} -> Rule.fail
      | {custom=_} as e
      | {title=_; page=_} as e
      | {dispatch=_} as e
      | {filter=_ dispatch=_} as e
      | {resources=_} as e -> simple_to_parser(e)
      end
    | l -> Rule.of_parsers(List.map(handler_to_parser, l))

  /**
   * {2 Starting a server}
   */

  /**
   * Start a server.
   *
   * @param s A server
   */
  start(~{port netmask encryption name}:Server.conf, handler:Server.handler): void =
    default_= if encryption=={ no_encryption } then Server_options.default_http_options else Server_options.default_https_options
    default_ = {default_ with ~port ~name} // TODO remove name duplication
    options = Server_options.spec_args(name, default_)
    url_handler = Rule.map(handler_to_parser(handler), (r -> (_ -> r)))
    service = ~{server_name=name options netmask encryption url_handler}
    Server_private.services.add(service)

  /**
   * {2 Server configuration}
   */

  /**
   * Default [http] configuration with port equals to 8080 and the server name is "http".
   */
  http : Server.conf = { port = Server_options.default_http_options.port; netmask = 0.0.0.0; encryption = {no_encryption}; name = "http"}

  /**

   * Default [https] configuration with port equals to 4343, the server
   * name is "https". SSL certificate should be at ./service.crt and
   * SSL key should be at ./service.key.
   */
  https : Server.conf = { port = Server_options.default_https_options.port; netmask = 0.0.0.0; encryption = {certificate = "service.crt" private_key="service.key" password=""}; name = "https"}

  /**
   * {2 Constructing a server}
   */

  /**
   * An application displayed purely as one web page.
   *
   * This function is used primarily for quick-testing. Most applications require several pages,
   * images, etc. For this purpose, use rather [simple_server] or the more powerful [make].
   *
   * @param title The title of the page.
   * @param page A function producing the contents of the page.
   */
  one_page_server(title: string, page: -> xhtml): service = simple_server(handler_to_parser(~{title page}))


   /**
    * Create a simple complete service producing web pages and other resources.
    *
    * Example:
    * The following extract defines a simple server, running on default port 8080,
    * which displays a message depending on the address it receives.
    *
    * [server = Server.simple_dispatch(| {~path ...} -> Resource.html("Test", <>Welcome to {List.to_string(path)}</>))]
    *
    * Limitations:
    * This constructor produces a service that occupies the complete server, including all
    * subdomains, subpaths, etc. For applications divided in several modules identified by
    * distinct URIs and/or domains, you should rather use the more powerful function [make_dispatch].
    *
    * See also:
    * If you are only interested in producing one page, you can also use the simplified function
    * [one_page]. Other functions provide servers with different features. For instance, if your
    * application requires encrypted communications and authentication, you should take a look
    * at function [secure_dispatch].
    */
   simple_dispatch(dispatcher:Uri.relative -> resource): service =
   (
      make_dispatch(Filter.anywhere, dispatcher)
   )

   /**
    * Create a simple complete service producing web pages and other resources, for a subset of
    * the server (e.g. a subpath or a subdomain).
    *
    * Example:
    * The following extract defines a simple server, running on default port 8080,
    * which displays a message depending on the address it receives.
    *
    * {[
    * server = Server.make_dispatch(Server.Filter.path(["root", "application"]),
    *          | {~path ...} -> Resource.html("Test", <>Welcome to {List.to_string(path)}</>)
    * }
    *
    * This server will only respond to paths starting with "/root/application".
    *
    * See also:
    * If you are only interested in producing one page, you can also use the simplified function
    * [one_page]. Other functions provide servers with different features. For instance, if your
    * application requires encrypted communications and authentication, you should take a look
    * at function [secure_dispatch].
    */
   make_dispatch(filter: Server.filter, dispatcher: Uri.relative -> resource): service =
   (
      filter_private = filter
      f(u) = match filter_private(u) with ~{some} -> {some = dispatcher(some)} | _ ->  {none}
      full_dispatch(f, identity)
   )

   full_dispatch(dispatcher: Uri.relative -> 'a, decoder: 'a -> option(resource)):service =
   (  //TODO: For the moment, the URI is a relative URI. That's a limitation we need to overcome
      handle_uri(u) = parser result={ Rule.succeed_opt(decoder(dispatcher(u))) } -> result
      extract_relative_uri(u) = //TODO: This whole function is a counter-hack around the uri hack introduced to handle initial "/".
      (                         //The original hack is probably useless now.
         match u with
          ~{path=_ fragment=_ query=_ is_directory=_ is_from_root=_} as u -> handle_uri(u)
         | _ -> error("Internal error: what's this absolute URI doing here?")
      )
      p = parser u=UriParser.uri r={ extract_relative_uri(u) } -> r
      simple_server(p)
   )

   of_bundle(resources: list(stringmap(resource))): service =
   (
      map = Rule.of_parsers(List.map(Rule.of_map, resources))
      blind_url_parser = parser | "/" result=map -> _ -> result
      make(blind_url_parser)
   )

   /**
    * Register a list of file name, based on the extension (".css" and ".js" are supported).
    */
   register_from_extension(l : list(string)): void =
    List.iter(file ->
      if String.has_suffix(".css", file) then
        Resource.register_external_css(file)
      else if String.has_suffix(".js", file) then
        Resource.register_external_js(file)
      else
        Log.error("Server", "Unknown type of file, the resource \"{file}\" will not registered")
    , l)

   Filter =
   {{
     anywhere: Server.filter = x -> {some = x}
     nowhere: Server.filter  = _ -> {none}

     /**
      * Accept only URIs from a given subpath.
      */
     path(path: list(string)): Server.filter =
     (
        filter(u) =
          do Log.info("Path filter", "{path}")
          match List.for_all2(`==`, path, u.path) with
            | {result={true}} | {different_length={longest_second}} -> {some = u}
            | _ -> {none}
        filter
     )

     /**
      * Accept only URIs from a given subpath and remove the URI before passing it to the dispatcher.
      */
     remove_path(path: list(string)): Server.filter =
     (
        Rec =
        {{
           loop(original: Uri.relative, filter_path:list(string), uri_path:list(string)) = match filter_path with
             | [] -> {some = {original with path = uri_path}}
             | [x|xs] -> match uri_path with
                | []     -> {none} //Uri too short
                | [y|ys] -> if x == y then loop(original, xs, ys) else {none}
        }}
        filter(u: Uri.relative) = Rec.loop(u, path, u.path)
        filter
     )

  }}


/**
 * An application displayed as exactly one web page and additional resources (images, css, ...)
 *
 * This function is used primarily for quick-testing. Most applications require more control on pages,
 * images, etc. For this purpose, use rather [simple_server] or the more powerful [make].
 *
 * @param title The title of the page.
 * @param resources The additional resources (images, css, ...) - include them with @static_resource_directory
 * @param css Style information for the page, as a list of addresses for CSS files.
 * @param content A function producing the contents of the page
 */
one_page_bundle(title:string, resources: list(stringmap(resource)), css:list(string), content: -> xhtml): service =
(
  map = Rule.of_parsers(List.map(Rule.of_map, resources))
  blind_url_parser = parser | "/" result=map -> _ -> result
                            | x={Server_private.overridable_handlers} -> _ -> x/*avoid capturing favicon, etc.*/
                            | .* -> _ -> Resource.styled_page(title, css, content())
  make(blind_url_parser)
)

/**
 * Create a simple server producing web pages and other resources.
 *
 * Example:
 * The following extract defines a simple server, running on default port 8080,
 * which displays a message depending on the address it receives.
 *
 * [server = simple_server(parser .* -> html("Page {__1}", <>Welcome to page {__1}</>))]
 *
 * You can further customize the server by setting its fields, in particular [port]
 * and [netmask].
 *
 * Example:
 * The following extract defines a server behaving as [simple_server(my_parser)] but
 * accepting requests on port 80
 *
 *  [server = {simple_server(my_parser) with port = Server.default_port(80)}]
 *
 * See also:
 * If you are only interested in producing one page, you can also use the simplified function
 * [one_page]. Other functions provide servers with different features. For instance, if your
 * application requires encrypted communications and authentication, you should take a look
 * at function [secure].
 */
simple_server(urls: simple_url_handler(resource)) : service =
  blind_url_parser = parser result=urls -> _ -> result
  make(blind_url_parser)

simple_bundle(resources: list(stringmap(resource)), urls:simple_url_handler(resource)): service =
(
  map = Rule.of_parsers(List.map(Rule.of_map, resources))
  blind_url_parser = parser result=urls -> _ -> result
                      | "/" result=map -> _ -> result
  make(blind_url_parser)
)

  @private
  default_server_name = "opa-server"

  /**
   * Create a complete server for following users, producing web pages and other contents.
   *
   * By opposition to [simple_server], services created with [make] can take advantage
   * of connexion information, e.g. to authenticate users, generate different contents based
   * on user agent, etc.
   *
   * Example:
   * The following extract defines a complete server, running on default port 8080,
   * which displays a message depending on the address it receives and the user
   * connecting.
   *
   * [server = Server.make(parser .* -> id ->
                        html("Page {__1}", <>Welcome to page {__1}, user {Server.string_user_of_connexion(id)}</>))]
   *
   * You can further customize the server by setting its fields, in particular [port]
   * and [netmask].
   *
   * Example:
   * The following extract defines a server behaving as [Server.make(my_parser)] but
   * accepting requests on port 80
   *
   *  [server = {Server.make(my_parser) with port = Server.default_port(80)}]
   *
   * See also:
   * If you are only interested in producing one page, you can also use the simplified function
   * [one_page]. Other functions provide servers with different features. For instance, if your
   * application requires encrypted communications and authentication, you should take a look
   * at function [secure].
   *
   * See also:
   * To attach information to users, see module [UserContext] and function [Resource.in_context].
   */
  make(url_handler : Parser.general_parser(HttpRequest.request -> resource)): service =
    { options= Server_options.spec_args(default_server_name, Server_options.default_http_options)
      netmask=0.0.0.0 ~url_handler encryption={no_encryption} server_name=default_server_name }

/**
 * {2 Security checks}
 */

  /**
   * Create a complete secured server for following users, producing web pages and other contents.
   *
   * By opposition to [Server.make], services created with [Server.secure] run in
   * encrypted mode (this can be deactivated at server launch-time) and require all resources to
   * be protected by some application-specific security policy. The simplest security policy is
   * function [Server.public], which marks resources as accessible by everyone. Most applications
   * require more fine-grained security policies, e.g. access control lists, group authorizations,
   * etc. To implement these policies, the library provides function [Server.protect].
   */
   secure(ssl_params : Server.encryption, urls : Parser.general_parser(Server.secure_resource)): service =
       {make(urls) with options = Server_options.spec_args(default_server_name, {Server_options.default_https_options with port = 443})
                        encryption = ssl_params}

   /**
    * Mark a resource factory as public.
    *
    * Use this function when defining a server using [secure]. Resources defined with this function
    * are marked as requiring no authentication/security checked.
    *
    * @param maker A function used to create resources for users.
    */
   public(maker : HttpRequest.request -> resource) =
        maker : Server.secure_resource

   /**
    * Protect a resource factory.
    *
    * Use this function when defining a server using [secure]. Resources defined with this function
    * are marked as protected with a given application-specific policy.
    *
    * @param context A user context in which security information is stored. Typically, this context
    * will determine for which user a set of authorizations.
    *
    * @param policy  A function using the contents of [context] to determine whether a user can
    * access the resource. This function receives as arguments the identifier of the connexion
    * and the security information stored for this connexion. It must return [{none}] if there is no
    * issue with the user accessing
    * the resource, or [{some = redirection_page}] to block access to the resource and rather
    * return [redirection_page]. Typically, [redirection_page] will be either an error message
    * or a page prompting for a log-in.
    *
    * @param sensitive The factory used to produce the sensitive resource. This function is called
    * only if the security check of [policy] has succeeded.
    */
   protect(context:      UserContext.t('a),
            policy:      HttpRequest.request, 'a -> option(resource),
            sensitive :  HttpRequest.request, 'a -> resource) =
     aux = (request, info ->
             match policy(request, info) with
             | {none}  -> sensitive(request, info)
             | {~some} -> some
           ) : HttpRequest.request, 'a -> resource
     (request ->
       Resource.later(f ->
         do jlog("UC")
         UserContext.execute((info -> f(aux(request,info))), context)
       )
     ) : Server.secure_resource

   /* params taken from weblib */
   ssl_default_params = {
     certificate = "cert.pem"
     private_key = "privkey.pem"
     password    = "change this password"
   } : Server.encryption

  /**
    * Return the port of the corresponding server
    * result with none is unspecified but useful with only one server
    */
  get_port(name) =
    Option.map(c -> c.options.port, Server_private.get_service(name))

  /**
    * Return the address of the corresponding server
    * result with none is unspecified but useful with only one server
    */
  get_addr(name) =
    Option.map(c -> c.options.addr, Server_private.get_service(name))


  /**
   * Construct a service from a map of resources.
   *
   * This is a simple and convenient manner of adding a set of resources created with [@static_include_directory] to
   * an application.
   */
  of_map(map: stringmap((->Resource.resource))) : service =
  (
     a : Parser.general_parser((->Resource.resource)) = Rule.of_map(map)
     b : Parser.general_parser(HttpRequest.request -> resource) = parser resource=a -> (_ -> resource())
     make(b)
  )


  /**
   * {2 Specialized parsers}
   */
/*  Parser =
  {{*/
     @private permanent_prefix = Rule.of_string(ExecInit.id())
     resource_map(map: stringmap(resource)): Parser.general_parser(resource) =
     (
       suffix = Rule.of_map(map)
       parser "/" result=suffix -> result
     )

     permanent_resource_map(map: stringmap(resource)): Parser.general_parser(resource) =
     (
       suffix = Rule.of_map(map)
       parser permanent_prefix result=suffix -> result
     )

     resource(name: string, resource:resource): Parser.general_parser(resource) =
     (
        parser "/" {Rule.of_string(name)} -> resource
     )

     permanent_resource(name: string, resource:resource): Parser.general_parser(resource) =
     (
        suffix = Rule.of_string(name)
        parser permanent_prefix suffix -> resource
     )
/*  }}*/
}}

/**
 * {1 Functions exported to global namespace}
 */

one_page_server = Server.one_page_server
simple_server   = Server.simple_server
