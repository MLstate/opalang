/*
    Copyright © 2011, 2012 MLstate

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

import stdlib.core.{parser, date, rpc.core, web.{core,resource,request}, xhtml, args, i18n}
import-plugin crypto


/**
 * Management of requests
 *
 * This module provides a low-level API for receiving requests and invoking the corresponding URL parsers.
 *
 * @author Rudy Sicard, 2008-2009
 * @author David Rajchenbach-Teller, 2010
 * @target INTERNAL_USE
 * @stability UNSTABLE
 */

/*
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
 * A type for messages used internally inside the server for updating state and/or applying actions.
 *
 * @param 'msg The type of messages used to communicate inside the server requesting update of state.
 * @param 'state The state of the server.
 */
type internal_server_communication('msg, 'state) =
    {set : 'msg         /**Send a message requesting update of state.*/
    request: HttpRequest.request
    }
  / {execute: void       /**Sum tag. Used to mark the fact that we're requesting execution of a command.*/;
     setter:  HttpRequest.request, 'msg -> void/**A pointer to the function used to request state changes. Later passed to [command].*/;
     command: 'state, ('msg -> void) -> void/**A command to execute.*/
     request: HttpRequest.request    /**The current connexion.*/
    }



type Server.private.generate_resource = {
       filename:string
       init_value:string
       kind:string // js, css
       mimetype : string
       replace : bool // indicate if the file should be replace on each run
       minifier : string -> string
  }

/**
 * {1 Interface}
 */

Server_private = {{

    @private _internal_ = "_internal_"
    @private base_url_string = Resource.base_url ? ""
    @private _internal_string = "{_internal_}"
    @private _internal_parser = parser "{_internal_}" -> void

    /**
     * The date at which the server was launched
     *
     * Used e.g. for caching.
     */
    launch_date: Date.date = Date.now()
    some_launch_date = {some = launch_date} //Provided for functions that would otherwise need to box [launch_date] all the time

    overridable_handlers = parser
       | "/about/opa"            -> page_version


    @private make_registerer() : ('a -> void, -> list('a)) =
      registered = Server_reference.create([])
      add(obj) : void =
        Server_reference.update(registered,(l -> [obj|l]))
      get() =
        r = Server_reference.get(registered)
        do Server_reference.set(registered,[])
        r
      (add,get)

    @private add_get_service : (service -> void, _) = make_registerer()
    add_service = add_get_service.f1

    @private add_get_url_handler : (url_handler(resource) -> void, _) = make_registerer()
    add_url_handler = add_get_url_handler.f1


    run_services() =
        debug_timeout_args:CommandLine.family(option(int)) = {
          title = "Unit testing"
          init = {none}
          anonymous = []
          parsers =
          [{ CommandLine.default_parser with
             names       = ["--test-stop-app-after", "-t", "--test_server_timeout"]
             description = "Completely stop application after given number of seconds. Used for automated test runs."
             param_doc   = "<n>"
             on_param(_) = parser seconds={Rule.natural} -> {no_params = {some = seconds}}
          }]
        }
        do match CommandLine.filter(debug_timeout_args) with
          | {none} -> void
          | {some = seconds} ->
             do Log.info("Test run", "This application will shutdown after {seconds} seconds.")
             Scheduler.sleep(seconds * 1000, (-> do Log.info("Test run", "Timeout reached, shutting down."); %% BslSys.exit %%(8)))

        make_server  = make_server_delayed()
        services     = add_get_service.f2()
        url_handlers = add_get_url_handler.f2()

        merge_handler(p1, p2) = parser
          | x=p1 -> x
          | x=p2 -> x
        (add, launch) =
        (
          name_to_implem = Server_reference.create(StringMap.empty)
          add(service:service) =
          (
            name = service.server_name
            map = Server_reference.get(name_to_implem)
            map = (match StringMap.get(name, map) with
              | {none} -> StringMap.add(name, service, map)
              |~{some} -> //Merge url handlers, ensure that everything else is compatible
                cmp = [("port",       service.port       == some.port),
                       ("netmask",    service.netmask    == some.netmask),
                       ("encryption", service.encryption == some.encryption)]
                match List.find(| (_, {false}) -> {true} | _ -> {false} , cmp) with
                  | ~{some} -> error("Error when launching service {service.server_name}: two definitions have distinct {some.f1}. Cannot launch the application.")
                  | {none} -> //Everything is compatible
                      merged_service = {service with url_handler = merge_handler(service.url_handler, some.url_handler)}
                      StringMap.add(name, merged_service, map)
             )
             Server_reference.set(name_to_implem, map)
          )
          launch() =
          (
             StringMap.iter((_, service ->
                service = {service with url_handler = List.fold(merge_handler, url_handlers, service.url_handler)}
                make_server(service)), Server_reference.get(name_to_implem))
          )
          (add, launch)
        )

        do List.iter(add, services)
        launch()



    /** High level URL dispatching : (completed by a low level dispatcher)
     *
     * 1 - Try on "_internal_" prefixed url :
     *     - Delegate cell call
     *     - Rpc call
     *     - Javascript (code/all.js)
     *     - CSS (css/dyn_css.css)
     *
     * 2 - Try with user dispatcher
     *
     * Note that this order should not be changed, in order to avoid
     * the user catching e.g. channel communications by accident
     */

    make_dispatcher_delayed() =
      /* we cannot retrieve_js_file several times and we cannot
       * execute it at toplevel either
       * so we must add a lambda here, and we must not
       * execute this code under the lambda that takes service
       * (because we can take several services) */
      get_executable_id = %% BslInit.get_executable_id %%
      executable_id = get_executable_id()

      /* Javascript variables */
      /* here, we are making sure that the html sent to the client
       * has a url for the js that always contains the server id
       * but we are also making sure that _internal_/code/all.js is still a valid
       * url (but the client never uses it, it is meant for us because having a stable
       * url is convenient for scripts) */
      js_code = Client_code.retrieve_js_file()
      js_unique_stamp = ( %% BslCrypto.md5 %% )(js_code) //As some bits of the JS are generated at launch-time and can be randomized (e.g. @public sessions), the server id isn't sufficient
      js_file_no_internal_without_version = "code/all.js"
      js_file_no_internal_with_version = "{js_unique_stamp}/{js_file_no_internal_without_version}"
      js_file_with_version = "{base_url_string}/{_internal_}/{js_file_no_internal_with_version}"
      //js_file_without_version = "/_internal_/{js_file_no_internal_without_version}"
      js_library = [js_file_with_version]
      js_inline_code = ""
      js_parser = parser
      | "{js_file_no_internal_with_version}" -> {}
      | "{js_file_no_internal_without_version}" -> {}

      /* CSS variables */
      /* same remark as for the js above */
      css_code = Client_code.retrieve_css_file()
      css_file_no_internal_without_version = "css/dyn_css.css"
      css_file_no_internal_with_version = "{executable_id}/{css_file_no_internal_without_version}"
      css_file_with_version = "{base_url_string}/{_internal_}/{css_file_no_internal_with_version}"
      //css_file_without_version = "/_internal_/{css_file_no_internal_without_version}"
      css_inline_code = ""
      css_library = if String.is_empty(css_code) then [] else [css_file_with_version]
      css_parser = parser
      | "{css_file_no_internal_with_version}" -> {}
      | "{css_file_no_internal_without_version}" -> {}

      make_dispatcher(service : service) =
        //Transform a resource into something which may be passed to weblib
        export_resource = Resource_private.export_resource(
            css_library, css_inline_code,
            js_library,  js_inline_code,
            Resource.base_url,
            WebCoreExport.default_make_response,
            WebCoreExport.make_response_with_headers)

        export(winfo, r) = export_resource(winfo,r)(HttpRequest.Generic.get_low_level_request(winfo.http_request)) // ??

        provide_js = Resource_private.make_resource_include(js_file_with_version, {system_js}, %% BslMinJs.minify %%, js_code, {true}, {false}, {permanent},
                   (x -> Resource.source(x, "text/javascript")))
        provide_css = Resource_private.make_resource_include(css_file_with_version, {system_css}, identity, css_code, {true}, {false}, {permanent},
                    (x -> Resource.source(x, "text/css")))

        internal_error(winfo)(e) =
          do Log.warning("Server_private","Exception while answering {Debug.dump(e)}")
          export(winfo, Resource.default_error_page({internal_server_error}))


        /* The dispatcher, the result of this function */
        dispatch(winfo: web_info)=
        (
          /* 1 - Extract requested url*/
          str_url = winfo.http_request |> HttpRequest.Generic.get_uri
          //do Log.info("Server dispatch", "Received URL {str_url}")
          str_url = Text.to_string(Parser.parse(url_decode,str_url))
          //do Log.info("Server dispatch", "Decoded URL to {str_url}")
          //str_url = Uri.to_string(Parser.parse(UriParser.uri, str_url)) //Clean-up URI (removing "//", "/../", etc.)
          //do Log.info("Server dispatch", "Cleaned URL to {str_url}")

          /* 2 - Internal handlers */
          rpc_handler = OpaRPC_Server.Dispatcher.parser_(winfo: web_info)
          cell_handler = Cell_Server.Dispatcher.parser_(winfo: web_info)
          dynamic_resource = DynamicResource.parser_()
          internal_handler:Parser.general_parser(void) = parser
            | cell_handler -> void //Note: response is provided directly by [cell_handler]
            | rpc_handler -> void  //Note: response is provided directly by [rpc_handler]
            | resource=dynamic_resource -> export(winfo, resource)
            | js_parser   -> export(winfo, provide_js)
            | css_parser  -> export(winfo, provide_css)
            | "src_code" -> export(winfo, AppSources.page())
            | "null" -> export(winfo, Resource.raw_text(""))
            | "about/opa" -> export(winfo, page_version)
            | any=(.*) ->
              do Log.warning("Server_private",
                 "This is an abnormal request to a non-existent internal resource."
               ^ " [{Text.to_string(any)}]"
               ^ "Dropping.")
              export(winfo, Resource.default_error_page({wrong_address}))

          /* 3 - User handlers*/
          external_handler:Parser.general_parser(void) = parser
            | make_resource=service.url_handler ->
              export(winfo, make_resource(HttpRequest._of_web_info(winfo)))
            //Default (can be overridden) favicon, shortcut icons, etc
            | x=overridable_handlers -> export(winfo, x)
            | any=(.*) ->
              do Log.warning("Server_private",
                 "This is a request for a non-existent resource."
                 ^ " [{Text.to_string(any)}]"
                 ^ "Answer wrong_adress")
              export(winfo, Resource.default_error_page({wrong_address})) : void

           // to update user lang when external handler is reached
           touch_lang(_bool,it) = do ServerI18n.touch_user_lang(winfo.http_request)
                                  some((it,void))

           full_handler:Parser.general_parser(void) = parser
            | "/" _internal_parser "/" internal_handler -> void
            | &touch_lang external_handler              -> void

          full_handler_with_base = parser
            | "{base_url_string}" full_handler -> void
            | any=(.*) ->
              do Log.warning("Server_private",
                 "This is a request for a non-existent resource."
                 ^ " [{Text.to_string(any)}]"
                 ^ "Answer wrong_adress")
              export(winfo, Resource.default_error_page({wrong_address})) : void

          Parser.parse(full_handler_with_base, str_url)
        )

        error_proof_dispatch(a) = @catch(internal_error(a), dispatch(a))
        error_proof_dispatch

      make_dispatcher

    bogus_cookie_expiry_callback((_ec:string), (_ic:string)) =
      //do println("bogus_cookie_expiry_callback: ec={_ec} ic={_ic}")
      void
    bogus_ontransfer((_str:string), (_hdrs:HttpRequest.msg_list), (_i:int)) =
      //do println("ontransfer: str={_str} hdrs={_hdrs} i={_i}")
      {true}

    /**
     * Initialize the opa server.
     */
    make_server_delayed() =
      make_dispatcher = make_dispatcher_delayed()
      make_server(service:service) =
        /* Select bypasses */
        init_server = %% BslNet.Http_server.init_server_cps %%
                    : _, _, _, _, _, _, continuation(WebInfo.private.native), _ -> void
        set_cookie_expiry_callback = %% BslNet.Http_server.set_cookie_expiry_callback %%
                    : (string, string -> void) -> void
        complete_dispatcher = %%BslDispatcher.complete_dispatcher_cps%%

        /* Make dispatcher */
        url_dispatcher = make_dispatcher(service)
        url_dispatcher(x) =
          #<Ifstatic:BENCH_SERVER>
            print_t(t) = Log.notice("Server", "responded in {t}s")
            CoreProfiler.instrument(1, print_t){->url_dispatcher(x)}
          #<Else>
            url_dispatcher(x)
          #<End>

        dispatcher =
          // CAUTION     : The url_dispatcher called is delayed (via push) by complete_dispatcher for one important reason :
          //               It introduces a new execution task in the scheduler.
          //               By default AppServer don't make this choice for you.
          //               Without it, you have no concurrent server response.
          // PERFORMANCE : The main task is not interrupted, so working set context
          //               switching may be minimized. The server is more efficient with the push
          complete_dispatcher((base_url_string),(x -> url_dispatcher(WebInfo.of_native_web_info(x)))):continuation(WebInfo.private.native)

        /* Initialize server */
        do set_cookie_expiry_callback(bogus_cookie_expiry_callback)
        match service.encryption with
        | {no_encryption} ->
          init_server(service.server_name, service.port,
                      {none}, {none}, {none}, (SSL.make_secure_type(none,none)),
                      dispatcher, bogus_ontransfer)
        | ~{ certificate private_key password } ->
          init_server(service.server_name, service.port,
                      some(certificate), some(private_key), some(password), (SSL.make_secure_type(none,none)),
                      dispatcher, bogus_ontransfer)
        | ~{ secure_type } ->
          init_server(service.server_name, service.port,
                      none, none, none, secure_type,
                      dispatcher, bogus_ontransfer)
      make_server

    /* Check the contatenation of potentially invalid string
       is a valid string */
    @private
    check(l) =
      str = String.concat("",l)
      Rule.succeed_opt(
       if Cactutf.check(str) then
          some(Text.cons(str))
       else do Log.warning("url decoding",
                        "Invalid escaped")
         none
      )

    /**
     * Decode url, as per "http://en.wikipedia.org/wiki/URL_Encoding
     * Byte should be decoded as if it was valid utf-8 (see current standard)
     */
    url_decode =
        hex2 = parser
          | h1=Rule.hexadecimal h2=Rule.hexadecimal ->
            h1 * 16 + h2
        special_char = parser
          | "+" -> " "
          | "%" ~hex2 -> String.of_byte_val(hex2)
        special_chars = parser
          | l=special_char+ r={check(l)} -> r
        (parser
          l=(((!special_char .)+) | c=special_chars -> c )* -> Text.ltconcat(l) )
        : Parser.general_parser(text)

  @private generate_dynamic_content(filename, default_dir, minifier, value, replace, mimetype) =
   (
    debug = {true} //FIXME always debugging ? opt.is_set(debug_opt)
    if debug then
      // Note: these bypasses are defined locally to avoid unsafe abuse of the library
      file_content   = %% BslFile.content %% : string -> string
      file_of_string = %% BslFile.of_string %% : string, string -> void
      file_exists    = %% BslFile.exists %% : string -> bool
      // TODO: put back when option will be here
      //dir   = match opt.get(dir_opt) with
      //  | { hd= data ... } -> data
      //  | _ -> default_dir
      dir = default_dir
      filename = dir ^ "/" ^ filename
      do if file_exists(filename) then
         // TODO:Should make a backup copy
         if replace then Log.warning("Server","The file '{filename}' already exists. I will replace it by the version embedded in this server.")
         else  Log.warning("Server","The file '{filename}' already exists. I will use it instead of the server version.")
      do file_of_string(filename, value)
      do Log.info("Server","You can edit file {filename} while the server is running.\n")
      (cont -> WebCoreExport.default_make_response({volatile}, cont, {success}, mimetype, file_content(filename)))//Always reload
    else
      generated = minifier(value)
      (cont -> WebCoreExport.default_make_response({permanent}, cont, {success}, mimetype, generated))//Reload only when necessary
   )

  @private generateIfNeeeded({~filename ~init_value ~kind ~mimetype ~replace ~minifier}:Server.private.generate_resource)=
    generate_dynamic_content(filename,  "opa-debug-{kind}", minifier, init_value, replace, mimetype)

  @private magicExportCSSResource(filename:string, init_value:string) =
    generateIfNeeeded({~filename ~init_value kind="css" mimetype="text/css" replace=false minifier=identity})

  @private magicGenerateAndExportJSIfNeeded(filename:string, init_value:string) =
    minifier = %% BslMinJs.minify %% : string -> string
    generateIfNeeeded({~filename ~init_value kind="js" mimetype="text/javascript" replace=true ~minifier})

    /**
     * A page displaying information on the compiler
     */
  @private page_version =
    int2time = %%BslTime.export_t%%
    data = [
      // Most of identifiers used there are inserted by the compiler, opa_Roots
      ("compilation options",     if @compiletime("release") then "release" else "debug"),
          // TODO: update compilation_date to time_t
      ("compilation date",        Date.to_string(Date.ll_import(int2time(@compiletime("compilation_date"))))),
      ("opa compilation date",    @compiletime("opa_date")),
      ("opa git version",         Int.to_string(@compiletime("opa_git_version"))),
      ("opa git version hash",    @compiletime("opa_git_version_hash")),
      ("opa status",              if @compiletime("opa_is_release") then "release" else "beta"),
    ]
    opa_filenames = @compiletime("opa_filenames")
    fline(i) = if i == 0 then <td rowspan={List.length(opa_filenames)}>filenames</td> else <></>
    Resource.page("About: OPA",
        <table border="1">
          { List.fold((line, accu -> accu <+> <tr><td>{line.f1}</td><td>{line.f2}</td></tr>), data, <></>) }
          { List.foldl((filename, (xhtml, i) -> (xhtml <+> <tr>{fline(i) <+> <td>{filename}</td>}</tr>, i + 1)), opa_filenames, (<></>, 0)).f1 }
        </table>)

}}

/**
 * {1 Functions exported to the global namespace}
 */

@opacapi Server_private_run_services = Server_private.run_services
@opacapi Server_private_add_service  = Server_private.add_service
