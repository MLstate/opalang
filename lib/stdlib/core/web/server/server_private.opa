/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.{parser, date, rpc.core, web.{core,resource,request}, xhtml, args, i18n}
import-plugin {unix, server}


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
@server_private
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


    @private make_registerer() =
      registered = Server_reference.create([])
      add(obj) : void =
        Server_reference.update(registered,(l -> [obj|l]))
      get() = Server_reference.get(registered)
      ~{add get}

    services = make_registerer() : ~{add:service->void get:->list(service)}

    get_service(name) = List.find(service -> service.server_name==name ,services.get())

    @private
    url_handlers = make_registerer() : ~{add:url_handler(resource)->void get:->list(url_handler(resource))}


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
        services     = services.get()
        url_handlers = url_handlers.get()

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
                cmp = [("options",    service.options    == some.options),
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
      executable_id = ExecInit.id()

      /* Javascript variables */
      /* here, we are making sure that the html sent to the client
       * has a url for the js that always contains the server id
       * but we are also making sure that _internal_/code/all.js is still a valid
       * url (but the client never uses it, it is meant for us because having a stable
       * url is convenient for scripts) */
      js_code = Client_code.retrieve_js_file()

      //As some bits of the JS are generated at launch-time and can be randomized (e.g. @public sessions), the server id isn't sufficient
      js_unique_stamp = %% BslMisc.md5 %%(js_code)
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

        provide_js =
          Resource_private.make_resource_include(js_file_with_version, {system_js},
            binary_of_string(JsMinifier.minify(js_code)), true, false, {permanent},
            Resource.binary(_, "text/javascript"))
        provide_css =
          Resource_private.make_resource_include(css_file_with_version, {system_css},
            binary_of_string(css_code), true, false, {permanent},
            Resource.binary(_, "text/css"))

        internal_error(winfo)(e) =
          do Log.warning("Server_private","Exception while answering {Debug.dump(e)}")
          export(winfo, Resource.default_error_page({internal_server_error}))

        /* The dispatcher, the result of this function */
        rec dispatch(winfo: web_info)=
        (
          request = winfo.http_request
          /* 1 - Extract requested url*/
          str_url = HttpRequest.Generic.get_uri(request)
          wrong_address(kind) =
            do Log.warning("Server", "This is an abnormal request to a non-existent {kind}
{str_url}")
            export(winfo, Resource.default_error_page({wrong_address}))
          /* 2 - Internal handlers */
          rpc_handler = OpaRPC_Server.Dispatcher.parser_(winfo: web_info)
          cell_handler = Cell_Server.Dispatcher.parser_(winfo: web_info)
          /* Context handler are dispatcher which needs a client context */
          context_handler = parser
            #<Ifstatic:OPA_FULL_DISPATCHER>
            | "chan/" winfo=WebSession.parser_(winfo) -> winfo : option(web_info)
            | winfo=PingRegister.parser_(winfo) -> winfo : option(web_info)
            | rpc_handler -> {none}
            | cell_handler -> {none}
            #<End>
            | .* -> do wrong_address("contexted resource") {none}
          internal_handler:Parser.general_parser(void) = parser
            #<Ifstatic:OPA_FULL_DISPATCHER>
            #<Else>
            | rpc_handler -> void
            | cell_handler -> void
            #<End>
            | resource=DynamicResource.parser_() -> export(winfo, resource)
            | js_parser   -> export(winfo, provide_js)
            | css_parser  -> export(winfo, provide_css)
            | "null" -> export(winfo, Resource.raw_text(""))
            | "about/opa" -> export(winfo, page_version)
            | .* -> wrong_address("internal resource")

          /* 3 - User handlers*/
          external_handler:Parser.general_parser(void) = parser
            | make_resource=service.url_handler ->
              export(winfo, make_resource(HttpRequest._of_web_info(winfo)))
            //Default (can be overridden) favicon, shortcut icons, etc
            | x=overridable_handlers -> export(winfo, x)
            | .* -> wrong_address("resource")

          /* 2.1 - Parser which set the thread context */
          full_handler =
            build_thread_context(page) = {
                key = {client = { client = %%BslNet.Requestdef.get_cookie%%(request.request) ~page} }
                request = {some = request}
                constraint = {free}
                details = none
              }
            create_thread_context_opt() =
              match ThreadContext.get_opt({current}) with
              | {none}
              | {some = {key = {nothing} ...}}
                -> build_thread_context(Random.int(Limits.max_int))
              | {some = x} -> x
            parser
             | "/{_internal_}/" page=Rule.integer "/" uri=(.*) ->
                Option.iter(dispatch,
                  @with_thread_context(
                    build_thread_context(page),
                    Parser.Text.parse(context_handler, uri)
                  )
                )
            | "/{_internal_}/" uri=(.*) ->
              @with_thread_context(
                create_thread_context_opt(),
                Parser.Text.parse(internal_handler, uri)
              )
            | uri=(.*) ->
              @with_thread_context(
                create_thread_context_opt(),
                do ServerI18n.touch_user_lang(winfo.http_request)
                Parser.Text.parse(external_handler, uri)
              )

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

    complete_dispatcher =
      #<Ifstatic:OPA_FULL_DISPATCHER>
      (_base_url, dispatcher ->
        Continuation.make(w -> dispatcher(w))
      )
      #<Else>
      %%BslDispatcher.complete_dispatcher_cps%%
      #<End>

    /**
     * Initialize the opa server.
     */
    make_server_delayed() =
      make_dispatcher = make_dispatcher_delayed()
      make_server(service:service) =
        /* Select bypasses */
        init_server = %% BslNet.Http_server.init_server_cps %%
                    : _, _, _, _, _, _, continuation(WebInfo.private.native), _ -> void

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
          // CAUTION     : The url_dispatcher called is delayed (via push) by complete_dispatcher (in bslDispatcher) for one important reason :
          //               It introduces a new execution task in the scheduler.
          //               By default AppServer don't make this choice for you.
          //               Without it, you have no concurrent server response.
          // PERFORMANCE : The main task is not interrupted, so working set context
          //               switching may be minimized. The server is more efficient with the push
          complete_dispatcher((base_url_string),(x -> url_dispatcher(WebInfo.of_native_web_info(x)))):continuation(WebInfo.private.native)

        /* Initialize server */
        match service.encryption with
        | {no_encryption} ->
          init_server(service.server_name, service.options,
                      {none}, {none}, {none}, (SSL.make_secure_type(none,none)),
                      dispatcher, bogus_ontransfer)
        | ~{ certificate private_key password } ->
          init_server(service.server_name, service.options,
                      some(certificate), some(private_key), some(password), (SSL.make_secure_type(none,none)),
                      dispatcher, bogus_ontransfer)
        | ~{ secure_type } ->
          init_server(service.server_name, service.options,
                      none, none, none, secure_type,
                      dispatcher, bogus_ontransfer)
      make_server

  @private iter_of_bin(b):iter(binary) = {next= -> {some=(b, {next=->{none}})}}

  @private generate_dynamic_content(filename, default_dir, minifier, value:string, replace, mimetype) =
   (
    debug = {true} //FIXME always debugging ? opt.is_set(debug_opt)
    if debug then
      // Note: these bypasses are defined locally to avoid unsafe abuse of the library
      file_content   = %% BslFile.content %% : string -> binary
      file_of_string(s, b) = Scheduler.push(->%% BslFile.write %%(s, b))
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
      do file_of_string(filename, binary_of_string(value))
      do Log.info("Server","You can edit file {filename} while the server is running.\n")
      (cont ->
         content = iter_of_bin(file_content(filename))
         WebCoreExport.default_make_response({volatile}, cont, {success}, mimetype, content))//Always reload
    else
      generated = iter_of_bin(binary_of_string(minifier(value)))
      (cont -> WebCoreExport.default_make_response({permanent}, cont, {success}, mimetype, generated))//Reload only when necessary
   )

  @private generateIfNeeeded({~filename ~init_value ~kind ~mimetype ~replace ~minifier}:Server.private.generate_resource)=
    generate_dynamic_content(filename,  "opa-debug-{kind}", minifier, init_value, replace, mimetype)

  @private magicExportCSSResource(filename:string, init_value:string) =
    generateIfNeeeded({~filename ~init_value kind="css" mimetype="text/css" replace=false minifier=identity})

  @private magicGenerateAndExportJSIfNeeded(filename:string, init_value:string) =
    minifier = JsMinifier.minify
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
@opacapi Server_private_add_service  = Server_private.services.add
