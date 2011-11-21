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

import stdlib.core.{parser, loop, date, web.core, rpc.core, web.request, cache, xhtml, args, set, i18n}
import stdlib.core.compare

/**
 * Management of web content.
 *
 * @category WEB
 * @author David Rajchenbach-Teller, 2010
 * @author Nicolas Glondu, 2011
 * @destination PRIVATE
 * @stability STABILIZING
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

/**
 * The doctype of an html resource
 */
type html_resource_doctype =
  {html5} /
  {xhtml1_1}

/**
 * The actual contents of a resource.
 */
type resource_private_content =
//User-definable resources
    {html:xhtml; doctype:option(html_resource_doctype); headers: xhtml; customizers: list(platform_customization)}
/*  / {soap:xmlns}*/
  / {xml:xmlns}
  / {png:binary}
  / {jpg:binary}
  / {gif:binary}
  / {ico:binary}
  / {txt:string}
  / {binary:binary; mimetype:string}
  / {source:string; mimetype:string}
  / {css:string}
  / {js:string}
  / {json:RPC.Json.json}
  / {dynamic: HttpRequest.request -> resource}
  / {later : (resource -> void) -> void}
  / {override_mime_type: string; resource: resource_private_content}

/**
 * The implementation of type [resource]
 */
type resource_private = { rc_content : resource_private_content;
                          rc_status  : web_response;
                          rc_headers : list(Resource.http_header);
                        }

type resource_cache_customizers = {
     customizers: list(platform_customization)
     external_css_files : list(string)
     inline_css_code : string
     external_js_files : list(string)
     inline_js_code : string
     headers : xhtml
}

type resource_cache_entry = {
 uri : string
 customizers : resource_cache_customizers
 body : xhtml
 user_agent: string
}

type iso_utf = external




/**
 * A dynamic resource can only be a binary file or a source file
 */
type dynamic_private_content =
   {binary:binary; mimetype:string}
  / {source:string; mimetype:string}

/**
 * The implementation of type [dynamic_resource]
 */

type dynamic_resource_private = { rc_name    : string;
                                  rc_content : dynamic_private_content;
                                  rc_status  : web_response;
                                  rc_headers : list(Resource.http_header);
                                }

/**
 * {1 Interface}
 */

Resource_private =
{{
  @private
  xhtml_compare = (Compare.compare : xhtml, xhtml -> 'a)

  @private
  xhtml_equality(a,b) = xhtml_compare(a,b) == {eq}

  html_doctype_to_string(doctype:html_resource_doctype) =
    match doctype with
    {xhtml1_1} -> shared_xhtml1_1_header
    {html5} -> shared_html5_header

  get_lastm(resource) =
    check(x) =
      match x with
      | { lastm = _ } -> true
      | _ -> false
      end
    List.find(check, resource.rc_headers)

  update_lastm(headers, new_lastm) =
    replace(elem, acc) =
      match elem with
      | { lastm = _ } -> List.cons({ lastm = new_lastm }, acc)
      | otherwise -> List.cons(otherwise, acc)
      end
    List.foldr(replace, headers, [])


   /**
    * Construct the inclusion of an external resource that can possibly be modified dynamically for debugging purposes
    *
    * @param file The name of the file
    * @param nature The nature of the file: OPA-generated js, OPA-generated css, anything else. Used in combination with command-line arguments to determine whether the file should be editable during this run.
    * @param minifier A function used to compress content. Will only be applied if the file is not subject to debugging.
    * @param static_source_content The default contents of the file. Will be used if debugging is deactivated or if the file needs to be regenerated.
    * @param replace If [{true}], if the file is already present on the disk, it will be replaced at start.
    *
    * @return {content; editable; last_modification}
    */     //Note: [Resource.add_auto], etc. is in charge of deciding whether to cache forever -- or not
   make_include =
   (

     choose_resources = CommandLine.filter(debug_editable_choose_resources)
     show_resources   = CommandLine.filter(debug_editable_show_resources)

     implem(name:string, nature: {system_js}/{system_css}/{misc}, minifier: string -> string, static_source_content:string, replace:bool, force_immutable:bool):
                         {mutable : -> {content:string ; modified_on: Date.date}} / {immutable: string} =
     (
        //file_content   = %% BslFile.content_cps %%:      string -> option(string) //Works only on Macintosh
        file_content   = %% BslFile.content_opt %%:      string -> option(string)
        file_exists    = %% BslFile.exists %%:           string -> bool
        file_create    = %% BslFile.of_string %%:        string, string -> void
        dir_exists     = %% BslFile.is_directory %%:     string -> bool
        dir_create     = %% BslFile.create_full_path %%: string -> void
        //get_base_name  = %% BslFile.basename %%:         string -> string
        get_dir_name   = %% BslFile.dirname %%:          string -> string
        file_move      = %% BslFile.move %%:             string,string,bool -> void

        //If necessary, show resources as they appear
        do if not(force_immutable) && show_resources then
           do Log.info("Debugging resources", "Registering resource {name}.")
           do println("This application embeds file {name}.")
           void

        debug_applies = if force_immutable then {false}
          else match choose_resources with
           | {all}  -> {true}
           | ~{js css files} ->
               match nature with
                 | {system_js}  -> js
                 | {system_css} -> css
                 | {misc}       -> StringSet.mem(name, files)
               end
        end
        if debug_applies then
        (
              name = "opa-debug/{name}"
              /**
               * This file should be reopened dynamically
               *
               * 1. Perform a little sanity check
               * 2. If the path doesn't exist, create the path
               * 3. If the file exists
               *  3.1 If [replace]
               *   3.1.1 Backup the old file and warn the user
               *   3.1.2 Create new file from [static_source_content]
               *  3.2 otherwise
               *   3.2.1 Inform user that existing file will be reused
               *  3.3 otherwise
               *   3.3.1 Create new file from [static_source_content]
               *  3.4 Inform the user that a file has been created
               */
              //1. Perform a few sanity checks
              do if String.is_empty(name)
                 || String.get(0, name) == "/" || String.get(0, name) == "\\"
                 || Option.is_some(String.index("../", name)) || Option.is_some(String.index("/..", name)) then
                      Log.warning("Debug inclusion", "The name of file \"{name}\" is suspicious. In debug mode, malicious users might be able to take advantage of this file name to consult system or private files")
                 else void

              //2. If path doesn't exist, create path
              //base_name = get_base_name(name)
              dir_name  = get_dir_name(name)

              do if not(dir_exists(dir_name)) then dir_create(dir_name) else void
              must_create =
              (
               if file_exists(name) then
                  if replace then
                    backup = "{name}.backup-{Random.string(32)}"
                    do Log.warning("Debugging resources", "Moving existing copy of {name} to {backup}")
                    do file_move(name, backup, {true})
                    {true}
                  else
                    do Log.info("Debugging resources", "Reusing existing copy of {name}")
                    {false}
                else {true}
              )

              do if must_create then
                 do file_create(name, static_source_content)
                 do Log.info("Debugging resources", "Created file {name}.")
                 void
                 else void
              do println("You can modify file {name} during the execution of the application.")
              do Log.info("Debugging resources", "You can modify file {name} during the execution of the application.")
              //Initialize cache
              cache = Mutable.make({content     = if must_create then static_source_content else match file_content(name) with ~{some} -> some | {none} -> static_source_content
                                    modified_on = if must_create then Date.now() else file_last_modification(name)})
              {mutable = (->

                 /**
                  * 1. If file still exists
                  *   1.1 Read file
                  *   1.2 Return content
                  *
                  * 2. Otherwise (the file has been removed during execution)
                  *   2.1 If path doesn't exist create path
                  *   2.2 Create new file from [static_source_content] and warn user
                  *   2.3 Return [static_source_content]
                  */
                (
                  regenerate(message:string) =
                     do Log.warning("Debugging resources", "{message}, regenerating")
                     do if not(dir_exists(dir_name)) then dir_create(dir_name) else void
                     do file_create(name, static_source_content)
                     result = {content = static_source_content; modified_on = Date.now()}
                     do cache.set(result)
                     result
                  if file_exists(name) then
                    /*Check if latest version is already in the cache -- if so, no need to reload*/
                    cache_content = cache.get()
                    modified_on   = cache_content.modified_on
                    last_modification = file_last_modification(name)
                    if Date.compare(last_modification, modified_on) == {gt} then
                      /*Ok, we do need to read from disk*/
                      do Log.info("Debugging resources", "Resource {name} has been updated, sending new version")
                      match file_content(name) with
                         | {none} -> regenerate("Cannot read file {name}")
                         |~{some} ->
                           result = {content = some; modified_on = file_last_modification(name)}
                           do cache.set(result)
                           result
                    else
                      cache_content
                  else
                    regenerate("File {name} has been removed")
                ))}
        ) else
           result = minifier(static_source_content)
           {immutable = result}
     )
     implem
   )

   /**
    * @param name The name of the resource.
    * @param cache_control A cache-control instruction applied to immutable resources. Typically, [{permanent}] for resources generated by the compiler.
    */
   make_resource_include(name:string, kind, minifier:string -> string, static_source_content:string, replace:bool, force_immutable: bool, cache_control:web_cache_control, make_resource:string -> Resource.resource): Resource.resource =
   (
        match make_include(name, kind, minifier, static_source_content, replace, force_immutable) with
           | ~{immutable} ->
                r = make_resource(immutable)
                { r with rc_headers = update_lastm(r.rc_headers, cache_control)}:resource
           | ~{mutable} ->
                {rc_content = {dynamic =
                     _ -> {~content; ~modified_on} = mutable()
                      tmp = make_resource(content)
                     { tmp with rc_headers = update_lastm(tmp.rc_headers, ~{modified_on}) }:resource
                      }
                 rc_status = {success}
                 rc_headers = [{lastm = {volatile}}]
                 } : resource
   )

   content_of_include(name:string, kind, minifier:string -> string, static_source_content:string, replace:bool, force_immutable:bool, make_content: string -> 'a): -> 'a =
   (
        match make_include(name, kind, minifier, static_source_content, replace, force_immutable) with
           | ~{immutable} ->
                 result = make_content(immutable)
              -> result
           | ~{mutable}   ->
              -> make_content(mutable().content)
   )

   raw_resource_status_factory(mimetype:string)(content:string, status:web_response) : Resource.resource =
      (
        { rc_content = {binary = content; mimetype = mimetype};
          rc_status  = status
          rc_headers = [{ lastm = {volatile}}]
        } : resource
      )

   raw_resource_factory(mimetype)(content) =
     raw_resource_status_factory(mimetype)(content, {success})

//TODO:
// - implement --debug-editable-directory
// - use non-blocking read
// - use non-blocking write
   /**
    * The command-line arguments for debugging
    */
   @private debug_editable_choose_resources = {
      title = "Debugging Resources : dynamic edition"
      init = {js = {false} css = {false} files = StringSet.empty}
      anonymous = []
      parsers = [
        {
          names       = ["--debug-editable-all", "-d"]
          param_doc   = ""
          description = "Export all the files embedded in the server to the file system, so that they can be viewed and edited during execution of the application"
          on_encounter(_) = {no_params = {all}}
          on_param    = CommandLine.no_params
        },
        {
          names       = ["--debug-editable-js"]
          param_doc   = ""
          description = "Export the JS files embedded in the server to the file system, so that they can be viewed and edited during execution of the application"
          on_encounter(state) = {no_params = match state with
                                       {all} -> {all}
                                     | {js=_ ~css ~files} -> {js = {true} ~css ~files}
                                }
          on_param    = CommandLine.no_params
        },
        {
          names       = ["--debug-editable-css"]
          param_doc   = ""
          description = "Export the CSS files embedded in the server to the file system, so that they can be viewed and edited during execution of the application"
          on_encounter(state) = {no_params = match state with
                                       {all} -> {all}
                                     | {~js css=_ ~files} -> {css = {true} ~js ~files}
                                }
          on_param    = CommandLine.no_params
        },
        {
          names       = ["--debug-editable-file"]
          param_doc   = "file*"
          description = "Export some files embedded with static_source_content, so that they can be viewed and edited during execution of the application"
          on_encounter(state) = {params = state}
          on_param(state) = parser x=(.*) -> {no_params = match state with
                                       | {all}  -> {all}
                                       | ~{js css files} -> {~js ~css files=StringSet.add("{x}", files)}
                                 }
        }
  /*,
        {
          names       = ["--debug-editable-directory"]
          param_doc   = "directory*"
          description = "Export some directories embedded with static_source_content, so that they can be viewed and edited during execution of the application"
          on_encounter(state) = {params = state}
          on_param(state) = parser x = (.*) -> {no_params = match state with
                                       | {all}  -> {all}
                                       | ~{js css files dirs} -> {~js ~css dirs=StringSet.add(dirs, "{x}") ~files}
                                 }
        }
  */
   ]}

   @private debug_editable_show_resources =
   {
       title = "Debugging Resources : listing resources"
       init = {false}
       anonymous = []
       parsers = [
         {CommandLine.default_parser with
          names     = ["--debug-list-resources"]
          param_doc = ""
          description = "List the resources embedded in this application."
          on_encounter(_) = {no_params = {true}}
          }
       ]
   }


        @private launch_date = Date.now()
        @private file_last_modification(file) =
          bsl_last_modification = %% BslFile.last_modification %% : string -> time_t
          Date.ll_import(bsl_last_modification(file))

        @private file_content = %% BslFile.content %% : string -> string

        resource_of_image(r:image)   =
            r0 = match r with
            | {~jpg} -> { ~jpg }
            | {~ico} -> { ~ico }
            | {~png} -> { ~png }
            | {~gif} -> { ~gif }
            { rc_content = r0;
              rc_status  = {success}
              rc_headers = [{ lastm = {permanent}}]
            } : resource

      dynamic_resource_of_image(r:image) =
            res = resource_of_image(r)
            { res with rc_headers = update_lastm(res.rc_headers, {volatile}) } : resource

    /**
     * see Resource.create_dynamic_resource
     */
    private_create_dynamic_resource_status(name:string, mtype:string, status:web_response) =
        lastm = {modified_on = file_last_modification(name)};//TODO: Should be max of this and Server_private.launch_time
        is_binary(arg:string)=
         Parser.partial_parse(parser
            | "image/"  -> true
            | "audio/"  -> true
            | "video/"  -> true
            | (.*) -> false,
                        arg);

            { rc_name = name;
              rc_content =
                if is_binary(mtype) then
                    {binary=file_content(name); mimetype=mtype}
                    : dynamic_private_content
                else
                    {source=file_content(name); mimetype=mtype}
                    : dynamic_private_content
                ;
              rc_status = status
              rc_headers = [~{lastm}]
             } : dynamic_resource

    private_create_dynamic_resource(name, mtype) =
      private_create_dynamic_resource_status(name, mtype, {success})

    /**
     * see Resource.update_dynamic
     */

    // TODO lorsque l'evaluation paresseuse sera en place, penser à ne charger le
    // contenu du fichier seulement lors de son utlisation et non à ce moment là
    // TODO s'assurer que le lazy est bien le comportement attendu : dans ce cas
    // on pourrait imaginer un @static_source_content et pas un file_content
    // le caractere dynamique de la fonction implique que nous ne chargeons pas
    // une seule fois le fichier, mais qu'il modifie en permanance
    private_update_dynamic(resource:dynamic_resource) =
        (new = file_last_modification(resource.rc_name)
        same = match Option.get(get_lastm(resource)) with
          | ~{lastm} -> (
              match lastm with
              | {volatile} | {permanent} | {check_for_changes_after = _} -> false
              | ~{modified_on} -> match Date.compare(new, modified_on) with {gt} -> true | _ -> {false}
            )
          | _ -> false
        if same then
            {rc_name = resource.rc_name;
             rc_content =
               ( match resource.rc_content : dynamic_private_content with
                 | {binary=_; mimetype=m} ->
                      {binary=file_content(resource.rc_name);
                       mimetype=m} : dynamic_private_content
                 | {source=_; mimetype=m} ->
                      {source=file_content(resource.rc_name);
                       mimetype=m} : dynamic_private_content );
             rc_status = resource.rc_status
             rc_headers = update_lastm(resource.rc_headers, {modified_on = new}) }
        else
            resource
        ): dynamic_resource

    private_update_dynamic_status(resource:dynamic_resource, status:web_response) =
        {resource with rc_status = status}: dynamic_resource

    /**
     * see Resource.dynamic_to_resource
     */
    private_dynamic_to_resource(dresource:dynamic_resource)=
        { rc_content = dresource.rc_content <: resource_private_content;
          rc_status = dresource.rc_status
          rc_headers = dresource.rc_headers
        } : resource



  /**
   * @param cache_control An information on the permanence of the resources present in the map
   * @param prefix An optional prefix to add before the directory name
   */
   add_auto(filemap : stringmap(resource), user_parser : Parser.general_parser('a), make_resource : (resource -> 'a), prefix):Parser.general_parser('a) =
   parser
     | {Rule.of_string(prefix)} file={Rule.of_map(filemap)} ->
        make_resource(file)
     | r={user_parser} -> r




/**
 * {2 Delivery mechanism}
 */

shared_xhtml1_1_header =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

shared_html5_header =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE html>"

shared_xml_header =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"

/**
 * Produce a response to a given request.
 *
 * [make_response last_modification request status mime_type content]
 * TODO add expires
 */

@private
customize_header(add) =  some({
         custom_body   = none
         custom_headers= some(add)
         custom_css    = []
         custom_js     = []
       })

customizer_for_icon =
  | { environment = _; renderer={ Trident =_ } } -> customize_header(<link rel="shortcut icon" type="image/x-icon" href="{Resource.base_url ? ""}/favicon.ico" />)
  | _ -> customize_header(<link rel="icon" type="image/gif" href="{Resource.base_url ? ""}/favicon.gif" />)

customizer_for_google_frame = // IE google frame
  | { environment = _; renderer={ Trident =_ } } -> customize_header(<meta http-equiv="X-UA-Compatible" content="chrome=1"/>)
  |_ -> none


/**
 * Construct a customizer for incompatible browsers.
 *
 * You can use this customizer e.g. to display an "incompatible browsers" warning,
 * or to redirect to a distinct page.  A default platform-customizer constructed
 * using this function is added by [full_page]. If, for some reason, you shortcut [full_page],
 * don't forget to reapply this customizer.
 *
 * For the moment, this customizer is invoked on
 * - Internet Explorer <= 7
 * - Unknown user agents
 *
 * Major search bots are recognized and should not trigger this warning.
 *
 * @param message
 * @param headers
 * @param css
 * @param js
 */
make_customizer_for_incompatible_browsers(incompatible_browser_customizer) /*: platform_customization*/ =
  customizer(user_compat:user_compat) =
  match user_compat.renderer with
    {Trident=[ major, _ ]}  -> if major <= 7 then incompatible_browser_customizer else none
    {Unidentified} -> incompatible_browser_customizer
    _ ->  none

   customizer

/**
 * Default message for unsupported browser.
 *
 */
incompatible_browser_message =
  Xhtml.compile(<div style="position:absolute;text-align:center;left:0px;top:0px;width:100%;
  background:#EEEEEE;text-color:black;border:ridge 2pt">
  <div style="margin:2pt">
    The browser you use is not supported by this application, probably
  because it lacks some critical features.
    </div>
    <div style="margin:2pt">For a better experience, please consider using this application with a <a style="text-decoration:none;color:green"
    href="http://www.mlstate.com/opa/documentation_-_supported_browsers">supported
  browser</a>.
  </div>
  </div>) : xhtml // value restriction ... restrictions

/**
 * Default customizer result for unsupported browser.
 *
 */
incompatible_browser_customizer =
                     {some= // some(
                     {custom_body    = {some=/*some(*/incompatible_browser_message/*)*/ }// value restriction ... restrictions
                     custom_headers = none
                     custom_css     = []
                     custom_js      = []}
                     } //) // value restriction ... restrictions

/**
 * Default customizer for unsupported browser.
 *
 */
required_customizer_for_incompatible_browsers: platform_customization =
  make_customizer_for_incompatible_browsers(incompatible_browser_customizer)

required_customizer_for_opa_ad =
| _ -> some({custom_body = {some = <div
  style="position:absolute;text-align:center;right:0px;top:0px;background:yellow;text-color:black;border:ridge
  2pt">
  <div style="margin:2pt">
    Application developed with <a href="http://www.mlstate.com">OPA</a>.
    </div>
    <div style="margin:2pt">Free preview release, for non-commercial use only.</div>
  </div>}
  custom_headers = {none}
  custom_js      = []
  custom_css     = []})

@private autoloaded_js = Mutable.make([] : list(string))
@private autoloaded_css = Mutable.make([] : list(string))
@package register_external_js(url : string) = autoloaded_js.set([url | autoloaded_js.get()])
@package unregister_external_js(url : string) = autoloaded_js.set(List.remove(url, autoloaded_js.get()))
@package register_external_css(url : string) = autoloaded_css.set([url | autoloaded_css.get()])
@package unregister_external_css(url : string) = autoloaded_css.set(List.remove(url, autoloaded_css.get()))

customizer_autoloaded : platform_customization =
  _ -> some(
    { custom_body = none
      custom_headers = none
      custom_js      = List.rev(autoloaded_js.get())
      custom_css     = List.rev(autoloaded_css.get())
    })

default_customizers = [customizer_for_icon,customizer_for_google_frame,required_customizer_for_incompatible_browsers, customizer_autoloaded]

@private cache_xhtml_options = CommandLine.filter({
      title = "Server xhtml resource cache"
      init = {disable=false}
      anonymous = []
      parsers = [
        {
          names       = ["--cache-xhtml-disable"]
          param_doc   = ""
          description = "Disable the server xhtml resource cache"
          on_encounter(_) = {no_params={disable = true}}
          on_param    = CommandLine.no_params
        }
      ]
   })

/**
 * A cache for generation of xhtml resources
 */
@private cache_for_xhtml : resource_cache_entry -> {body:xhtml; head:xhtml; mime_type:string} =
  compute_result(body:xhtml, customizations):{body:xhtml head:xhtml mime_type:string} =
     {html=body_content js=raw_js_content} = Xhtml.prepare_for_export_as_xml_blocks(body)

     {body     = {html=body_custom  js=raw_js_body_custom}
      head     = {html=head_custom  js=raw_js_head_custom}
      js_links = js_links_custom
      ~user_compat} = customizations

     js_inline = Xhtml.finalize_js_inline(raw_js_head_custom<+>raw_js_content<+>raw_js_body_custom)
     {body = <body id="Body">{body_content}{body_custom}{js_links_custom}{js_inline}</body>
      head = head_custom
      //Additional IE-specific fix -- note that the mime type can be ignored if the resource uses [override_mime_type]
      mime_type = match user_compat.renderer with
         /* hack for IE (considers application/xhtml+xml as files to save) */
         | { Trident=_ } -> "text/html"
         /* work-around for Chrome & Safari's bug http://code.google.com/p/chromium/issues/detail?id=45440
            if we serve application/xhtml+xml, we loose the password-saving mechanism for login forms
            but unfortunately, with text/html, we loose some features (e.g. ability to have SVG)
            TODO: remove next line when Chrome bug 45440 is fixed */
         | { Webkit=_; variant=_ } -> "text/html"
         /* application/xhtml+xml is the right content-type by default */
         | _             -> "application/xhtml+xml"
      end }

  compute_everything(customizers, body:xhtml, user_agent) =
     //do jlog("RECOMPUTE")
     customizations  = compute_customization(customizers, user_agent)
     compiled_result = compute_result(body, customizations)
     compiled_result


  f({uri=_ ~customizers ~body user_agent=_}) =
    customizer_cache = Cache.make(
        Cache.Negociator.always_necessary(user_agent -> compute_customization(customizers, user_agent)),
        {Cache.default_options with size_limit = {some = 30}})

    result_cache = Cache.make(
        Cache.Negociator.always_necessary(user_agent -> compute_result(body, customizer_cache.get(user_agent))),
        {Cache.default_options with size_limit = {some = 30}})

   {cache_everything =
        { ~customizers  ~body ~result_cache ~customizer_cache } }

  cache_options = {Cache.default_options with
     size_limit = {some = 30}
     storage    = {ordering = Order.make_by(x -> x.uri, String.order)}//TODO: Declare type (may speed this up)
  }
  global_cache = Cache.make(Cache.Negociator.always_necessary(f), cache_options)

  customizer_equality(a:resource_cache_customizers,b:resource_cache_customizers) =
        a.external_css_files === b.external_css_files
     && a.inline_css_code    === b.inline_css_code
     && a.external_js_files  === b.external_js_files
     && a.inline_js_code     === b.inline_js_code
     && xhtml_equality(a.headers, b.headers)
     && a.customizers        === b.customizers

  strategy(x:resource_cache_entry) =
  (
    {uri=_ ~customizers ~body ~user_agent} = x
    if cache_xhtml_options.disable then compute_everything(customizers, body, user_agent)
    else
    match global_cache.get(x) with
      | {no_caching}        -> //results seems variable, cache deactivated for this [uri]
            compute_everything(customizers, body, user_agent)
      |~{cache_customizers} -> //the body changes, but the customizations don't seem to
            if customizer_equality(cache_customizers.customizers,customizers) then
               //customizers haven't changed, that's a good sign, let's continue
               compute_result(body, cache_customizers.customizer_cache.get(user_agent))
            else
               //customizers have changed, the resource is unstable
               do global_cache.put(x, {no_caching}, void) //    fully deactivate caching for this resource
               compute_everything(customizers, body, user_agent)
      |~{cache_everything}  -> //there's something in the cache, let's check if it's correct
            if customizer_equality(cache_everything.customizers,customizers) then
              //    customizers haven't changed, that's a good sign, let's continue
              if xhtml_equality(cache_everything.body,body) then                      //    body hasn't changed, this is a full hit
                 cache_everything.result_cache.get(user_agent)
              else //    the body has changed, fallback to caching only customization
                 customizer_cache = cache_everything.customizer_cache
                 do global_cache.put(x, {cache_customizers = {~customizers ~customizer_cache}}, void)
                 compute_result(body, customizer_cache.get(user_agent))
            else  //customizers have changed, the resource is unstable
              do global_cache.put(x, {no_caching}, void)                            //    fully deactivate caching for this resource
              compute_everything(customizers, body, user_agent)
    )
    strategy

@private compute_customization(~{customizers external_css_files inline_css_code external_js_files inline_js_code headers}, user_agent)=
  user_compat = UserAgentParser.user_compat(user_agent)
  join_customizations(acc, right)=
       merge(acc: xhtml, right:option(xhtml)) = match right with
          | {none}  -> acc
          | ~{some} -> acc <+> some
      { body          = merge(acc.body, right.custom_body)
        headers       = merge(acc.headers, right.custom_headers)
        css_files     = List.append(acc.css_files, right.custom_css)
        js_files      = List.append(acc.js_files,  right.custom_js) }

  init= { body      = <></> : xhtml
          headers   = headers : xhtml
          css_files = external_css_files: list(string)
          js_files  = external_js_files:  list(string) }

  customizations = Fold.list(customizers,init)(customizer, acc ->
    match customizer(user_compat) with
    { ~some } -> join_customizations(acc, some)
    { none } -> acc
  )

  css_files= customizations.css_files
  js_files= customizations.js_files
  make_links(list)(maker) = Fold.list(list,<></>)(file,accu -> accu <+> maker(file))
  css_links= make_links(css_files)(path -> <link rel="stylesheet" type="text/css" href="{path}"/>)
  js_links=  make_links(js_files)(path -> <script src={path} type="text/javascript" />)
  css_inline= if String.is_blank(inline_css_code) then <></> else <style type="text/css">{inline_css_code}</style>
  js_inline = if String.is_blank(inline_js_code)  then <></> else <script type="text/javascript">{Xhtml.of_string_unsafe(inline_js_code)}</script>
  final_header   =  css_links <+> css_inline <+> customizations.headers <+> js_inline
  {head = Xhtml.prepare_for_export_as_xml_blocks(final_header)
   body = Xhtml.prepare_for_export_as_xml_blocks(customizations.body)
   js_links = Xhtml.compile(js_links)
   ~user_compat}


/**
 * Prepare a resource for sending to the user.
 *
 * @param external_css_files A list of external files containing shared CSS. Links will be added to web pages only.
 * @param inline_css_code Internal CSS styling. Code will be added to web pages only.
 * @param external_js_files A list of external files containing shared JS code. Links will be added to web pages only.
 * @param inline_js_code Internal JavaScript code. Code will be added to web pages only.
 */
export_resource(external_css_files: list(string),
                inline_css_code: string,
                external_js_files: list(string),
                inline_js_code: string,
                base_url: option(string),
                _make_response: (
                    web_cache_control, WebInfo.private.native_request,
                    web_response, string, string
                      -> WebInfo.private.native_response
                  ),
                make_response_with_headers: (
                    WebInfo.private.native_request, web_response,
                    list(Resource.http_header), string, string
                    -> WebInfo.private.native_response
                  )
                ) =
    /**
     * Produce a "ok" HTTP response with the contents of a request.
     *
     * Used for plain text answers.
     */
    make_plain_response_with_headers(mime_type: string, content:string, status, req:WebInfo.private.native_request, headers) =
       make_response_with_headers(req, status, headers, mime_type, content)

//    _make_plain_response(mime_type: string, content:string, status, req:WebInfo.private.native_request, lastm) =
//       _make_response(lastm, req, status, mime_type, content)

    /**
     * Produce an HTTP response with the contents of a request.
     *
     * Used for binary answers.
     */
    make_bin_response_with_headers(mime_type: string, content:binary, status, req:WebInfo.private.native_request, headers) =
       make_response_with_headers(req, status, headers, mime_type, string_of_binary(content))

//    _make_bin_response(mime_type: string, content:binary, status, req:WebInfo.private.native_request, lastm) =
//       _make_response(lastm, req, status, mime_type, string_of_binary(content))

    /**
     * Produce an HTTP response with the contents of a request.
     *
     * Used for UTF-8 answers.
     */
    make_utf_response_with_headers(t, s, status, req, headers) =
      make_response_with_headers(req, status, headers, t ^ "; charset=utf-8", s)

//    _make_utf_response(t, s, status, req, lastm) =
//      _make_response(lastm, req, status, t ^ "; charset=utf-8", s)

    /**
     * The continuation to call to respond to the request
     */
    rec response(force_mimetype)(winfo:web_info, resource: resource)=
      resource_pr = resource
      status = resource_pr.rc_status

      // Various content handler
      handle_bin(out,mime_str)(r) = winfo.cont(make_bin_response_with_headers(force_mimetype ? mime_str,out, status, r, resource_pr.rc_headers))
      handle_utf(out,mime_str)(r) = winfo.cont(make_utf_response_with_headers(force_mimetype ? mime_str, out, status,r, resource_pr.rc_headers))
      handle_utf_no_cache(out,mime_str)(r) =
        winfo.cont(
          make_utf_response_with_headers(
            force_mimetype ? mime_str, out, status, r,
            update_lastm(resource_pr.rc_headers, {volatile})
          )
        )

      // A user agent getter on request
      get_request_ua  = %% BslNet.Requestdef.get_request_ua %% :  WebInfo.private.native_request -> string
      get_request_uri = %% BslNet.Http_server.get_uri %% :  WebInfo.private.native_request -> string

      match resource_pr.rc_content with
      | ~{override_mime_type resource} ->
        resource = {resource_pr with rc_content = resource } : resource
        response(some(override_mime_type))(winfo,resource)
      | {~dynamic} ->
        response(force_mimetype)(winfo, dynamic(HttpRequest._of_web_info(winfo)))
      | {~later} ->
        (r -> later(( (resource : resource) -> response(force_mimetype)(winfo, resource)(r))))

      | { html=body ~doctype ~headers ~customizers } ->
        (
         (req:WebInfo.private.native_request) ->
          //Prepare customizations
          user_agent = get_request_ua(req)
          uri        = get_request_uri(req)
          num_page = match thread_context().key with
            | ~{client}-> do %%BslPingRegister.client_start%%(client) client.page
            | _ -> result = Random.int(max_int)
                   do Log.warning("Resource export",
                      "This page is exported from a context that doesn't have a valid client thread context. Replacing by random value {result}")
                   result
          page_lang = ServerI18n.page_lang() // TODO by customizer
          page_info = "var page_server = {num_page};var page_lang = \"{page_lang}\";"
          js_base_url = Option.switch(base -> "var base_url = \"{base}\";", "", base_url)

          global_variable = {content_unsafe="<script type=\"text/javascript\">{page_info} {js_base_url}</script>"} : xhtml
          {body = ready_body
           head = head_without_id
           mime_type= mime_type} = cache_for_xhtml(
            {~uri
             customizers= ~{customizers external_css_files inline_css_code
                            external_js_files inline_js_code headers}
             ~body ~user_agent})

          base =
            request = HttpRequest._of_web_info(winfo)
            Option.switch(base ->
              Option.switch(host ->
                s = if HttpRequest.Generic.is_secured(request) then "s" else "";
                <base href="http{s}://{host}{base}/" />,
                <>{Xhtml.of_string_unsafe("<!-- no host to set base url -->")}</>,
                HttpRequest.Generic.get_host(request)),
              <></>, base_url)
          ready_head = <head>{base}{head_without_id}{global_variable}</head>

          doctype = match doctype with {some=d} -> html_doctype_to_string(d) {none} -> shared_xhtml1_1_header

          page= Xhtml.of_string_unsafe(doctype) <+>
            <html xmlns="http://www.w3.org/1999/xhtml">{ready_head}{ready_body}</html>

          //Serialize and send
          data = Xhtml.serialize_to_string(page)
          handle_utf_no_cache(data,mime_type)(req) //As we regenerate the page, it should not be cached
      )
/*      | {~soap}  -> (req ->
        soap = shared_xml_header^Xmlns.to_string(soap)
        winfo.cont(make_utf_response(SOAP.mime_type, soap, req, last_modif)))*/
      | {~xml}  ->
        xml = shared_xml_header^Xmlns.to_string(xml)
        handle_utf_no_cache(xml,"application/xml")

      //Other cases are simpler
      | {~png}  -> handle_bin(png,"image/png")
      | {~jpg}  -> handle_bin(jpg,"image/jpeg")
      | {~gif}  -> handle_bin(gif,"image/gif")
      | {~ico}  -> handle_bin(ico,"image/x-icon")
      | {~mimetype ~binary} -> handle_bin(binary,mimetype)

      | {~js}   -> handle_utf(js ,"application/x-javascript")
      | {css=c} -> handle_utf(c  ,"text/css")
      | {~txt}  -> handle_utf(txt,"text/plain")
      | {~json} -> handle_utf(Json.to_string(json),"text/plain")

      | {~mimetype ~source} ->
          (r ->
            winfo.cont(
              make_plain_response_with_headers(mimetype, source, status, r, resource_pr.rc_headers) // should it not be in utf ?
            ))

      response(none)

 }}

@opacapi Resource_private_make_include          = Resource_private.make_include
@opacapi Resource_private_content_of_include    = Resource_private.content_of_include
@opacapi Resource_private_make_resource_include = Resource_private.make_resource_include
@opacapi Resource_private_raw_resource_factory  = Resource_private.raw_resource_factory
