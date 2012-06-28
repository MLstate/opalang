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
/*
    @authors Hugo Heuzard
**/

/**
 * Framework 
 */

//package framework

/**
 * @author Hugo Heuzard
 *
 * {1 About this package}
 *
 * This package aims to provide a small framework that handle pages' creation,
 * navigations throw pages, pages' access credentials, ...
 *
 * {1 Where should I start?}
 * 
 * First : Create an application.
 * Second : Declare all your entry point ("Page.Handler.t") with or without parameters.
 * Third : Create your pages ("Page.Content")
 * Finaly : Connect your pages to corresponding entry points. 
 *
 * {1 What if I need more?}
 * 
**/


// TODO
// Cache client
// Action
// block system
// on_leave

/**
 * {1 Types defined in this module}
 */
@private
type Application.State.msg =
  {register path:string page:Page.Private.page}
/ {get}
 
/**
 * The name of an application. This is mostly use for information purpose.
**/
type Application.name = string

@private
type Application.State.result =
  option(stringmap(Page.Private.page))

/**
 * Internal private state of an application.
**/ 
@abstract
type Application.State.t = Cell.cell(Application.State.msg, Application.State.result)

/**
 * Frame Maker type. 
**/
type Application.frame_maker('state, 'cred) =  Application.t('state, 'cred),xhtml,string -> xhtml

/**
 * Type of function that generate the top bar title
**/
type Application.title = string -> string

/**
 * The application type
**/
type Application.t('state, 'cred) = {
  frame_maker : Application.frame_maker('state, 'cred)
  title : Application.title
  pages : Application.State.t
  option : Application.option('state, 'cred)
  id : string
  state : 'state
  get_cred : 'state -> 'cred
  name : Application.name
}

/**
 * The application's options
**/
type Application.option('state, 'cred) = {
  /** Page to serve when no page's found. The string argument is the last part of the url. **/
  not_found : Page.Content.t('state, 'cred, string)
  /** Page to serve when credential are missing. The string argument is the last part of the url. **/
  unauthorized : Page.Content.t('state, 'cred, string)
  /** true if you want to use anchor to change page. **/
  single_page : bool
  /** List of style sheet to include in the header. **/
  css_uris : list(string)
  /** List of javascript file to include in the header. **/
  js_uris : list(string)
  /** List of custom headers. **/
  headers : list(xhtml)
  /** Advance option, to precompile xhtml on server side. **/
  precompile_xhtml : bool
}

import stdlib.web.client

Application = {{
  @server @private
  State = {{
    get(appl) = Option.get(Cell.call(appl.pages, {get}))
    register(appl,handler,page) =
      black_page = {
        title=page.title
        content_parser = parser state=handler.param_parser -> page.content(appl,appl.get_cred(appl.state),state)
      }
      ignore(Cell.call(appl.pages, {register path=handler.path page=black_page}))
    msg_handler(state,msg) = match msg with
      | {get} -> {return=some(Map.filter_map(x -> x,state)) instruction={unchanged}}
      | {register ~path ~page} -> {return=none instruction={set=
        match Map.get(path, state) with
          | {some={none}}
          | {none} -> Map.add(path,{some=page},state)
          | {some=_} ->
            do Log.warning("Application.register", "Page already registered ({path})")
            state
        }}
  }}

  /** Defautl application option. **/
  default_option : Application.option('state,'cred) = {
    not_found = {title=->"not found" content=_,_,_ -> {xhtml=Xhtml.precompile(<>Not Found</>)} }
    unauthorized = {title=->"unauthorized" content=_,_,_ -> {xhtml=Xhtml.precompile(<>unauthorized</>)} }
    single_page = true
    precompile_xhtml = false
    css_uris = []
    js_uris = []
    headers = []
  }

  @private @client
  client_handler = Mutable.make((_:string) ->void)

  /**
   * Make an application.
   * name : The name of your application
   * title : function that generate a top bar title for each pages
   * frame_maker : function that generate a common frame for your pages
   * state : a state for application.
   * get_cred : a function to extract credentials from state 
  **/
  @server
  make(name : string, title : Application.title, frame_maker : Application.frame_maker, state, get_cred) : Application.t =
    make_with(name, title, frame_maker, default_option, state, get_cred)

  /**
   * Make an application with custom options. See [make]
  **/
  @server
  make_with(name : string, title : Application.title, the_frame_maker : Application.frame_maker,
            option : Application.option, state ,get_cred) : Application.t =
    id = Random.string(10)
    pages=Cell.make(Map.empty,State.msg_handler)
    option={option with not_found.title=->title(option.not_found.title())}
    frame_maker =
      if option.single_page
      then
        fm(appl, x : xhtml,id : string) =
          ss = Session.make_callback(get_page_async(id,appl,_))
          <div onready={_ ->
          do client_handler.set(Session.send(ss,_))
          do ignore(Client.Anchor.add_handler(Session.send(ss,_)))//get_page_async(appl,_)))
          anchor = Client.Anchor.get_anchor()
          s = if (String.length(anchor)>=1 && String.get(0,anchor)=="#")
              then String.drop_left(1,anchor)
              else if (String.length(anchor)>=3 && (String.get_prefix(3,anchor)?"")=="%23")
              then String.drop_left(3,anchor)
              else anchor
          Session.send(ss,s)//get_page_async(appl,s)
          }>{the_frame_maker(appl, x, id)}</div>
        fm
      else the_frame_maker
    appl = {~frame_maker ~title ~pages ~id ~state ~get_cred ~option ~name}
    appl

  /**
   * Register a page
  **/
  @server
  register(appl : Application.t('state, 'cred), handler : Page.Handler.t('param), page :  Page.Content.t('state, 'cred, 'param)) =
    do State.register(appl,handler,page)
    do Page.Handler.State.register(appl.name, handler)
    void

  /**
   * Register a page with credential checks
  **/
  @server
  register_secure_with(appl : Application.t, handler : Page.Handler.t('param), page :  Page.Content.t('state, 'cred, 'param), check : ('cred -> bool)) =
    page = {
      title=page.title
      content(appl,cred,param)=
        if check(cred)
        then page.content(appl,cred,param)
        else appl.option.unauthorized.content(appl,cred,"")
    }
    register(appl, handler, page)

  @server @private
  get_parser(appl : Application.t) =
    parser page={Rule.of_map(State.get(appl))} content=page.content_parser ->
    ~{content page}

  @server @private
  get_page(appl : Application.t, path) : {title : string ; content : 'a } =
    cred = appl.get_cred(appl.state)
    match Parser.try_parse(get_parser(appl), path) with
      | {some={~page ~content}} ->
        do Log.info("Application.get_page","Got page {page.title()}")
        content=match content with
          | {xhtml=content} ->
            if appl.option.precompile_xhtml
            then {xhtml=Xhtml.precompile(content)}
            else {xhtml=content}
          | x -> x
        end
        {title=appl.title(page.title()) ~content}
      | {none} -> {title = appl.title(appl.option.not_found.title())
                   content=appl.option.not_found.content(appl,cred,path)}

  @private @server @publish @async
  get_page_async(appl_id, appl : Application.t, path) =
    page = get_page(appl, path)
    update_dom(appl_id,page)

  @server @private
  make_header(appl)=
    <>
      {appl.option.headers}
      {List.map(s -> <link rel="stylesheet" type="text/css" media="all" href="{s}" />, appl.option.css_uris)}
      {List.map(s -> <script type="text/javascript" src="{s}" />, appl.option.js_uris)}
    </>

  /** Generate the parser of an application **/
  @server_private
  make_parser(appl : Application.t) =
    page(content : Page.Content.return,title : ->string) = (_ ->
      match content with
        | {xhtml=content} ->
          Resource.full_page(appl.title(title()), appl.frame_maker(appl,content,appl.id), make_header(appl), {success}, [])
        | {redirection=s} ->
          Resource.redirection_page(title(), <></>, {address_moved}, 0, s)
      end)
    if appl.option.single_page
    then parser "/" -> page({xhtml=<></>},->"")
              | "/" x=(.*) -> page({redirection="/#{Text.to_string(x)}"},->"")
    else parser "/" x={get_parser(appl)} -> page(x.content,x.page.title)

  /** Generate the server of an application **/
  @server_private
  make_server(appl : Application.t) =
    Server.make(make_parser(appl))

  @private
  make_path_with(handler : Page.Handler.t('param), param : 'param) : string =
    "{handler.path}{handler.param_serializer(param)}"

  @client @private
  update_dom(id,page) =
    do Client.setTitle(page.title)
    match page.content with
      | {xhtml=content} -> Dom.transform([#{id} <- content])
      | {redirection=redirect} -> Client.goto(redirect)
    end

  /** Client side function to refresh the current page **/
  @client
  refresh(appl : Application.t) =
    anchor = Client.Anchor.get_anchor()
    s =
      if (String.length(anchor)>=1)
      then String.drop_left(1,anchor)
      else ""
    if appl.option.single_page
    then client_handler.get()(s) //Scheduler.push(->get_page_async(appl.id,appl,s))
    else Client.reload()

  @client
  is_page(handler : Page.Handler.t) : bool =
    s =
      anchor = Client.Anchor.get_anchor()
      if (String.length(anchor)>=1)
      then String.drop_left(1,anchor)
      else ""
    s2 = Client.get_location().pathname
    p=parser {Rule.of_string(handler.path)} handler.param_parser -> true
    Parser.try_parse(p,s) ? Parser.try_parse(p,s2) ? false

  is_pages(handlers : list(Page.Handler.t)) : bool =
    List.exists(is_page,handlers)

  /** Client side function to go to a page with default arguments. **/
  @client
  goto(appl : Application.t, handler : Page.Handler.t) : void=
    goto_with(appl, handler, handler.default_param)

  /** Client side function to go a page with custom arguments. **/
  @client
  goto_with(appl : Application.t, handler : Page.Handler.t('param), param : 'param) : void =
    path = make_path_with(handler, param)
    if appl.option.single_page
    then Client.Anchor.set_anchor(path)
    else Client.goto(path)

  /** Generate an uri of a page. To be use in href. **/
  make_uri(appl : Application.t, handler : Page.Handler.t) : string =
    make_uri_with(appl, handler, handler.default_param)

  /** Generate an uri of a page with custom arguments. **/
  make_uri_with(appl : Application.t, handler : Page.Handler.t('param) , param : 'param) : string =
    uri = make_path_with(handler, param)
    if appl.option.single_page then "#{uri}" else uri


  /** Generate an uri of a page. To be use in href. **/
  make_static_uri(handler : Page.Handler.t) : string =
    make_static_uri_with(handler, handler.default_param)

  /** Generate an uri of a page with custom arguments. **/
  make_static_uri_with(handler : Page.Handler.t('param) , param : 'param) : string =
    make_path_with(handler, param)




  /** Check that all Page.Handler.t have a Page.Content.t registered to it.
   * It prints logs if error
  **/
  check_one(appl : Application.t) : void = Page.Handler.State.check([appl.name])
  check_several(appls : list(Application.t)) : void = Page.Handler.State.check(List.map(a -> a.name, appls))
}}

/**
 * The page handler type.
**/

type Page.Handler.t('param) = {
  name_prefix : string
  path : string
  param_parser : Parser.general_parser('param)
  param_serializer : 'param -> string
  default_param : 'param
}

@private
type Page.Private.page =
  {content_parser : Parser.general_parser(Page.Content.return) title : ->string }

/**
 * The type of a page content
 */
type Page.Content.return =
  {xhtml : xhtml}
/ {redirection : string}

/**
 * The type of a page.
 */
type Page.Content.t('state, 'credential,'param) = {
  content : Application.t('state, 'credential), 'credential, 'param -> Page.Content.return
  title : -> string
}

@private
type Page.Handler.State.msg =
  {register path:string appl:Application.name}
/ {create path:string}
/ {check appls:list(Application.name)}

// type Page.Block('param) = {
//   id : string
//   default_param : 'param
//   create : 'param -> xhtml
//   update : 'param -> void
// }

@private
type Page.Handler.State.result = void

Page = {{
  Handler = {{
    @server @package
    State = {{
      @private
      state = Cell.make(Map.empty,(state,msg ->
        match msg : Page.Handler.State.msg with
          | {create ~path} ->
            if(Map.mem(path,state))
            then
              do Log.warning("Application.register", "Page already created ({path})")
              {return=void instruction={unchanged}}
            else {return=void instruction={set=Map.add(path,[],state) }}
          | {register ~appl ~path} ->
            match Map.get(path,state) with
              | {none} ->
                do Log.warning("Application.register", "Page handler not created ({path})")
                {return=void instruction={unchanged}}
              | {some=l} ->
                if List.mem(appl, l)
                then
                  //do Log.warning("Application.register", "Page already registered ({path})")
                  {return=void instruction={unchanged}}
                else
                  {return=void instruction={set=Map.add(path,[appl|l],state)}}
            end
          | {check ~appls} ->
            check(k,l) =
              List.iter(a ->
                ok = List.mem(a,l)
                if not(ok)
                then _ = k void // Log.warning("Application.register", "Page handler not registered ({a}{k})")
                ,appls)
            do Map.iter(check,state)
            {return=void instruction={unchanged}}

        )) : Cell.cell(Page.Handler.State.msg,Page.Handler.State.result)

      create(h : Page.Handler.t('a)) : void =
        Cell.call(state, {create path="{h.name_prefix}:{h.path}"})

      register(name, h : Page.Handler.t('a)) : void =
        Cell.call(state, {register appl=name path="{h.name_prefix}:{h.path}"})

      check(appls) : void= Cell.call(state, {check ~appls})

    }}
    /** Make a page handler **/
    @server
    make(path : list(string)) : Page.Handler.t(void) =
      void_parser = parser "" -> void
      void_serializer(_) = ""
      make_with( path, void_parser, void_serializer, void)

    /** Make a page handler that handle arguments **/
    @server
    make_with(path : list(string), param_parser, param_serializer, default_param : 'param) : Page.Handler.t('param) =
      path=String.concat("/",path)
      handler = ~{name_prefix="" path default_param param_serializer param_parser}
      do State.create(handler)
      handler

    // combine(path, h1 : Page.Handler.t('param1), h2 : Page.Handler.t('param2)) : Page.Handler.t(('param1,'param2)) =
    //   p = parser p1=h1.param_parser p2=h2.param_parser -> (p1,p2)
    //   s((p1,p2)) = "{h1.param_serializer(p1)}{h2.param_serializer(p2)}"
    //   d = (h1.default_param,h2.default_param)
    //   make_with( path, p, s, d)

    // copy( path, h) =
    //   make_with( path, h.param_parser, h.param_serializer, h.default_param)
  }}

  Content = {{
    /** Make a page that take arguments and return xhtml **/
    @server_private
    make_with(title : -> string, content : (Application.t('state,'cred),'cred,'param -> xhtml)) : Page.Content.t('state,'cred,'param) =
      ~{title content=(a,cred,p -> {xhtml=content(a,cred,p)}) }
    /** Make a page that return xhtml **/
    @server_private
    make(title :-> string, content : (Application.t('state,'cred),'cred -> xhtml)) : Page.Content.t('state,'cred, void) =
      make_with(title, (appl,cred,{} -> content(appl,cred)))
    /** Make a page that return xhtml or redirect to another page **/
    @server_private
    make_or_redir(title :-> string, content : (Application.t('state,'cred),'cred -> Page.Content.return)) : Page.Content.t('state,'cred,void) =
      make_or_redir_with(title, (appl,cred,{} -> content(appl,cred)))
    /** Make a page that take arguments and return xhtml or redirect to another page */
    @server_private
    make_or_redir_with(title :-> string, content : (Application.t('state,'cred),'cred,'param -> Page.Content.return)) : Page.Content.t('state,'cred, 'param) =
      ~{title content}
    /** Return xhtml content **/
    @server_private
    return(xhtml : xhtml) : Page.Content.return = {~xhtml}
    /** Redirect to a page **/
    @server_private
    redirect(redirection : string) : Page.Content.return = {~redirection}
  }}
}}

