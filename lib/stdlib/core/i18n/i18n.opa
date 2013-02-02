/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* A language is associated to each user (cookie defined),
     each time a page is generated in destination to the user it get localised with the current associated language (using ServerI18n.page_lang)
     the user can change its langage association (using I18n.set_lang)
     the first time a user connect the association is initialised using Accept-language header field (using ServerI18n.request_lang)

   A initial language is associatd to the server using system specific environment var (using ServerI18n.get_system_lang)
   Currenlty the language can't be updated later and can be consulted (using ServerI18n.get_server_lang)

*/


/* @author Rudy Sicard */

import-plugin {unix, server}
import stdlib.core.parser
import stdlib.core.web.context
import stdlib.core.web.request
import stdlib.core.rpc.core // ensure dependencies for @sliced_expr are met

@opacapi
I18n_lang = I18n.lang

/** Plurials for simple case, bool otherwise : */
type I18n.plurial.ternary = {`0`} / {`1`} / {`2`}
/** and */
type I18n.plurial.quaternary = I18n.plurial.ternary / {`3`}

/** Module for asking and setting current language and other localisation properties */
I18n = {{

  /* TODO, add call back on set_lang, so that any other component is aware of lang update */
  /** change the lang associated to current client context, i.e. change the lang for a specific client */
  set_lang(l) = UserI18n.set_lang(l)

  /** returns the current language
      on client side it is the client language
      on server side with a client context it is the client language
      on server side otherwise it is the server language */
  lang() : I18n.language = @sliced_expr({
    client = ClientI18n.get_page_lang
    server = UserI18n.get_lang()
  })

/*  /** same as above but for region, WIP */
  region() : I18n.region   = {`001`} */

  /** suitable for
Germanic family
    English, German, Dutch, Swedish, Danish, Norwegian, Faroese
Romanic family
    Spanish, Portuguese, Italian, Bulgarian
Latin/Greek family
    Greek
Finno-Ugric family
    Finnish, Estonian
Semitic family
    Hebrew
Artificial
    Esperanto
Finno-Ugric family
    Hungarian
Turkic/Altaic family
    Turkish
   */
  plurial_zero_is_plurial(str:string) = match str
   "1" -> false
    _ -> true

  /** suitable for Brazilian Portuguese, French  */
  plurial(str:string) = match str
   "0" "1" -> false
    _ -> true

  // TODO   http://www.gnu.org/s/hello/manual/gettext/Plural-forms.html#Plural-forms
  /** suitable for ?? */
  ternary_plurial(str:string):I18n.plurial.ternary =
   match str
   "0" -> {`0`}
   "1" -> {`1`}
    _  -> {`2`}

  /** suitable for ?? */
  quaternary_plurial(str:string):I18n.plurial.quaternary =
   match str
   "0" -> {`0`}
   "1" -> {`1`}
    _  -> {`2`}

 // CTYPE, NUMERIC, TIME, COLLATE, MONETARY, ...

}}

@private
UserI18n =
{{

  get_lang_opt() = UserContext.execute(id->id,UC)

  @publish
  get_lang():I18n.language =
    match thread_context().key
    {client=_} -> get_lang_opt()
               ?  do Log.warning("I18n","ServerI18n.page_lang should have access to the lang")
                  ServerI18n.get_server_lang()
   _ -> ServerI18n.get_server_lang()

  @publish
  set_lang(l:I18n.language) =
    match thread_context().key
    {client=_} -> UserContext.change(_->some(l), UC)
    _ -> ServerI18n.set_server_lang(l)
  // Associate cookies and users


  @private
  @server_private
  UC = UserContext.make(none):UserContext.t(option(I18n.language))

}}

@private
@expand
#<Ifstatic:OPA_I18N_DEBUG>
debug(msg) = Log.warning("I18n",msg)
#<Else>
debug(_) = void
#<End>

@server_private
ServerI18n =
{{

  default_lang = "en" : I18n.language

  get_system_lang() =
    // OS specific
    // TODO Mac and Windows case
    env = %% BslSys.get_env_var %%
    extract(s) = Option.bind(extract_lang,env(s))
      extract("LANG")     // Linux, e.g. fr_FR.UTF-8
    ? extract("LANGUAGE") // Linux, e.g. fr_FR:en
    ? //do Log.warning("I18n","ServerI18n.get_system_lang can't get system language")
      default_lang

  initial_server_lang = get_system_lang()

  // TODO : set
  get_server_lang() = initial_server_lang
  set_server_lang(_l) = void

  // TODO make it faster using partial parsing and cache (btw it is called once for each new user)
  request_lang(request) =
    match HttpRequest.Generic.`get_Accept-Language`(request)
    {none} -> get_server_lang()
    {some=al} ->
    match extract_lang(al)
    {none} -> do debug("Extract lang failed on {al}")
              get_server_lang()
    {some=l} -> l


  /** touch lang */
  touch_user_lang(request) =
    match UserI18n.get_lang_opt()
    {none} -> l = request_lang(request) // if user has no lang get it from the request
                UserI18n.set_lang(l)   // and fix it
    _ -> void

  /** the lang for a page generated in a client context */
  page_lang() = I18n_language.to_string(UserI18n.get_lang())

  // will cause problem with UTF-8 on linux LANG
  sep = parser [-_:.,;'`] -> void
  locale_element = parser sep* e=((!sep .)+) sep* -> Text.to_string(e)
  elements = parser l=locale_element* -> l

  extract_lang(s) =
//   do println("Extract lang of {s}")
   cl = Parser.parse(elements,s)
//   do println("Extract lang of {s} => {cl}")
   match List.filter_map(I18n_language.parse ,cl)
    [l|_] -> some(l)
    []    -> none

}}

@private
@client
ClientI18n = {{
  get_page_lang = %% BslI18n.page_lang %% : I18n.language
}}



