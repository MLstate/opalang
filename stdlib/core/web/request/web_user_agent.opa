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

import stdlib.core.{parser, web.core}

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

UserAgentParser =
{{
  user_compat_default = { renderer = { Unidentified } ; environment = { Unidentified } }: user_compat

  /* CACHE TEST TO DELETE */
  @private
   cache_a_partial_try_parse(p) =
   cache = Server_reference.create([])
   s ->
     match List.assoc_gen(String.equals, s, Server_reference.get(cache))
     {~some} -> some
     {none} ->
            r = Parser.partial_try_parse(p,s)
            do Server_reference.set(cache,
            // Server_reference.update(cache, // TODO : understand why .update is so slow
                   // ( l ->
                   l=Server_reference.get(cache)
                   if List.length(l) < 100 then [(s,r)|l]
                   else [(s,r)|List.tail(List.rev(l))]
                   )//)
            r

  @private
  user_compat_parsers = {{
    not_number = parser ((![0-9] .)*) -> void
    version = parser
      | i=Rule.natural "." tl=version -> [i|tl]
      | i=Rule.natural               -> [i]

    grep(p) = parser
      | (!p .)* p -> void
    grep_nn(p) = parser
      | {grep(p)} not_number -> void
//Browser
    gecko = parser
      "Mozilla/5.0" ((!"rv:" .)+) not_number ver=version ((!"Gecko" .)+) "Gecko" .* -> ver
    presto = parser
      | "Opera/" ((!"Presto" .)+) not_number ver=version  -> ver
      | "Opera/"                                          -> []
    msie = parser
      | {grep_nn(parser "MSIE")} ver=version  -> ver
    webkit_variant = parser
      | {grep_nn(parser "Chrome/")} ver=version -> {Chrome = ver} : webkit_variant //Note: this case must come before [Safari], as Chrome also identifies
      | {grep_nn(parser "Safari/")} ver=version -> {Safari = ver} : webkit_variant //itself as Safari
      | .* -> { Unidentified } : webkit_variant
    webkit = parser
      "Mozilla/5.0" ((!"AppleWebKit" .)+) not_number ver=version variant=webkit_variant -> {Webkit = ver; variant=variant} : renderer_engine
    bot = parser
      | {grep_nn(parser "Googlebot/")} ver=version  ->                            { Googlebot = ver} : bot_engine
      | {grep_nn(parser "msnbot" ("-media" | "-webmaster" )? "/")} ver=version -> { Msnbot    = ver} : bot_engine
      | {grep_nn(parser "Yahoo! Slurp")}                                       -> { Yahoobot }       : bot_engine
    nokia = parser
      | "Nokia" ((!"(" .)+) "(" ver=version ")" -> ver
      | "Nokia"                                 -> []
    p_re = parser
      | r=bot    -> { Bot    =   r }
      | r=msie   -> { Trident=   r }
      | r=gecko  -> { Gecko=     r }
      | r=presto -> { Presto=    r }
      | r=webkit -> r
      | r=nokia  -> { Nokia=     r }

//Operating system
    windows = parser
      | {grep(parser "Windows")} -> { Windows }
    x11 = parser
      | {grep(parser "X11")} -> { X11 }
    macintosh = parser
      | {grep(parser "Macintosh")} -> { Macintosh }
    iphone = parser
      | {grep(parser ("iPhone" | "iPod"))} -> { iPhone }
    symbian = parser
      | {grep(parser "Nokia")} -> { Symbian }
    p_eie = parser
      | r=windows   -> r
      | r=x11       -> r
      | r=macintosh -> r
      | r=iphone    -> r
      | r=symbian   -> r

//Putting it all together
    c_pre : string -> option(renderer_engine) = cache_a_partial_try_parse(p_re)
    c_peie : string -> option(environment_interface_engine) = cache_a_partial_try_parse(p_eie)
  }}
  user_compat(user_agent) =
    re = user_compat_parsers.c_pre(user_agent)   ? { Unidentified }
    eie = user_compat_parsers.c_peie(user_agent) ? { Unidentified }
    { renderer = re; environment = eie }: user_compat
}}
