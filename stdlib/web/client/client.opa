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
@abstract type Client.Anchor.handler = -> void

type position = {x:int y:int} // FIXME: MOVE or KILL

/* Js types definition */
type JsTimerObjectI = external
type jstimerobjecti = JsTimerObjectI
type JsTimerObjectT = external
type jstimerobjectt = JsTimerObjectT
type JsFunction = external

@both Client =
{{

  /**
   * Navigate to a given URI
   *
   * @param to A destination. If the URI only implies an anchor change, this will not prompt a page reload
   */
  go(to: Uri.uri): void =
    %% BslClient.Client.goto %%(Uri.to_string(to))

  /**
   * Load dynamically and execute some JavaScript code.
   *
   * You will generally wish to combine uses of [Script] with a BSL binding to give access to
   * the code from Opa (or the opposite).
   */
  Script =
  {{
    load_uri(source: Uri.uri): void =
    (
      load_uri_then(source, ->void)
    )
    load_uri_then(source: Uri.uri, callback: -> void): void =
    (
      load_from_then(Uri.to_string(source), callback)
    )

    load_from(source: string): void =
    (
       load_from_then(source, -> void)
    )
    load_from_then(source: string, callback: -> void): void =
    (
       %% BslClientOnly.load_js_then %%(source, callback)
    )
  }}

  Anchor =
  {{
     /**
      * Move forward in browser history and register a callback to be invoked if the user navigates back to this point in history.
      *
      * @param state A callback which will be invoked if the user navigates to the given URI, generally by pressing the "backwards" button
      */
     push_state(state: (string -> void)): Uri.uri =
     (
          name = %% BslAnchors.push_state%%(state)
          uri  = {path=[] fragment={some = name} query=[] is_from_root=true is_directory=false}
          //do %% BslClient.Client.goto %%(Uri.to_string(uri))
          uri
     )

     /**
      * Attach a callback (typically, a parser) to navigation between anchors
      *
      * @param callback A function which will be called whenever the user navigates between anchors
      * @return a reference to the anchor handler, which may be used at a later stage to remove the callback
      */
     add_handler(callback:(string -> void)): Client.Anchor.handler =
     (
          %% BslAnchors.add_history_handler %%(callback)
     )

     remove_handler(handler: Client.Anchor.handler): void =
     (
        do %% BslAnchors.remove_history_handler %%(handler)
        void
     )

     /**
      * Generate a new random anchor URI and the corresponding XHTML node
      */
     make(): (Uri.uri, xhtml) =
     (
        anchor = Random.string(32)
        uri    = {path=[] fragment={some = anchor} query=[] is_from_root=true is_directory=false};
        tag    = <a name="{anchor}" />
        (uri, tag)
     )

     /**
      * Prepare a xhtml which, will attach a handler to any document to which it is attached
      *
      * Note: For the moment, the handler returned is a dummy. Using [remove_handler] on it will have no effect.
      */
     make_handler(callback: string -> void): (Client.Anchor.handler, xhtml) =
     (
          ((-> void), <script onready={_ -> _ = add_handler(callback) void}/>)
     )

     /**
      * Get the current anchor
      */
     get_anchor = %% BslAnchors.get_anchor %% : -> string

     /**
      * Set the anchor
      */
     set_anchor(anchor) = (%% BslAnchors.set_anchor %%)(anchor)

  }}
  reload          = %% BslClient.Client.reload %%: -> void

  get_cookie() =
    get_cookie = %%BslClientOnly.get_cookie%%
    bad_cookie= %%BslClientOnly.bad_cookie%%
    affect= parser
      | ident=Rule.ident Rule.ws "=" Rule.ws text=((!";" .)+) Rule.ws -> (ident, Text.to_string(text))
    rec cookie= parser
      | affect=affect Rule.ws ";" Rule.ws cookie=cookie? -> List.cons(affect, cookie ? [])
      | affect=affect Rule.ws ";"? Rule.ws Rule.eos -> [affect]
      | Rule.eos -> []
    get_stable= source -> Option.lazy_default(bad_cookie, List.nth(1, String.explode("_",source)))
    Option.lazy_switch(get_stable, bad_cookie, Map.get("id", Map.From.assoc_list(Parser.parse(cookie, get_cookie()))))


  get_raw_cookie = %%BslClientOnly.getCookie%%
  setTimeout = %%BslClientOnly.setTimeout%%
  setInterval = %%BslClientOnly.setInterval%%
  clearTimeout = %%BslClientOnly.clearTimeout%%
  clearInterval = %%BslClientOnly.clearInterval%%

  /*
   * Deprecated zone: start
   */

   // FIXME: MOVE (don't kill -- used in TT)
   scrollBy(x, y) =
     by = %%BslClientOnly.scrollby%%
     by(x, y)

  register_anchor(name:string, cb:(->void)): void =
  (
     _ = Anchor.add_handler(s -> if s==name then _ = cb() void else void)
     void
  )

  setTitle        = %% BslClient.Client.setTitle %%: string -> void
  width           = %% BslClient.Client.width %%: -> int
  height          = %% BslClient.Client.height %%: -> int
  goto            = %% BslClient.Client.goto %%: string -> void
  alert           = %% BslClient.Client.alert %%: string -> void
  confirm         = %% BslClient.Client.confirm %%: string -> bool
  prompt          = %% BslClient.Client.prompt %%: string, string -> option(string)


  /**
   * This function is deprecated, and does not belong to this module
   * WARNING : return -1 if the event doesn't contains a keyCode!!!
   * TODO : Remove it (beware some applications use it, triptizer, etc...)
   */
  get_keycode(event : Dom.event) =
    event.key_code ? (do Log.warning("Client.get_keycode", "No keycode present on event (return -1)"); -1)

  stop            = %% BslClient.Client.stop %%: -> void
  historyGoto     = %% BslClient.Client.historyGoto %%: int -> void
  historyLength   = %% BslClient.Client.historyLength %%: -> int
  main_window     = %% BslClient.Client.the_window %%: -> client_window_element



  winopen(url : string, jswin : JsWindow, lis : list(string), b : bool) : client_window_element =
   the_window = match jswin with
     | {_self}
     | {_blank} -> main_window()
     | ~{win}   -> win
   end
   bypass2 = %% BslClient.Client.winopen2 %% : string, client_window_element, list(string), bool, string -> client_window_element
   bypass2(url, the_window, lis, b, Random.string(32))

  winclose(win_opt : option(client_window_element)) =
    close = %% BslClient.Client.winclose %%: client_window_element -> void
    close(win_opt ? main_window())

  /*
   * Deprecated zone: stop
   */


  }}

/*
 * Deprecated zone: start
 */

type JsWindow  = { _blank }
               / { _self }
               / { win: client_window_element }
type jswindow = JsWindow
type client_window_element = external



type resolution = { width: int height: int }

/*
 * Deprecated zone: end
 */
