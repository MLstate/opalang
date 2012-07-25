/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
//Implement [Client.load]
//reCaptcha: api/recaptcha

bug = {true} //Replace by {false} to obtain something that works


type Recaptcha.success = {captcha_solved}
type Recaptcha.failure =
     {captcha_not_reachable}
 /   {upstream: string} /**Upstream error, code may be used for debugging purposes.*/
 /   {unknown: list(string)}
 /   {empty_answer}

type Recaptcha.result = {success: Recaptcha.success} / {failure: Recaptcha.failure}

type Recaptcha.config =
{
   private:
   {
      privkey: string
   }
   public:
   {
      pubkey:  string
      theme:   option(string)
   }
}

type Recaptcha.validator = private((string, string, (Recaptcha.result -> void) -> void))

Recaptcha =
{{
   make(config: Recaptcha.config): (Recaptcha.validator, xhtml) =
   (
     id = Dom.fresh_id()
     xhtml = <div id={id} onready={_ -> onready(id, config.public.pubkey, config.public.theme?"red")}/>
     (validator(config.private.privkey), xhtml)
   )

   validate(validator: Recaptcha.validator, callback: Recaptcha.result -> void): void =
   (
      (a, b) = get_token()
      @unwrap(validator)(a, b, callback)
   )

   @private path_api_uri = Option.get(Parser.try_parse(Uri.uri_parser, "http://www.google.com/recaptcha/api/verify"))
   @private path_js      = "http://www.google.com/recaptcha/api/js/recaptcha_ajax.js"

   @private onready(id: string, pubkey: string, theme: string): void =
   (
      do Log.info("onready", "start")
      Client.Script.load_from_then(path_js, ->
              do Log.info("onready", "loaded")
              do (%% Recaptcha.init %%)(id, pubkey, theme)
              do Log.info("onready", "initialized")
              void
              )
   )
   @private validator(privkey: string): Recaptcha.validator =
   (
      payload(challenge, response, callback:Recaptcha.result -> void) =
      (
        if String.is_empty(challenge) || String.is_empty(response) then callback({failure = {empty_answer}})
        else
        (
          data = [("privatekey", privkey),
                  ("remoteip",   "{HttpRequest.get_ip(Option.get(thread_context().request))}"),
                  ("challenge",  challenge),
                  ("response",   response)]
          with_result =
          (
            |  {failure = _} -> callback({failure = {captcha_not_reachable}})
            | ~{success} ->
              details = String.explode("\n", success.content)
              match details with
                | ["true" | _]        -> callback({success = {captcha_solved}})
                | ["false", code | _] -> callback({failure = {upstream = code}})
                | _ -> callback({failure = {unknown = details}})
           )
           if bug then WebClient.Post.try_post_with_options_async(path_api_uri,
                WebClient.Post.of_form({WebClient.Post.default_options with content = {some = data}}),
                with_result) // <= this version doesn't work, callback is called but cannot perform server->client calls
           else
             with_result(WebClient.Post.try_post_with_options(path_api_uri,
                WebClient.Post.of_form({WebClient.Post.default_options with content = {some = data}})))
                             // <= this version works, callback is called but cannot perform server->client calls
        )
      )
      @wrap(payload)
   )
   @private get_token(): (string, string) =
   (
     result = ((%%Recaptcha.get_challenge%%)(), (%%Recaptcha.get_response%%)())
     do (%%Recaptcha.destroy%%)()
     result
   )
}}

config = {
 private = {
   privkey = "6LeeR8QSAAAAADSnODF238nFuhfPQ13OH9sMib6o"
 }
 public  = {
   pubkey =  "6LeeR8QSAAAAANEPtPmwgSATS64g1p4qOg0fpEJH"
   theme  = {some = "red"}
 }
}

after_validation =
  | {success = _} ->
                     do Log.info("Success", "success")
                  Dom.transform([#status <- <>success</>])
  | {~failure}    -> do Log.info("Failure", failure)
                  Dom.transform([#status <- <>failure</>])

server = one_page_server("Hi", ->
  (validator, recaptcha) = Recaptcha.make(config)
  <div>
     {recaptcha}
     <button onclick={_ -> Recaptcha.validate(validator, after_validation)} > Submit </button>
     <div id=#status></div>
  </div>
)
