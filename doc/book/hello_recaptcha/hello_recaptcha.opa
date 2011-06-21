package tutorial.recaptcha
import stdlib.web.client

/**
 * A user interface component based on Google reCaptcha, used to differenciate between
 * human beings and spam/link harvest bots by confronting users to read words from scanned
 * books.
 *
 * This module implements a very thin layer on top of the Google reCaptcha AJAX API.
 * For more information on reCaptcha API, see {{:http://code.google.com/apis/recaptcha/intro.html} the official
 * documentation}.
 */

/**
 * {1 Types defined in this module}
 */

/**
 * {2 Setting up the recaptcha}
 */

/**
 * A configuration for the captcha.
 */
type Recaptcha.config =
{
   /**
    * Private fields, isolated for clarity: for security reasons, content of these fields
    * must never be sent to the client.
    */
   private:
   {
      /**
       * The private access key to the service.
       *
       * Obtain one (and the corresponding public key) from {{:https://www.google.com/recaptcha/admin/create} reCaptcha}
       */
      privkey: string
   }
   /**
    * Public fields, isolated for clarity: the content of these fields can be shared with
    * the client without causing any security issue.
    */
   public:
   {
      /**
       * The public access key to the service.
       *
       * Obtain one (and the corresponding private key) from {{:https://www.google.com/recaptcha/admin/create} reCaptcha}
       */
      pubkey:  string

      /**
       * Name of the visual theme to use for this captcha.
       *
       * If [{none}], we default to the "red" style. Other acceptable styles are: "white", "blackglass",
       * "clean" or "custom". For more details, see the {{:http://code.google.com/apis/recaptcha/docs/customization.html}
       * reCaptcha customization manual}.
       */
      theme:   option(string)
   }
}

/**
 * {2 The reCaptcha object}
 */

/**
 * An abstract object implementing the methods of the reCaptcha.
 */
@abstract type Recaptcha.implementation = {
     /**Place a request to the reCaptcha server to verify that user entry is correct.
        @param challenge
        @param response
        @param callback*/
     validate: (string, string, (Recaptcha.result -> void) -> void)
     reload:   -> void /**Reload the reCaptcha, displaying a different challenge*/
     destroy:  -> void /**Destroy the reCaptcha*/
}


/**
 * {2 Manipulation of recapatcha results}
 */
type Recaptcha.success = {captcha_solved} /**The captcha is correct.*/
type Recaptcha.failure =
     {captcha_not_reachable: WebClient.failure} /**Could not reach the distant server.*/
 /   {upstream: string}      /**Upstream error, documented but left unspecified by Google.*/
 /   {unknown: list(string)} /**Server could be reached, but produced an error that doesn't match the specifications provided by Google.
                                Possible cause: proxy problem.*/
 /   {empty_answer}          /**Recaptcha guidelines mention that we should never send answers that are empty.*/

type Recaptcha.result = {success: Recaptcha.success} / {failure: Recaptcha.failure}


/**
 * {1 Implementation}
 */

Recaptcha =
{{
   /**
    * Construct a reCaptcha.
    *
    * Note: The API provided by Google only permits one reCaptcha per page.
    *
    * @param config The configuration for the recaptcha. Note that, as this configuration
    * contains security-sensitive information, it {e must} be stored on the server.
    *
    * @return An object representing the methods of the reCaptcha, which you can later use
    * to validate an entry, reload the reCaptcha, etc. and a [xhtml] extract which should be
    * inserted in a page to display the captcha dialog.
    */
   make(config: Recaptcha.config): (Recaptcha.implementation, xhtml) =
   (
     id = Dom.fresh_id()
     xhtml = <div id={id} onready={_ -> onready(id, config.public.pubkey, config.public.theme?"red")}/>
     (make_implementation(config.private.privkey), xhtml)
   )

   /**
    * Attempt to determine whether a user is a human being.
    *
    * This request calls upon Google's reCaptcha servers to determine whether
    * the user is probably a human being.
    *
    * @param implementation A reCaptcha.
    * @param callback A function which will be called once the server has provided an answer
    */
   validate(implementation: Recaptcha.implementation, callback: Recaptcha.result -> void): void =
   (
      ~{challenge response} = get_token()
      implementation.validate(challenge, response, callback)
   )

   /**
    * Display a new challenge
    *
    * @param implementation A reCaptcha.
    */
   reload(implementation: Recaptcha.implementation): void =
   (
     implementation.reload()
   )

   /**
    * Clean-up and remove the reCaptcha.
    *
    * If you are using the reCaptcha in a dialog box, it is generally a good idea to call
    * this function if the user closes the box.
    *
    * @param implementation A reCaptcha.
    */
   destroy(implementation: Recaptcha.implementation): void =
   (
     implementation.destroy()
   )

   /**
    * {2 Private implementation details}
    */

   /**
    * The URIs for the API
    */

   @private path_validate_uri = Option.get(Parser.try_parse(Uri.uri_parser, "http://www.google.com/recaptcha/api/verify"))
   @private path_js_uri       = Option.get(Parser.try_parse(Uri.uri_parser, "http://www.google.com/recaptcha/api/js/recaptcha_ajax.js"))

   /**
    * Perform client-side delayed initialization.
    *
    * Load the reCaptcha script if necessary, then perform script initialization.
    *
    * @param id The dom identifier of the xhtml component which will contain the reCaptcha.
    * @param pubkey The public key for this reCaptcha.
    * @param theme The name of the theme for this reCaptcha (in case of doubt, "red" is a good choice).
    */
   @private onready(id: string, pubkey: string, theme: string): void =
   (
      Client.Script.load_uri_then(path_js_uri, ->
              (%% Recaptcha.init %%)(id, pubkey, theme)
      )
   )

   /**
    * Construct the object representing the reCaptcha
    *
    * @param privkey The private key.
    * @return An object containing the necessary methods to contact the reCaptcha server.
    */
   @private make_implementation(privkey: string): Recaptcha.implementation =
   (
      validate(challenge, response, callback:Recaptcha.result -> void) =
      (
        //By convention, do not even post a request if the data is empty
        if String.is_empty(challenge) || String.is_empty(response) then callback({failure = {empty_answer}})
        else
        (
          /**POST request, formatted as per API specifications*/
          data = [("privatekey", privkey),
                  ("remoteip",   "{HttpRequest.get_ip()?(127.0.0.1)}"),
                  ("challenge",  challenge),
                  ("response",   response)]
          /**Handle POST failures, decode reCaptcha responses, convert this to [reCaptcha.result].*/
          with_result =
          (
            | ~{failure} -> callback({failure = {captcha_not_reachable = failure}})
            | ~{success} ->
              details = String.explode("\n", success.content)
              match details with
                | ["true" | _]        -> callback({success = {captcha_solved}})
                | ["false", code | _] -> callback({failure = {upstream = code}})
                | _ -> callback({failure = {unknown = details}})
          )
          /**Encode arguments, POST them*/
          WebClient.Post.try_post_with_options_async(path_validate_uri,
                WebClient.Post.of_form({WebClient.Post.default_options with content = {some = data}}),
                with_result)

        )
      )
      reload() = (%%Recaptcha.reload%%)()       /**Implementation of [reload]*/
      destroy()= (%%Recaptcha.destroy%%)()      /**Implementation of [destroy]*/
      ~{validate reload destroy}
   )

   /**
    * Grab all necessary information from the user interface.
    */
   @private get_token(): {challenge:string response:string} =
   (
     result = ({challenge = (%%Recaptcha.get_challenge%%)()
                response  = (%%Recaptcha.get_response%%)()})
     do (%%Recaptcha.destroy%%)()
     result
   )
}}
