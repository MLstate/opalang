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
   {
     /**
      * The private access key to the service.
      *
      * Obtain one (and the corresponding public key) from {{:https://www.google.com/recaptcha/admin/create} reCaptcha}
      */
     string privkey
   }
   /**
    * Private fields, isolated for clarity: for security reasons, content of these fields
    * must never be sent to the client.
    */
   cfg_private,

   {
      /**
       * The public access key to the service.
       *
       * Obtain one (and the corresponding private key) from {{:https://www.google.com/recaptcha/admin/create} reCaptcha}
       */
      string pubkey,

      /**
       * Name of the visual theme to use for this captcha.
       *
       * If [{none}], we default to the "red" style. Other acceptable styles are: "white", "blackglass",
       * "clean" or "custom". For more details, see the {{:http://code.google.com/apis/recaptcha/docs/customization.html}
       * reCaptcha customization manual}.
       */
      option(string) theme
   }
   /**
    * Public fields, isolated for clarity: the content of these fields can be shared with
    * the client without causing any security issue.
    */
   cfg_public
}

/**
 * {2 The reCaptcha object}
 */

/**
 * An abstract object implementing the methods of the reCaptcha.
 */
abstract type Recaptcha.implementation = {
     /**Place a request to the reCaptcha server to verify that user entry is correct.
        @param challenge
        @param response
        @param callback*/
     (string, string, (Recaptcha.result -> void) -> void) validate,
     /**Reload the reCaptcha, displaying a different challenge*/
     (-> void) reload,
     /**Destroy the reCaptcha*/
     (-> void) destroy
}

/**
 * {2 Manipulation of recapatcha results}
 */
type Recaptcha.success = {captcha_solved} /**The captcha is correct.*/
type Recaptcha.failure =
     {WebClient.failure captcha_not_reachable} /**Could not reach the distant server.*/
  or {string upstream}      /**Upstream error, documented but left unspecified by Google.*/
  or {list(string) unknown} /**Server could be reached, but produced an error that doesn't match the specifications provided by Google.
                                Possible cause: proxy problem.*/
  or {empty_answer}          /**Recaptcha guidelines mention that we should never send answers that are empty.*/

type Recaptcha.result = {Recaptcha.success success} or {Recaptcha.failure failure}

/**
 * {1 Implementation}
 */

module Recaptcha
{
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
   function (Recaptcha.implementation, xhtml) make(Recaptcha.config config) {
       id = Dom.fresh_id();
       xhtml = <div id={id} onready={function(_) { onready(id, config.cfg_public.pubkey, config.cfg_public.theme?"red") }}/>;
       (make_implementation(config.cfg_private.privkey), xhtml);
   }

   /**
    * Attempt to determine whether a user is a human being.
    *
    * This request calls upon Google's reCaptcha servers to determine whether
    * the user is probably a human being.
    *
    * @param implementation A reCaptcha.
    * @param callback A function which will be called once the server has provided an answer
    */
   function void validate(Recaptcha.implementation implementation, (Recaptcha.result -> void) callback) {
       t = get_token();
       implementation.validate(t.challenge, t.response, callback);
   }

   /**
    * Display a new challenge
    *
    * @param implementation A reCaptcha.
    */
   function void reload(Recaptcha.implementation implementation) {
       implementation.reload();
   }

   /**
    * Clean-up and remove the reCaptcha.
    *
    * If you are using the reCaptcha in a dialog box, it is generally a good idea to call
    * this function if the user closes the box.
    *
    * @param implementation A reCaptcha.
    */
   function void destroy(Recaptcha.implementation implementation) {
       implementation.destroy();
   }

   /**
    * {2 Private implementation details}
    */

   /**
    * The URIs for the API
    */

   private path_validate_uri =
     Option.get(Parser.try_parse(Uri.uri_parser, "http://www.google.com/recaptcha/api/verify"))

   private path_js_uri =
     Option.get(Parser.try_parse(Uri.uri_parser, "http://www.google.com/recaptcha/api/js/recaptcha_ajax.js"))

   /**
    * Perform client-side delayed initialization.
    *
    * Load the reCaptcha script if necessary, then perform script initialization.
    *
    * @param id The dom identifier of the xhtml component which will contain the reCaptcha.
    * @param pubkey The public key for this reCaptcha.
    * @param theme The name of the theme for this reCaptcha (in case of doubt, "red" is a good choice).
    */
   private function void onready(string id, string pubkey, string theme) {
      Client.Script.load_uri_then(path_js_uri,
        function(){
            (%% Recaptcha.init %%)(id, pubkey, theme);
        }
      )
   }

   private cl_reload =
     %%Recaptcha.reload%% /**Implementation of [reload]*/

   private cl_destroy =
     %%Recaptcha.destroy%% /**Implementation of [destroy]*/

   /**
    * Construct the object representing the reCaptcha
    *
    * @param privkey The private key.
    * @return An object containing the necessary methods to contact the reCaptcha server.
    */
   private function Recaptcha.implementation make_implementation(string privkey) {
      function validate(challenge, response, (Recaptcha.result -> void) callback) {
        //By convention, do not even post a request if the data is empty
        if (String.is_empty(challenge) || String.is_empty(response)) {
            callback({failure: {empty_answer}});
        } else {
          /**POST request, formatted as per API specifications*/
          data = [ ("privatekey", privkey)
                 , ("remoteip",   "{HttpRequest.get_ip()?(127.0.0.1)}")
                 , ("challenge",  challenge)
                 , ("response",   response)
                 ];
          /**Handle POST failures, decode reCaptcha responses, convert this to [reCaptcha.result].*/
          function with_result(res) {
              match (res) {
              case ~{failure}:
                  callback({failure: {captcha_not_reachable: failure}});
              case ~{success}:
                  details = String.explode("\n", success.content);
                  match (details) {
                  case ["true" | _]: callback({success: {captcha_solved}});
                  case ["false", code | _]: callback({failure: {upstream: code}});
                  default: callback({failure: {unknown: details}});
                  }
              }
          }
            /**Encode arguments, POST them*/
            WebClient.Post.try_post_with_options_async(path_validate_uri,
                WebClient.Post.of_form({WebClient.Post.default_options with content: {some: data}}),
                with_result);
        }
      }
      {~validate, reload:cl_reload, destroy:cl_destroy};
   }

   /**
    * Grab all necessary information from the user interface.
    */
   private function {string challenge, string response} get_token() {
       result = ({ challenge: (%%Recaptcha.get_challenge%%)()
                   , response: (%%Recaptcha.get_response%%)()
                 });
       (%%Recaptcha.destroy%%)();
       result;
   }

}
