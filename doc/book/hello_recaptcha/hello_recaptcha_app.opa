import tutorial.recaptcha

/**
 * A test configuration for the reCaptcha, based on a public/private key pair
 * registered for "example.com".
 */
@server_private config = {
 private = {
   privkey = "6LdVTcQSAAAAAJGuyHbunFigtJ1HuI1nkVtwAahg"
 }
 public  = {
   pubkey =  "6LdVTcQSAAAAABMAPRuRawMTsX0vfW2O2c3bDeeL"
   theme  = {some = "red"}
 }
}

/**
 * After validation, just display the result.
 */
after_validation =
  | {success = _} -> Dom.transform([#status <- <>success</>])
  | {failure = _} -> Dom.transform([#status <- <>failure</>])

/**
 * Main application.
 */
server = one_page_server("Hi", ->
  (implementation, recaptcha) = Recaptcha.make(config)
  <div>
     Are you a human being?<br />
     {recaptcha}
     <button onclick={_ -> Recaptcha.validate(implementation, after_validation)} > Submit </button>
     <div id=#status></div>
  </div>
)
