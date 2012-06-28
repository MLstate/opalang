import tutorial.recaptcha

/**
 * A test configuration for the reCaptcha, based on a public/private key pair
 * registered for "example.com".
 */
server protected config =
{
  cfg_private: {
    privkey: "6LdVTcQSAAAAAJGuyHbunFigtJ1HuI1nkVtwAahg"
  },
  cfg_public: {
    pubkey: "6LdVTcQSAAAAABMAPRuRawMTsX0vfW2O2c3bDeeL",
    theme: {some: "red"}
  }
}

/**
 * After validation, just display the result.
 */
function after_validation(res) {
  match (res) {
    case { success: _ }: #status = <>success</>;
    case { failure: _ }: #status = <>failure</>;
  }
}

function page() {
  (implementation, recaptcha) = Recaptcha.make(config);
  <div>
    Are you a human being?<br />
    {recaptcha}
    <button onclick={function(_) { Recaptcha.validate(implementation, after_validation)}}> Submit </button>
    <div id=#status></div>
  </div>;
}

/**
 * Main application.
 */
Server.start(Server.http, {title: "Hello, Recaptcha", ~page});

