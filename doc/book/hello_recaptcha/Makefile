hello_recaptcha_app.exe: recaptcha.opp hello_recaptcha.opa hello_recaptcha_app.opa
	$(OPA) $^

recaptcha.opp: recaptcha.js
	$(OPA_PLUGIN_BUILDER) recaptcha.js -o recaptcha
