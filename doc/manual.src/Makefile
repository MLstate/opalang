OCAMLOPT ?= ocamlopt.opt
OPA ?= opa --parser js-like
OPA_PLUGIN_BUILDER ?= opa-plugin-builder

export

EXAMPLES= \
  hello-opa/hello-opa.exe \
  hello_chat/hello_chat.exe \
  hello_chat/hello_chat_min.exe \
  hello_wiki/hello_wiki.exe \
  hello_wiki/hello_wiki_simple.exe \
  hello_web_services/hello_wiki_rest.exe \
  hello_web_services/hello_wiki_rest_client.exe \
  hello_web_services/hello_wiki_rest_client_customizable.exe \
  hello_recaptcha/hello_recaptcha_app.exe

hello_recaptcha/%.exe:
	$(MAKE) -C $(@D) $(@F)

%.exe: %.opa
	cd $(<D) && $(OPA) $(<F)

examples: $(EXAMPLES)
	$(MAKE) -C opa_binding_examples
	$(MAKE) -C opa_binding_examples/c_binding
	$(MAKE) -C opa_binding_examples/external_types
	$(MAKE) -C opa_binding_examples/opa_types

clean:
	rm -rf `find . -name '_build' -o -name '*.broken' -o -name '_tracks' -o -name '*.opp' -o -name '*.opx'`
	rm -rf $(EXAMPLES)
	$(MAKE) -C opa_binding_examples clean
	$(MAKE) -C opa_binding_examples/c_binding clean
	$(MAKE) -C opa_binding_examples/external_types clean
	$(MAKE) -C opa_binding_examples/opa_types clean
