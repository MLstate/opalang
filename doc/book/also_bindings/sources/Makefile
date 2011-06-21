all : plugin opa

plugin:
	$(OPA_PLUGIN_BUILDER) -o plugin plugin.js plugin.ml

opa:
	$(OPA) plugin.opp hello_bindings.opa
	@echo "run: ./hello_bindings.exe"

clean:
	rm -rf hello_bindings.exe
	rm -rf _build _tracks
	rm -rf plugin.opp
