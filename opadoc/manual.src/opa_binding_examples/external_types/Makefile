all : plugin opa

plugin:
	$(OPA_PLUGIN_BUILDER) -o bigint bigint.ml

opa:
	$(OPA) bigint.opp bigint.opa

clean:
	rm -rf _build _tracks bigint.opp bigint.exe *.log
