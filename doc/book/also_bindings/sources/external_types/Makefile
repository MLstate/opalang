all : plugin opa

plugin:
	opa-plugin-builder -o bigint bigint.ml

opa:
	opa bigint.opp bigint.opa

clean:
	rm -rf _build _tracks bigint.opp bigint.exe *.log
