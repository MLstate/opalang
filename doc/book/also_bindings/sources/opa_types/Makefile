# dbm is in a subdirectory of ocaml libs
INCLUDE=-I +dbm
LINK=dbm.cmxa

all : plugin opa

plugin:
	opa-plugin-builder $(INCLUDE) -o dbm dbm.ml

opa:
	opa $(INCLUDE) $(LINK) dbm.opp dbm.opa

clean:
	rm -rf _build _tracks dbm.opp dbm.exe *.log
