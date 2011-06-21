DIR:=$(shell pwd)

all: freq.o plugin opa

freq.o: freq.c
	$(OCAMLOPT) freq.c

plugin:
	$(OPA_PLUGIN_BUILDER) -o c_binding c_binding.ml

opa:
	$(OPA) --mllopt $(DIR)/freq.o c_binding.opp freq.opa
	@echo "run: ./freq.exe" 

clean:
	rm -rf c_binding.opp
	rm -rf freq.exe freq.o
	rm -rf _build _tracks
