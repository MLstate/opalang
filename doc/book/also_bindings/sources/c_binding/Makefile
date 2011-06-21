DIR:=$(shell pwd)

all: freq.o plugin opa

freq.o: freq.c
	ocamlopt.opt freq.c

plugin:
	opa-plugin-builder -o c_binding c_binding.ml

opa:
	opa --mllopt $(DIR)/freq.o c_binding.opp freq.opa
	@echo "run: ./freq.exe" 

clean:
	rm -rf c_binding.opp
	rm -rf freq.exe freq.o
	rm -rf _build _tracks
