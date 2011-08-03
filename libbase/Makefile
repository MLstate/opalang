
ocamlbasetop :
	ocamlmktop -thread -custom unix.cma threads.cma -cclib -lthreads -cclib cmap.c --cclib rusage.c journal.ml lversion.ml base.ml -o ocamlbasetop
	install *.cmi *.cmo ocamlbasetop ocamlbase $(MLSTATELIBS)/bin
	rm -f *.cmi *.cmo ocamlbasetop

clean :
	rm -f *.cmi *.cmo ocamlbasetop
