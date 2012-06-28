all : plugin opa

# to build the plugin, call opa-plugin-builder with -o the name and then the files
# note that with option -I you can link with existing libraries
# see bslregister --help

# bslregister is a binary compiled and installed in libqml.git
plugin :
	opa-plugin-builder $(BSLOPT) -o sample myfile.ml myfile.js

# to clean everything if you need to
clean :
	rm -rf sample.opp
	rm -rf _build _tracks
	rm -rf *.exe

# after that, you must add the opp directory in the command line with opa :
opa :
	opa sample.opp with_plugin.opa --
