BLDDIR=../_build

#MANDIR=$(BDLDIR)/manpages

.PHONY: all

all: opa.1 opatop.1 opa-db-server.1 opa-db-tool.1 opa-plugin-browser.1 opa-plugin-builder.1 opadoc.1

opa.1: $(BLDDIR)/opa/gen_opa_manpage.native
	$< > $@

opatop.1: $(BLDDIR)/opatop/gen_opatop_manpage.native
	$< > $@

%.1: $(BLDDIR)/bin/%
	@echo ".TH \"$*\" \"1\" \"`date +"%Y-%m-%d"`\" \"\" \"Opa Manual\"" > $@
	@echo ".SH \"HELP\""  >> $@
	$< --help 2>> $@ ||true