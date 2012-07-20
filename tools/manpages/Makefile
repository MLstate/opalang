.PHONY: all clean help view

# --
# build the list of target binaries to be documented:
#   bin/* and lib/opa/bin/* together with inclusions and exclusions
# --
BLDDIR ?= ../../_build
OCAMLBUILD ?= bld

BINDIR=$(BLDDIR)/bin
EXCLUDE=opa-bin mlstate_platform genproto ocamlbase.top ppdebug-opa ppjs
INCLUDE=opa
BINS=$(INCLUDE) $(filter-out $(EXCLUDE), $(notdir $(wildcard $(BLDDIR)/bin/* $(BLDDIR)/lib/opa/bin/*)))

TARGETDIR=$(BLDDIR)/man/man1
TARGETS=$(addprefix $(TARGETDIR)/, $(addsuffix .1, $(BINS)))

all: $(TARGETS)

# for debugging
help: $(TARGETS:.1=.help)

clean:
	rm -f $(TARGETS) $(TARGETS:.1=.help)

view: $(TARGETS)
	man $(TARGETS)

# --
# first check if a manpage has been written by hand
# --
$(TARGETDIR)/%.1: %.man
	@mkdir -p $(TARGETDIR)
	cp $< $@
	gzip -f $@

# --
# second run specific manpage generation programs
# --
$(BLDDIR)/compiler/opa/gen_opa_manpage.native: ../../compiler/opa/gen_opa_manpage.ml
	cd ../..; $(OCAMLBUILD) compiler/opa/gen_opa_manpage.native

$(TARGETDIR)/opa.1: $(BLDDIR)/compiler/opa/gen_opa_manpage.native
	@mkdir -p $(TARGETDIR)
	$< > $@
	gzip -f $@

$(BLDDIR)/compiler/opatop/gen_opatop_manpage.native: ../../compiler/opatop/gen_opatop_manpage.ml
	cd ../..; $(OCAMLBUILD) compiler/opatop/gen_opatop_manpage.native

$(TARGETDIR)/opatop.1: $(BLDDIR)/compiler/opatop/gen_opatop_manpage.native
	@mkdir -p $(TARGETDIR)
	$< > $@
	gzip -f $@

# --
# third run our hackish conversion tool help->manpage
# --
GENMAN=$(BLDDIR)/tools/manpages/genman.native

$(GENMAN): genman.ml
	cd ../..; $(OCAMLBUILD) -cflags "-w -14" tools/manpages/genman.native

$(TARGETDIR)/%.help: $(BLDDIR)/bin/%
	@mkdir -p $(TARGETDIR)
	$< --help >$@ 2>&1 ||true

$(TARGETDIR)/%.help: $(BLDDIR)/lib/opa/bin/%
	@mkdir -p $(TARGETDIR)
	$< --help >$@ 2>&1 ||true

$(TARGETDIR)/%.1: $(TARGETDIR)/%.help $(GENMAN)
	@mkdir -p $(TARGETDIR)
	$(GENMAN) $* $(TARGETDIR)/$* > $@
	gzip -f $@
