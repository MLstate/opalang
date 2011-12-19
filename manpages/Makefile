.PHONY: all clean help

# --
# build the list of target binaries to be documented:
#   bin/* and lib/opa/bin/* together with inclusions and exclusions
# --
BLDDIR=../_build
BINDIR=$(BLDDIR)/bin
EXCLUDE=opa-bin mlstate_platform genproto ocamlbase.top ppdebug-opa ppjs
INCLUDE=opa
BINS=$(INCLUDE) $(filter-out $(EXCLUDE), $(notdir $(wildcard $(BLDDIR)/bin/* $(BLDDIR)/lib/opa/bin/*)))

TARGETS=$(BINS:=.1)

all: $(TARGETS)

# for debugging
help: $(TARGETS:.1=.help)

clean:
	rm -f $(TARGETS) $(TARGETS:.1=.help)

# --
# first check if a manpage has been written by hand
# --
%.1: %.man
	cp $< $@

# --
# second run specific manpage generation programs
# --
$(BLDDIR)/opa/gen_opa_manpage.native: ../opa/gen_opa_manpage.ml
	cd ..; bld opa/gen_opa_manpage.native

opa.1: $(BLDDIR)/opa/gen_opa_manpage.native
	$< > $@

$(BLDDIR)/opatop/gen_opatop_manpage.native: ../opatop/gen_opatop_manpage.ml
	cd ..; bld opatop/gen_opatop_manpage.native

opatop.1: $(BLDDIR)/opatop/gen_opatop_manpage.native
	$< > $@

# --
# third run our hackish conversion tool help->manpage
# --
GENMAN=$(BLDDIR)/manpages/genman.native

$(GENMAN): genman.ml
	cd ..; bld -cflags "-w -14" manpages/genman.native

%.help: $(BLDDIR)/bin/%
	$< --help >$@ 2>&1 ||true

%.help: $(BLDDIR)/lib/opa/bin/%
	$< --help >$@ 2>&1 ||true

%.1: %.help $(GENMAN)
	$(GENMAN) $* > $@
