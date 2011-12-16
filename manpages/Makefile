BLDDIR=../_build

BINDIR=$(BLDDIR)/bin

.PHONY: all clean

EXCLUDE=opa-bin mlstate_platform
INCLUDE=opa
BINS=$(INCLUDE) $(filter-out $(EXCLUDE), $(notdir $(wildcard $(BLDDIR)/bin/* $(BLDDIR)/lib/opa/bin/*)))

TARGETS=$(BINS:=.1)

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

GENMAN=$(BLDDIR)/manpages/genman.native

$(GENMAN): genman.ml
	cd ..; bld -cflags "-w -14" manpages/genman.native

$(BLDDIR)/opa/gen_opa_manpage.native: ../opa/gen_opa_manpage.ml
	cd ..; bld -cflags manpages/opa/gen_opa_manpage.native

opa.1: $(BLDDIR)/opa/gen_opa_manpage.native
	$< > $@

#opatop.1: $(BLDDIR)/opatop/gen_opatop_manpage.native
#	$< > $@

%.help: $(BLDDIR)/bin/%
	$< --help >$@ 2>&1 ||true

%.help: $(BLDDIR)/lib/opa/bin/%
	$< --help >$@ 2>&1 ||true

%.1: %.help $(GENMAN)
	$(GENMAN) $* > $@
