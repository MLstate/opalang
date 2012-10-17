.PHONY: all clean help view

# --
# build the list of target binaries to be documented:
#   bin/* and lib/opa/bin/* together with inclusions and exclusions
# --
BUILD_DIR ?= _build
OCAMLBUILD ?= bld

BINDIR=$(BUILD_DIR)/bin
EXCLUDE=opa-bin mlstate_platform genproto ocamlbase.top ppdebug-opa ppjs
INCLUDE=opa
BINS=$(INCLUDE) $(filter-out $(EXCLUDE), $(notdir $(wildcard $(BUILD_DIR)/bin/* $(BUILD_DIR)/lib/opa/bin/*)))

TARGETDIR=$(BUILD_DIR)/man/man1
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
	gzip -f --best $@

# --
# second run specific manpage generation programs
# --
$(BUILD_DIR)/$(OPALANG_DIR)/compiler/opa/gen_opa_manpage.native: $(OPALANG_DIR)/compiler/opa/gen_opa_manpage.ml
	$(OCAMLBUILD) $(OPALANG_DIR)/compiler/opa/gen_opa_manpage.native

$(TARGETDIR)/opa.1: $(BUILD_DIR)/$(OPALANG_DIR)/compiler/opa/gen_opa_manpage.native
	@mkdir -p $(TARGETDIR)
	$< > $@
	gzip -f --best $@

$(BUILD_DIR)/$(OPALANG_DIR)/compiler/opatop/gen_opatop_manpage.native: $(OPALANG_DIR)/compiler/opatop/gen_opatop_manpage.ml
	$(OCAMLBUILD) $(OPALANG_DIR)/compiler/opatop/gen_opatop_manpage.native

$(TARGETDIR)/opatop.1: $(BUILD_DIR)/$(OPALANG_DIR)/compiler/opatop/gen_opatop_manpage.native
	@mkdir -p $(TARGETDIR)
	$< > $@
	gzip -f --best $@

# --
# third run our hackish conversion tool help->manpage
# --
GENMAN=$(BUILD_DIR)/$(OPALANG_DIR)/tools/manpages/genman.native

$(GENMAN): $(OPALANG_DIR)/tools/manpages/genman.ml
	$(OCAMLBUILD) -cflags "-w -14" $(OPALANG_DIR)/tools/manpages/genman.native

$(TARGETDIR)/%.help: $(BUILD_DIR)/bin/%
	@mkdir -p $(TARGETDIR)
	$< --help >$@ 2>&1 ||true

$(TARGETDIR)/%.help: $(BUILD_DIR)/lib/opa/bin/%
	@mkdir -p $(TARGETDIR)
	$< --help >$@ 2>&1 ||true

$(TARGETDIR)/%.1: $(TARGETDIR)/%.help $(GENMAN)
	@mkdir -p $(TARGETDIR)
	$(GENMAN) $* $(TARGETDIR)/$* > $@
	gzip -f --best $@
