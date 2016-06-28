.PHONY: doc
include bu.mk

compile:
	$(verbose) $(REBAR) compile

doc:
	$(verbose) $(REBAR) as doc edoc

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

dist: compile elixir doc

distclean:
	$(verbose) $(RM_RF) _build rebar.lock mix.lock test/eunit

