.PHONY: doc
REBAR = ./rebar3

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) as doc edoc

elixir:
	@$(REBAR) elixir generate_mix
	@$(REBAR) elixir generate_lib

dist: compile elixir doc

distclean:
	@rm -rf _build rebar.lock mix.lock test/eunit

