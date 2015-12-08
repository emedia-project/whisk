PROJECT = gitignore

DEPS = eutils
dep_eutils = git https://github.com/emedia-project/eutils.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

