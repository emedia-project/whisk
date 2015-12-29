PROJECT = whisk

DEPS = eutils
dep_eutils = git https://github.com/emedia-project/eutils.git master

DOC_DEPS = edown
dep_edown = git https://github.com/uwiger/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/emedia-project/${PROJECT}"}}

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

