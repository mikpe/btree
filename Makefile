suite=$(if $(SUITE), suite=$(SUITE), )

DIALYZER_OPTIONS := --fullpath --no_native -Wunderspecs
DIALYZER_PLT     := ./dialyzer.plt

.PHONY: default all compile deps test dialyze docs clean

default: compile

all: compile deps test dialyze docs

compile:
	./rebar compile

deps:
	./rebar get-deps

test:
	./rebar eunit $(suite) skip_deps=true

dialyze: build-plt
	./rebar dialyze

build-plt:
	./rebar build-plt

docs:
	./rebar doc

conf_clean:
	@:

clean:
	./rebar clean
	$(RM) doc/*.html doc/*edoc-info doc/erlang.png doc/stylesheet.css
