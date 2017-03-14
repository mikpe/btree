suite=$(if $(SUITE), suite=$(SUITE), )

.PHONY: all compile deps test check docs clean

all: compile deps test check docs

compile:
	./rebar compile

deps:
	./rebar get-deps

test:
	./rebar eunit $(suite) skip_deps=true

check:
	./rebar check-plt
	./rebar dialyze

docs:
	./rebar doc

conf_clean:
	@:

clean:
	./rebar clean
	$(RM) doc/*.html doc/*edoc-info doc/erlang.png doc/stylesheet.css
