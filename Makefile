REBAR3=rebar3

.PHONY: all compile test dialyze clean

all:
	$(REBAR3) do compile, dialyzer, eunit

compile:
	$(REBAR3) compile

test:
	$(REBAR3) eunit

clean:
	$(REBAR3) clean
	rm -f TEST-*.xml priv/btree*.so rebar.lock
	rm -rf _build
