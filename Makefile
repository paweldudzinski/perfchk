PROJECT := perfchk

ERL := erl
EPATH = -pa _build/default/lib/jiffy/ebin -pa _build/default/lib/perfchk/ebin -pa _build/default/lib/cast/ebin

.PHONY: all compile configure run deps test shell

all: deps compile

compile:
	@./rebar3 compile

configure:
	@./rebar3 get-deps compile

run:
	@$(ERL) $(EPATH) -s $(PROJECT) -noshell

deps:
	@./rebar3 get-deps update-deps

test:
	@./rebar3 eunit

shell:
	@./rebar3 shell
