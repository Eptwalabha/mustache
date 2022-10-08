#!/usr/bin/make

.PHONY : all deps compile cover clean
all: deps compile cover
deps:
	@rebar3 get-deps
compile:
	@rebar3 compile
	@rebar3 escriptize
test:
	-@rebar3 eunit
cover:
	-@rebar3 eunit --cover
	-@rebar3 cover -m 100
clean:
	@rebar3 clean
