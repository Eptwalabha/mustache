#!/usr/bin/make

.PHONY : all deps compile dialyzer lint test cover clean
all: deps compile dialyzer lint cover
deps:
	rebar3 get-deps
compile:
	rebar3 compile
	rebar3 escriptize
dialyzer:
	rebar3 dialyzer
lint:
	rebar3 lint
test:
	rebar3 eunit
cover:
	rebar3 eunit --cover
	rebar3 cover -m 100
clean:
	rebar3 clean
