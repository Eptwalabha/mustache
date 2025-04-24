#!/usr/bin/make

MUSTACHE_SPECS_VERSION = 1.4.2
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
fetch-mustache-specs:
	mkdir -p test/specs
	curl -L https://github.com/mustache/spec/archive/refs/tags/v$(MUSTACHE_SPECS_VERSION).tar.gz -o /tmp/mustache-specs-$(MUSTACHE_SPECS_VERSION).tar.gz
	tar -xzf /tmp/mustache-specs-$(MUSTACHE_SPECS_VERSION).tar.gz -C test/specs --wildcards 'spec-$(MUSTACHE_SPECS_VERSION)/specs/*.json' --strip-components 2
	rm /tmp/mustache-specs-$(MUSTACHE_SPECS_VERSION).tar.gz
