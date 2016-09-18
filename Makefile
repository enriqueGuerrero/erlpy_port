#!/bin/bash
REBAR=./rebar3
DIR=$(pwd)
DIALYZER=dialyzer



.PHONY: deps
compile: deps
	rm -rf _build
	$(REBAR) release

clean:
	@rm -rf _build

console:
	 _build/default/rel/erlpy_port/bin/erlpy_port console 

start:
	_build/default/rel/erlpy_port/bin/erlpy_port start
