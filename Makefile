REBAR = ./rebar

all: compile

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

rel: compile
	$(REBAR) generate

clean:
	$(REBAR) clean

doc:
	$(REBAR) skip_deps=true doc

test:
	$(REBAR) skip_deps=true eunit

.PHONY: all compile deps rel clean doc test
