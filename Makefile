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

.PHONY: all compile deps rel clean
