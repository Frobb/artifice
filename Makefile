REBAR = ./rebar

all: compile

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

.PHONY: all compile clean
