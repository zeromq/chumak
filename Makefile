.PHONY: all setup test doc

all: setup test

setup:
	./rebar3 compile

test:
	./rebar3 eunit -c

doc:
	./rebar3 edoc

cover:
	./rebar3 cover

shell:
	./rebar3 shell
