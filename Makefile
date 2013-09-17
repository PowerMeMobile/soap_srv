NAME=soap_srv
BIN=./rel/${NAME}/bin/${NAME}

all: generate

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

compile: get-deps compile-src
	@./rebar skip_deps=true xref

compile-src:
	@./rebar compile

generate: compile
	@rm -rf ./rel/${NAME}
	@./rebar generate

clean:
	@./rebar clean

console:
	@${BIN} console

develop:
	@./rel/$(NAME)/bin/$(NAME) develop

run-tests:
	@./rebar skip_deps=true eunit

tags:
	@find . -name "*.[e,h]rl" -print | etags -
