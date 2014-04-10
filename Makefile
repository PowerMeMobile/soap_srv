NAME=soap_srv
BIN=./rel/${NAME}/bin/${NAME}

all: generate

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

xref: compile
	@./rebar xref skip_deps=true

compile: get-deps
	@./rebar compile

generate: compile xref
	@rm -rf ./rel/${NAME}
	@./rebar generate

clean:
	@./rebar clean

console:
	@${BIN} console

develop:
	@./rel/$(NAME)/bin/$(NAME) develop

run-tests:
	@./rebar eunit skip_deps=true

api-test:
	@./test/test.escript

tags:
	@find . -name "*.[e,h]rl" -print | etags -
