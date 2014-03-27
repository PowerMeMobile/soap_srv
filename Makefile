NAME=soap_srv
BIN=./rel/${NAME}/bin/${NAME}

all: generate

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

xref:
	@./rebar xref skip_deps=true

compile: get-deps xref
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
	@./rebar eunit skip_deps=true

tags:
	@find . -name "*.[e,h]rl" -print | etags -
