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
	cd rel/$(NAME) && erl -eval '[ReleaseDir] = [D || D <- string:tokens(os:cmd("ls -1 releases/"), "\n"), D =/= "RELEASES", D =/= "start_erl.data"], ok = release_handler:create_RELEASES(".", "releases", "releases/"++ ReleaseDir ++"/soap_srv.rel", []).' -s init stop -noshell && cd -

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
