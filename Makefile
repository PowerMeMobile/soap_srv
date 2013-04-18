NAME=soap_srv
BIN=./rel/${NAME}/bin/${NAME}

all: generate

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

generate: compile
	rm -rf ./rel/${NAME}
	./rebar generate

clean:
	./rebar clean

console:
	${BIN} console