.PHONY: compile shell clean release run

compile:
	./rebar3 compile

shell:
	./rebar3 shell

clean:
	./rebar3 clean

release:
	./rebar3 release

run:
	./start.sh

test:
	./rebar3 eunit

dialyzer:
	./rebar3 dialyzer

docs:
	./rebar3 edoc

all: compile test

help:
	@echo "Erlang Agent Makefile"
	@echo "---------------------"
	@echo "make compile  - Compile the project"
	@echo "make shell    - Start an Erlang shell with the project loaded"
	@echo "make clean    - Clean build artifacts"
	@echo "make release  - Build a release"
	@echo "make run      - Run the application"
	@echo "make test     - Run the tests"
	@echo "make dialyzer - Run dialyzer"
	@echo "make docs     - Generate documentation"
	@echo "make all      - Compile and test"
	@echo "make help     - Show this help message"