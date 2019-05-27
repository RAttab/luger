REBAR = ./rebar3

all: deps compile

clean:
	$(RM) -rf deps
	$(REBAR) clean

compile:
	$(REBAR) compile

deps:
	$(REBAR) update

fast:
	$(REBAR) compile skip_deps=true

test: compile eunit

eunit:
	ERL_LIBS=deps $(REBAR) $(if $(EUNIT_SUITES),suites=$(EUNIT_SUITES)) $(if $(EUNIT_TESTS),tests=$(EUNIT_TESTS)) eunit

.PHONY: all deps fast clean compile eunit test
