REBAR = ./rebar

all: deps compile

clean:
	$(RM) -rf deps
	$(REBAR) clean

compile:
	$(REBAR) compile

deps:
	$(REBAR) update-deps

fast:
	$(REBAR) compile skip_deps=true

test: compile eunit

eunit:
	ERL_LIBS=deps $(REBAR) skip_deps=true $(if $(EUNIT_SUITES),suites=$(EUNIT_SUITES)) $(if $(EUNIT_TESTS),tests=$(EUNIT_TESTS)) eunit

.PHONY: all deps fast clean compile eunit test
