
ERL=erl
ERLC=erlc

EXTRA_EUNIT_BUILD ?=
EXTRA_ERL_FLAGS ?= -pa ../../ebin
EXTRA_ERLC_FLAGS ?=

TEST_SRC ?= $(wildcard ./*.erl)
TEST_SUITES ?= $(wildcard ./*_tests.erl)
TEST_MODULES2 ?= $(subst ./,,$(subst .erl,,$(TEST_SUITES)))
TEST_MODULES ?= $(shell echo $(TEST_MODULES2) |  sed -e 's/ /, /g')
TEST_BEAM ?= $(subst .erl,.beam,$(subst ./,ebin/test/,$(TEST_SRC)))

SRC_DIRS=$(shell pwd)

# Use absolute path for yanger/ebin using pwd to ensure 
# that code:priv_dir resolves correct.
TEST_EXTRA_ERL_FLAGS=-pa $(shell cd ../../ebin && pwd) $(EXTRA_ERL_FLAGS) \
                        -pa ebin/test -pa ebin/app

test: build $(APP_BEAM) $(TEST_BEAM)
	@$(ERL) $(TEST_EXTRA_ERL_FLAGS) -noinput \
                -eval "eunit:test([$(TEST_MODULES)], [verbose])" \
                -s init stop

test-%: build $(APP_BEAM) $(TEST_BEAM)
	env ERL="$(ERL)" EXTRA_ERL_FLAGS="$(TEST_EXTRA_ERL_FLAGS)" \
                ../support/run_eunit.sh test-single $(subst test-,,$@)

# Mandatory targets
build:  ebin/test ebin/app $(EXTRA_EUNIT_BUILD) $(TEST_BEAM)
	@echo "Build complete"

clean:
	-rm -rf $(BEAM) test-reports ebin

######################################################################
# Internal targets

ebin/test:
	mkdir -p ebin/test

ebin/app:
	mkdir -p ebin/app

ebin/test/%.beam: %.erl
	$(ERLC) -DTEST -I ../../../ -I ../../include $(EXTRA_ERLC_FLAGS) \
                +debug_info -o ebin/test $<

ebin/app/%.beam: ../../src/%.erl
	$(ERLC) -DTEST -I ../../../ -I ../../include $(EXTRA_ERLC_FLAGS) \
                +export_all +debug_info -o ebin/app $<
