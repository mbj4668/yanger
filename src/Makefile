include ../vsn.mk

ERL_SOURCES  := $(wildcard *.erl)
YECC_SOURCES := $(wildcard *.yrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=../ebin/%.beam) \
	       $(YECC_SOURCES:%.yrl=../ebin/%.beam)

ERL_OBJECTS += ../ebin/yang_llerror.beam

APP_SOURCES := $(wildcard *.app.src)
APP_OBJECTS := $(APP_SOURCES:%.app.src=../ebin/%.app)


ERLC ?= erlc

ERLC_FLAGS += -DVSN=\"$(VSN)\" -pa ../../yanger/ebin -Werror

compile: all

all: $(ERL_OBJECTS) $(APP_OBJECTS)

yang_llerror.erl: ../c_src/yang_error.h
	awk -f mk_llerror.awk < $< > $@

yang_llerror.hrl: ../c_src/yang_error.h
	awk -f mk_llerror_hrl.awk < $< > $@

../ebin/yang.beam: yang_llerror.hrl

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

%.erl: %.yrl
	$(ERLC) $<

clean:
	rm ../ebin/*.beam
	rm -f yang_llerror.erl yang_parse.erl

