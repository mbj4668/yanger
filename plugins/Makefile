include ../vsn.mk

ERL_SOURCES  := $(wildcard *.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=../priv/%.beam)

ERLC_FLAGS += -pa ../../yanger/ebin -Werror

ERLC ?= erlc

all: $(ERL_OBJECTS)

debug:
	$(MAKE) TYPE=debug

../priv/%.beam: %.erl ../include/yang.hrl
	$(ERLC) $(ERLC_FLAGS) -o ../priv $<

clean:
	rm -f ../priv/*.beam .*.erl.d

ERL_DEPS = $(ERL_SOURCES:%.erl=.%.erl.d)

.%.erl.d: %.erl
	$(ERLC) -M -MG -I ../.. -o ../priv -pa ../ebin $< > $@

.PHONY: depend

# forcefully re-generate dependency files
depend:
	rm -f .*.erl.d
	$(MAKE) $(ERL_DEPS)

ifneq ($(MAKECMDGOALS),clean)
-include $(ERL_DEPS)
endif
