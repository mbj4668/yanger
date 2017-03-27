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
	rm -f ../priv/*.beam
