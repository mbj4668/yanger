PARSER_NIF = ../priv/yang_parser_nif.so
REGEX_NIF = ../priv/w3cregex_nif.so

PRIV_FILES = $(PARSER_NIF) $(REGEX_NIF)

all: $(PRIV_FILES)

include c_src.mk

c_src.mk:
	$(MAKE) -f ../erl.mk $@

CFLAGS += -std=c99
CWARNINGS = \
       -Werror \
       -Wall   \
       -Wsign-compare \
       -Wstrict-prototypes \
       -Wpointer-arith \
       -Wcast-align \
       -Wwrite-strings \
       -Wnested-externs

ifneq ($(DEBUG),)
  CFLAGS += -g -ggdb
  CWARNINGS += -Wuninitialized
  ERLC_FLAGS += +debug_info
else
  CFLAGS += -O2
endif

CFLAGS  += $(CWARNINGS)

XML_CFLAGS ?= $(shell pkg-config --cflags libxml-2.0)
ifeq ($(XML_CFLAGS),)
  XML_CFLAGS = -I/usr/include/libxml2
endif
CFLAGS += $(XML_CFLAGS)
XML_LIBS = $(shell pkg-config --libs libxml-2.0)
ifeq ($(XML_LIBS),)
  XML_LIBS = -lxml2
endif
LIBS += $(XML_LIBS)

LD = $(CC)

PARSER_SOURCES := $(wildcard yang*.c)
PARSER_HEADERS := $(wildcard yang*.h)
PARSER_OBJS := $(PARSER_SOURCES:%.c=./%.o)

$(PARSER_NIF): $(PARSER_OBJS)
	$(LD) $(LDFLAGS_NIF) -o $@ $^ $(LIBS)

$(REGEX_NIF): w3cregex_nif.o
	$(LD) $(LDFLAGS_NIF) -o $@ $^ $(LIBS)

debug:
	$(MAKE) DEBUG=true all

# Keep .o file around (handy when gdb debugging...)
.PRECIOUS: $(OBJS)

clean:
	rm -f ../priv/*.so ./*.so ./*.o *.o
