
PARSER_NIF = ../priv/yang_parser_nif.so
REGEX_NIF = ../priv/w3cregex_nif.so

PRIV_FILES = $(PARSER_NIF) $(REGEX_NIF)

CFLAGS = -std=c99
CWARNINGS = \
       -Werror \
       -Wall   \
       -Wsign-compare \
       -Wstrict-prototypes \
       -Wpointer-arith \
       -Wcast-align \
       -Wwrite-strings \
       -Wnested-externs

CFLAGS += -fpic -DDYNAMIC_DRIVER

ifneq ($(DEBUG),)
  CFLAGS += -g -ggdb
  CWARNINGS += -Wuninitialized
  ERLC_FLAGS += +debug_info
else
  CFLAGS += -O2
endif

CFLAGS  += $(CWARNINGS)

ERL_EVAL    = 'io:format("~s", [os:getenv("ROOTDIR")]), erlang:halt(0)'
ERL_ROOTDIR = $(shell erl -noinput -eval $(ERL_EVAL))
ifneq ($(findstring /, $(ERL_ROOTDIR)),)
  CFLAGS += -I$(ERL_ROOTDIR)/usr/include
endif

XML_CFLAGS = $(shell pkg-config --cflags libxml-2.0)
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

ifeq ($(shell uname -s), Darwin)
  LDDRV_FLAGS = -bundle -undefined dynamic_lookup
else
  LDDRV_FLAGS = -shared
endif


C_SOURCES := $(wildcard yang*.c)
C_HEADERS := $(wildcard yang*.h)
OBJS := $(C_SOURCES:%.c=./%.o)


all: $(PRIV_FILES) TAGS

TAGS: $(C_SOURCES) $(C_HEADERS)
	-etags $^

$(OBJS): $(C_HEADERS)

$(PARSER_NIF): $(OBJS)
	$(LD) $(LDDRV_FLAGS) -o $@ $^ $(LIBS)

$(REGEX_NIF): w3cregex_nif.o
	$(LD) $(LDDRV_FLAGS) -o $@ $^ $(LIBS)

debug:
	$(MAKE) DEBUG=true all

# Keep .o file around (handy when gdb debugging...)
.PRECIOUS: $(OBJS)

clean:
	rm -f ../priv/*.so ./*.so ./*.o *.o TAGS
