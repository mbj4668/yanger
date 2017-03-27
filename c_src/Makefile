
PARSER_NIF = ../priv/$(XHOST)/yang_parser_nif.so
REGEX_NIF = ../priv/$(XHOST)/w3cregex_nif.so

PRIV_FILES = $(PARSER_NIF) $(REGEX_NIF)

CC=gcc
LD=gcc

CFLAGS = -Wall -I${ERL_DIR}/usr/include \
	-I/usr/include/libxml2 \
	-std=c99
#	-pedantic-errors
CWARNINGS       = \
		-Werror \
		-Wall   \
		-Wsign-compare \
		-Wstrict-prototypes \
		-Wpointer-arith \
		-Wcast-align \
		-Wwrite-strings \
		-Wnested-externs

CFLAGS += -fpic -DDYNAMIC_DRIVER

LDDRV_FLAGS	= -shared

ifneq ($(DEBUG),)
CFLAGS += -g -ggdb
CWARNINGS += -Wuninitialized
ERLC_FLAGS += +debug_info
else
CFLAGS += -O2
endif

CFLAGS  += $(CWARNINGS)

C_SOURCES := $(wildcard yang*.c)
C_HEADERS := $(wildcard yang*.h)
OBJS := $(C_SOURCES:%.c=./$(XHOST)/%.o)

SHARED_LIBS += -lxml2

all: $(PRIV_FILES) TAGS

TAGS: $(C_SOURCES) $(C_HEADERS)
	-etags $^

$(OBJS): $(C_HEADERS)

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

../priv/%.so: %.o
	$(LD) $(LDDRV_FLAGS) -o $@ $< $(SHARED_LIBS)
../priv//%.so: %.o
	$(LD) $(LDDRV_FLAGS) -o $@ $< $(SHARED_LIBS)

$(PARSER_NIF): $(OBJS)
ifeq ($(XHOST),) # Native build
	$(LD) $(LDDRV_FLAGS) -o $@ $^ $(SHARED_LIBS)
else # Cross build
	mkdir -p ../priv/$(XHOST)
	$(XLD) $(XLDDRV_FLAGS) -o $@ $^ $(XSHARED_LIBS) $(SHARED_LIBS)
endif # Cross build

$(REGEX_NIF): w3cregex_nif.o
ifeq ($(XHOST),) # Native build
	$(LD) $(LDDRV_FLAGS) -o $@ $^ $(SHARED_LIBS)
else # Cross build
	mkdir -p ../priv/$(XHOST)
	$(XLD) $(XLDDRV_FLAGS) -o $@ $^ $(XSHARED_LIBS) $(SHARED_LIBS)
endif # Cross build

debug:
	$(MAKE) DEBUG=true all

# Keep .o file around (handy when gdb debugging...)
.PRECIOUS: $(OBJS)

clean:
	rm -f ../priv/$(XHOST)/*.so ./$(XHOST)/*.so ./$(XHOST)/*.o *.o TAGS
