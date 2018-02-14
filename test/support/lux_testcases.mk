#Targets for building and running tests within a testarea

#Default values for make script
SUMMARYLOG=lux.summary.log.html
SUT_RUN_TARGET=default
LUX=lux
LUX_FILES?=.
LUX_FLAGS_DEFAULT="--progress=verbose --debug"

LUX_FLAGSs = $(shell echo $$LUX_FLAGS)

#Set the PATH varible
PATH := $(TEST_DIR)/bin:$(PATH)

all: build test

. PHONY: all

#Runs a testcase
test:	dotest
.PHONY: test

dotest:
	if [ "$(LUX_FLAGSs)" != "" ]; then \
	   LUX_FLAGSx=$(LUX_FLAGSs) ; \
	fi ; env "LUX_FLAGS=$$LUX_FLAGSx" $(LUX) $(LUX_FILES)
.PHONY: dotest
