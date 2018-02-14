# Targets for building and running tests within a testarea

# Default values for make script
SUMMARY_LOG=lux_summary.log.html
SUT_RUN_TARGETS=default
LUX=lux
LOG_DIR=lux/

ifeq ($(LUX_FILES),)
   LUX_FILES=.
endif

# Finds which test cases should be run
ifeq ($(LUXSUBDIRS),)
    FILES = $(shell $(LUX) --mode list $(LUX_FILES))
    ifneq ($(FILES),)
        LUXSUBDIRS=$(shell for d in $(FILES); do echo `dirname $$d`; done|sort -u)
    else
        LUXSUBDIRS=empty
    endif
endif

# Builds a testarea
build_testarea:
	@if [ "$(SUT_RUN_TARGETS)" = "default" ]; then                  \
           if [ "$(LUXSUBDIRS)" != "empty" ]; then                      \
	      for d in $(LUXSUBDIRS); do                                \
                     echo "cd $$d" ;                                    \
	             (cd $$d && $(MAKE) build EXECTARGET="no") || exit 1; \
	      done ;                                                    \
           fi;                                                          \
         fi;

.PHONY: build_testarea

# Cleans a test area note that it will only clean SUT_RUN_TARGETS=default
clean_testarea:
	@rm -fr lux_logs;                                             \
        if [ "$(LUXSUBDIRS)" != "empty" ]; then                       \
	   for d in $(LUXSUBDIRS); do                                 \
	     (                                                        \
	       (cd $$d && $(MAKE) clean EXECTARGET="no" ) || exit 1   \
	     );                                                       \
	   done;                                                      \
        fi;
.PHONY: clean_testarea

# Runs a testarea
test_testarea:
	for type in $(SUT_RUN_TARGETS); do  \
		if [ "$$type" = "default" ]; then \
                   RUN_TYPE_SUFFIX=""; \
		   echo "Default no need to use SUT_RUN_TARGETS"; \
		else \
                   RUN_TYPE_SUFFIX="_$$type"; \
		   if [ "$(LUXSUBDIRS)" != "" ]; then \
		      $(MAKE) $$type; \
		   else \
		      echo "No need to build SUT_RUN_TARGET= $$type SKIPPED"; \
		   fi ; \
		fi ; \
		LUXARGS="$(LUX_ARGS)" ; \
		($(LUX) $$LUXARGS $(LUX_FILES)) || failed="true"; \
	done; \
	if [ "$$failed" = "true" ]; then exit 1; fi


.PHONY: test_testarea

lux_desc:
	@env $(LUX) --config_name Docspec --mode=doc --skip_skip $(LUX_FILES)
.PHONY: lux_desc
