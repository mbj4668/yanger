SUBDIRS = c_src src plugins

all:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( cd $$d && $(MAKE) ) || exit 1 ; \
	    fi ; \
	  done

clean: $(LOCALCLEAN)
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
	    fi ; \
	  done

include vsn.mk
dialyzer:
	dialyzer -DVSN=\"$(VSN)\" -pa ../yanger/ebin --src src/*.erl
