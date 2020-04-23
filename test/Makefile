DIRS = lux

all: test

build:
	for d in $(DIRS) ; do \
		(cd $$d && $(MAKE) build) || exit 1; \
	done

test:
	@error="" ; \
	for d in $(DIRS);  do \
		( cd $$d && env MAKE=$(MAKE) $(MAKE) test;  \
	         ) || error="$$error $$d"; \
	done ; \
	if [ "$$error" ]; then \
		echo '***********************************' ; \
		echo '*** Failed test(s) (see above): ***' ; \
		echo '     ' $$error ; \
		echo '***********************************' ; \
		exit 1 ; \
	fi;

clean:
	for d in $(DIRS) ; do ( cd $$d &&  $(MAKE) clean) ; done
desc:
	@echo "Overview:"
	@echo "yanger grey box testing"
	@$(MAKE) -C lux lux_desc --no-print-directory

.PHONY: all build test clean desc
