###
### Tests of yanger internals, e.g., callbacks.
###

include ../../support/*_testcases.mk

.PHONY: build

build: mksn.beam

mksn.beam: mksn.erl
	erlc -pa ../../../../yanger/ebin $<

clean:
	rm -f mksn.beam
	rm -fr lux_logs
