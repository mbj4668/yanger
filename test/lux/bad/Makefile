#Includes for test and build
include ../../support/*_testcases.mk

.PHONY: build

YANG = $(wildcard *.yang)
GENLUX = $(YANG:%.yang=%.lux)

build: $(GENLUX)

%.lux: %.yang
	sh gen_lux.sh $< > $@

clean:
	rm -f $(GENLUX)
	rm -fr lux_logs
