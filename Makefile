DESCRIPTION = Erlang YANG compiler

SUBDIRS = plugins

ERL_MODULES = yang_llerror xpath_parse

include erl.mk

erl.mk:
	curl -s -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@

ERLC_OPTS := $(filter-out +warn_export_vars,$(ERLC_OPTS))

include vsn.mk
ERLC_OPTS += -DVSN=\"$(VSN)\"
DIALYZER_OPTS = -DVSN=\"$(VSN)\"

src/yang_llerror.erl: c_src/yang_error.h
	awk -f src/mk_llerror.awk < $< > $@

src/yang_llerror.hrl: c_src/yang_error.h
	awk -f src/mk_llerror_hrl.awk < $< > $@

ebin/yang.beam: src/yang_llerror.hrl

src/%.erl: src/%.yrl
	erlc -o src $<

clean: clean-gen

clean-gen:
	rm -rf src/yang_llerror.erl src/yang_llerror.hrl src/xpath_parse.erl
