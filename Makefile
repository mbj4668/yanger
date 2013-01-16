
.PHONY: all compile clean eunit test doc

all: compile

compile:
	(cd src && $(MAKE) compile) || exit 1;
	rebar compile

eunit: compile
	rebar eunit

test: eunit

clean:
	rebar clean
	(cd src && $(MAKE) clean)

doc:
	rebar doc