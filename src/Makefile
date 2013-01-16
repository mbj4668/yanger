all:
	@echo "Don't do make in this directory"
	exit 1

compile: yang_llerror.erl

clean:
	rm -f yang_llerror.erl

yang_llerror.erl: ../c_src/yang_error.h
	awk -f mk_llerror.awk < $< > $@
