
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src
EFLAGS += -I /usr/lib/erlang/lib

ifeq ($(MAKECMDGOALS),test)
	EFLAGS += -Ddebug -DTEST +debug_info
endif
export CC BEAMDIR EFLAGS

all: src

src:
	@- test ! -d ebin && mkdir ebin
	@$(MAKE) -C src/

test: all
	@$(MAKE) -C test/

runtest:
	for i in `find ebin -iname "*_tests.beam"`; do \
		j=$$(basename $$i); j=$${j%_tests.beam}; \
		erl +v -pa ebin/ -I src -eval "eunit:test($$j,[verbose])." -s erlang halt; \
	done

clean:
	rm -f $(BEAMDIR)/*.beam

.PHONY: src 

