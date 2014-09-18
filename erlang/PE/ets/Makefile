.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

# ets = Erlang term storage

MAIN_MOD = ets_test

MODS =  ${MAIN_MOD} 

ERL = erl -boot start_clean 

LIB_PATH = ../lib/

all:	compile

compile: ${MODS:%=%.beam} lib
	@echo "make clean - clean up"

test: compile
	erl -noshell -pa ${LIB_PATH} -s ${MAIN_MOD} test -s init stop

lib:
	cd ${LIB_PATH};make

clean:	
	rm -rf *.beam erl_crash.dump
