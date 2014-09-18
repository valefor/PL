.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

# mnesia = erlang DB

MAIN_MOD = test_mnesia

MODS =  ${MAIN_MOD}\
		qlc_test \
		tt

ERL = erl -boot start_clean 
ERL_FLAG = -noshell
ERL_CLEAN = -s init stop
LIB_PATH = ../lib/

all:	compile


compile: ${MODS:%=%.beam} lib
	@echo "make clean - clean up"

test: compile
	${ERL} ${ERL_FLAG} -pa ${LIB_PATH} -s ${MAIN_MOD} start \
		-s ${MAIN_MOD} reset_tables \
		-s ${MAIN_MOD} demo select_stop

qlc: compile
	${ERL} ${ERL_FLAG} -pa ${LIB_PATH} -s qlc_test q

lib:
	cd ${LIB_PATH};make

clean:	
	rm -rf *.beam erl_crash.dump
