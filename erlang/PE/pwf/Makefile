.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

# pwf = programming with file

MAIN_MOD = id3_v1

MODS =  ${MAIN_MOD}

ERL = erl -boot start_clean 

LIB_PATH = ../lib/

all:	compile


compile: ${MODS:%=%.beam} lib
	@echo "make clean - clean up"

test: compile
	erl -pa ${LIB_PATH} -s  ${MAIN_MOD} test


lib:
	cd ${LIB_PATH};make

clean:	
	rm -rf *.beam erl_crash.dump
