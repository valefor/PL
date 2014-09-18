.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

# pws = programming with socket

MAIN_MOD = socket_examples

MODS =  ${MAIN_MOD} \
		socket_server \
		socket_client

ERL = erl -boot start_clean 

LIB_PATH = ../lib/

all:	compile


compile: ${MODS:%=%.beam} lib
	@echo "make clean - clean up"

test: compile
	erl -pa ${LIB_PATH} -s  ${MAIN_MOD} test

server: compile
	erl -pa ${LIB_PATH} -s socket_server start_nano_server

client: compile
	erl -pa ${LIB_PATH} -s socket_client test

lib:
	cd ${LIB_PATH};make

clean:	
	rm -rf *.beam erl_crash.dump
