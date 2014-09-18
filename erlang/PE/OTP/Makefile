.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

# OTP = Open Telecom Platform

MAIN_MOD = server

MODS =  ${MAIN_MOD} \
		server1 \
		name_server \
		name_server_swp \
		my_bank 

ERL = erl -boot start_clean
ERL_FLAG = -noshell
ERL_CLEAN = -s init stop
LIB_PATH = ../lib/

all:	compile


compile: ${MODS:%=%.beam} lib
	@echo "make clean - clean up"

test: compile
	${ERL} ${ERL_FLAG} -pa ${LIB_PATH} -s ${MAIN_MOD} start name_server name_server \
		-s name_server add alucard "at office" \
		-s name_server add joe "at home" \
		-s name_server whereis alucard \
		-s ${MAIN_MOD} swap_code name_server name_server_swp \
		-s name_server_swp all_names \
		${ERL_CLEAN}

bank: compile
	${ERL} ${ERL_FLAG} -pa ${LIB_PATH} -s my_bank start \
		-s my_bank new_account alucard \
		-s my_bank deposit alucard 10000 \
		-s my_bank with_draw alucard 5000 \
		${ERL_CLEAN}

server: compile
	erl -pa ${LIB_PATH} -s server1 start name_server name_server 

client: compile
	erl -pa ${LIB_PATH} -s socket_client test

lib:
	cd ${LIB_PATH};make

clean:	
	rm -rf *.beam erl_crash.dump
