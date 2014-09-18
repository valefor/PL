# leave this line alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

# Here's a list of the erlang modules you wants compiling
# If the modules don't fit onto the one line add a \ character
# to the end of the line and continue ont the next line

# Edit the lines below
MAIN_MODS = chat_client

MODS = ${MAIN_MODS} chat_group \
	   				chat_server \
	   				io_widget kvs \
	   				lib_chan lib_chan_mm lib_chan_cs lib_chan_auth \
	   				io_widget \
					mod_echo mod_math mod_name_server mod_chat_controller

LIB_PATH = ../lib/

# The first target in any makefile is the default target.
# If you just type "make" the "make all " is assumed(because
# "all" is the first target in this makefile)

all:compile

compile:${MODS:%=%.beam} lib

chat_client:compile
	erl -pa ${LIB_PATH} -s chat_client test

test:compile
	erl -s chat_tests start

chat_server:compile
	erl -pa ${LIB_PATH} -s chat_server start

server:compile
	erl -pa ${LIB_PATH} -boot start_clean -pa '.' -s lib_chan_test start_server

client:compile
	erl -pa ${LIB_PATH} -boot start_clean -pa '.' -s lib_chan_test start_client

run:compile
	${ERL} -s module start

lib:
	cd ${LIB_PATH};make

clean:
	rm -rf *.beam erl_crash.dump
