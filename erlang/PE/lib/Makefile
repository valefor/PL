.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

## to test lib_chan
##    make server in one window
##    make client in the other window

MODS = lib_misc \
	   lib_find \
	   lib_trigrams \
	   lib_util \
	   lib_md5


ERL = erl -boot start_clean 

all:	compile

compile: ${MODS:%=%.beam}
	@echo "make dependency lib"
	@echo "make clean - clean up"
