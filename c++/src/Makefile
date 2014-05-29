.PHONY: all obj
.SUFFIXES: .cc .o .cpp

AS 	= as
LD 	= ld
GCC = gcc
CXX = g++
#Var
PLATFORM= CYGWIN
LIB_PATH= 
#Flags
HDR_DIR= ../lib/
CFLAG= -I$(HDR_DIR) -Wall -O -g
CXXFLAGS= -I$(HDR_DIR) -std=c++11 -g 
LDFLAGS= 
# From g++ 4.7, c++0x11 has been supported
GNUVERSION:=$(shell gcc --version | grep ^gcc | sed 's/^.* //g')
CXX11SUPPOTED:= $(shell expr `gcc -dumpversion ` \>= 4.7)
PWD=`pwd`

ifeq "$(CXX11SUPPOTED)" "0"
   	$(error Your c++ compiler does not support c++11)
endif

ACHIIVES=myLibTest

.c.s:
	$(GCC) $(CFLAG) \
	-S -o $*.s $<
.c.o:
	$(GCC) $(CFLAG) \
	-c -o $*.o $<
.cpp.s:
	$(CXX) $(CXXFLAGS) \
	-S -o $*.s $<
.cpp.o:
	$(CXX) $(CXXFLAGS) \
	-c -o $*.o $<

GLOBAL_SRC = 

USER_SRC = mainTest.cpp \
		   list.cpp \
		   mutex.cpp \
		   ptr.cpp \
		   sem.cpp \
		   tree.cpp

OBJECTS = $(GLOBAL_SRC:.cpp=.o) $(USER_SRC:.cpp=.o) 

TARGET = mainTest

all: $(TARGET)

obj:$(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CXX) -o $@ $^ $(LDFLAGS)

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -rf *.o *.exe *.dump *.log
