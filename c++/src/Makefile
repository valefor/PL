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
CXXFLAGS= -I$(HDR_DIR) -std=c++11 -O -g
LDFLAGS= 
GCC_VERSION_FULL=`gcc --version |head -n 1`
GCC_VERSION=$(word 3, $(GCC_VERSION_FULL))
PWD=`pwd`

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

USER_SRC = list.cpp \
		   mutex.cpp \
		   ptr.cpp \
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
