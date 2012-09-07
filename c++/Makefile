.PHONY: all obj
.SUFFIXES: .cc .o .cpp

GCC = gcc
CXX = g++

PLATFORM = CYGWIN

USER_SRC = ex10_2.cc

LDFLAGS = 

CXXFLAGS = -Wall -g

GLOBAL_SRC = utils.cc

OBJECTS = $(GLOBAL_SRC:.cc=.o) $(USER_SRC:.cc=.o) 

LIB_PATH = 

TARGET = transformer

all: $(TARGET)

obj: $(OBJECTS);

$(TARGET): $(OBJECTS)
	$(CXX) $(LDFLAGS) -o $@ $^

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -rf *.o *.exe *.dump
