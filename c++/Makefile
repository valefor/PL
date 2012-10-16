.PHONY: all obj
.SUFFIXES: .cc .o .cpp

GCC = gcc
CXX = g++

PLATFORM = CYGWIN

USER_SRC = main.cc Book.cc Console.cc

LDFLAGS = 

CXXFLAGS = -Wall -g 

GLOBAL_SRC = 

OBJECTS = $(GLOBAL_SRC:.cc=.o) $(USER_SRC:.cc=.o) 

LIB_PATH = 

TARGET = cxxPrimer

all: $(TARGET)

obj: $(OBJECTS);

$(TARGET): $(OBJECTS)
	$(CXX) $(LDFLAGS) -o $@ $^

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -rf *.o *.exe *.dump
