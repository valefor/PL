.PHONY: all obj
.SUFFIXES: .cc .o .cpp

GCC = gcc
CXX = g++

PLATFORM = CYGWIN

USER_SRC = mainTest.cpp \
		   list.cpp \
		   ptr.cpp

LDFLAGS = 

CXXFLAGS = -Wall -g 

GLOBAL_SRC = 

OBJECTS = $(GLOBAL_SRC:.cpp=.o) $(USER_SRC:.cpp=.o) 

LIB_PATH = 

TARGET = mainTest

all: $(TARGET)

obj: $(OBJECTS);

$(TARGET): $(OBJECTS)
	$(CXX) -o $@ $^ $(LDFLAGS)

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -rf *.o *.exe *.dump *.log
