.PHONY: all obj
.SUFFIXES: .cc .o .cpp

GCC = gcc
CXX = g++

PLATFORM = CYGWIN

USER_SRC = main.cc Book.cc Console.cc Interface.cc CxxPrimer.cc

LDFLAGS = -lboost_regex-mt

CXXFLAGS = -Wall -g 

GLOBAL_SRC = 

OBJECTS = $(GLOBAL_SRC:.cc=.o) $(USER_SRC:.cc=.o) 

LIB_PATH = 

TARGET = cxxPrimer

DOCDIR = ~/doxy/cxxPrimer
DOXYCONFIG = cxxPrimerDox

all: $(TARGET)

obj: $(OBJECTS);

doc: $(USER_SRC) $(GLOBAL_SRC)
	doxygen $(DOXYCONFIG)

$(TARGET): $(OBJECTS)
	$(CXX) -o $@ $^ $(LDFLAGS)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -rf *.o *.exe *.dump *.log
	rm -rf latex html