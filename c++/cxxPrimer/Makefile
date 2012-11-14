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

DOC_DIR = doc
DOXY_CONFIG_TEMP = cxxPrimerDox.tmp
DOXY_CONFIG = cxxPrimerDox

all: $(TARGET)

obj: $(OBJECTS);

doc: $(USER_SRC) $(GLOBAL_SRC) docGenPre
	doxygen $(DOXY_CONFIG_TEMP)

docGenPre: $(DOXY_CONFIG)
	-mkdir -p $(DOC_DIR)
	sed "s#^OUTPUT_DIRECTORY.*#OUTPUT_DIRECTORY = $(basename $(DOC_DIR) )#" $< > $(DOXY_CONFIG_TEMP)

$(TARGET): $(OBJECTS)
	$(CXX) -o $@ $^ $(LDFLAGS)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -rf *.o *.exe *.dump *.log
	rm -rf latex html
	rm -rf $(DOC_DIR) $(DOXY_CONFIG_TEMP)
