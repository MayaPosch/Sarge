# Makefile for the Sarge command line argument parsing project.


export TOP := $(CURDIR)

GPP = g++
GCC = gcc
MAKEDIR = mkdir -p
RM = rm -f
AR = ar
MAKE = make

BIN_OUTPUT := sarge_test
INCLUDE = -I src/
FLAGS += 
CPPFLAGS := $(FLAGS) -g3 -std=c++14 $(INCLUDE)
ifdef OS
	#LIBS += -lboost_filesystem-mt -lboost_program_options-mt -lboost_system-mt
	EXT = .exe
else
	#LIBS += -lboost_filesystems -lboost_program_options -lboost_system
endif

CPP_SOURCES := $(wildcard src/*.cpp)
CPP_OBJECTS := $(addprefix obj/,$(notdir) $(CPP_SOURCES:.cpp=.o))

TEST_SOURCES := $(wildcard test/*.cpp)
TEST_OBJECTS := $(addprefix obj/,$(notdir) $(TEST_SOURCES:.cpp=.o))

all: makedir $(CPP_OBJECTS) test 

test: $(TEST_OBJECTS) $(BIN_OUTPUT)
	
obj/%.o: %.cpp
	$(GPP) -c -o $@ $< $(CPPFLAGS)
	
$(BIN_OUTPUT):
	$(RM) bin/$@$(EXT)
	$(GPP) -o bin/$@ $(CPPFLAGS) $(CPP_OBJECTS) $(TEST_OBJECTS) $(LIBS)
	
makedir:
	$(MAKEDIR) bin
	$(MAKEDIR) obj/src
	$(MAKEDIR) obj/test

clean:
	$(RM) $(CPP_OBJECTS) $(TEST_OBJECTS)
	
	
	
.PHONY: test src doc