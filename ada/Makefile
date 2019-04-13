# Makefile for the Sarge command line argument parsing project.


export TOP := $(CURDIR)

GPP = g++
GCC = gcc
GNATMAKE = gnatmake
MAKEDIR = mkdir -p
RM = rm -f
AR = ar
MAKE = make

BIN_OUTPUT := sarge_test
INCLUDE = -Isrc/
FLAGS += 
ADAFLAGS := $(FLAGS) -g $(INCLUDE)
ifdef OS
	#LIBS += -lboost_filesystem-mt -lboost_program_options-mt -lboost_system-mt
	EXT = .exe
else
	#LIBS += -lboost_filesystems -lboost_program_options -lboost_system
endif

SOURCES := $(wildcard src/*.adb)
SPECS := $(wildcard src/*.ads)
OBJECTS := $(addprefix obj/,$(notdir) $(CPP_SOURCES:.adb=.o))

TEST_SOURCES := $(wildcard test/*.adb)
TEST_OBJECTS := $(addprefix obj/,$(notdir) $(TEST_SOURCES:.adb=.o))

all: makedir $(OBJECTS) test 

test: $(TEST_OBJECTS) $(BIN_OUTPUT)
	
obj/%.o: %.adb
	#$(GCC) -c -o $@  $(ADAFLAGS) $(SPECS) $<
	
build:
	$(GNATMAKE) $(SOURCES)
	
$(BIN_OUTPUT):
	$(RM) bin/$@$(EXT)
	#$(GPP) -o bin/$@ $(ADAFLAGS) $(OBJECTS) $(SPECS) $(TEST_OBJECTS) $(LIBS)
	
makedir:
	$(MAKEDIR) bin
	$(MAKEDIR) obj/src
	$(MAKEDIR) obj/test

clean:
	$(RM) $(OBJECTS) $(TEST_OBJECTS)
	
	
	
.PHONY: test src doc