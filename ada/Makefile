# Makefile for the Sarge command line argument parsing project.


export TOP := $(CURDIR)


GNATMAKE = gnatmake
MAKEDIR = mkdir -p
RM = rm -f
AR = ar
MAKE = make

BIN_OUTPUT := sarge_test
INCLUDE = -Isrc/
FLAGS += 
ADAFLAGS := -aIsrc -D obj/
ifdef OS
	EXT = .exe
else
	# No extension.
endif

SOURCES := $(wildcard src/*.adb)
OBJECTS := $(addprefix obj/,$(notdir) $(SOURCES:.adb=.o))

TEST_SOURCES := $(wildcard test/*.adb)
TEST_OBJECTS := $(addprefix obj/,$(notdir) $(TEST_SOURCES:.adb=.o))

all: makedir $(OBJECTS) test 

test: $(TEST_OBJECTS) $(BIN_OUTPUT)
	
obj/%.o: %.adb
	$(GNATMAKE) $(ADAFLAGS) -c $<
	
build:
	$(GNATMAKE) $(TEST_SOURCES) -aIsrc
	
$(BIN_OUTPUT):
	$(RM) bin/$@$(EXT)
	$(GNATMAKE) -o bin/$@ $(OBJECTS) $(TEST_OBJECTS)
	
makedir:
	$(MAKEDIR) bin
	$(MAKEDIR) obj/src
	$(MAKEDIR) obj/test

clean:
	$(RM) $(OBJECTS) $(TEST_OBJECTS)
	
	
	
.PHONY: test src doc