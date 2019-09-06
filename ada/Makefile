# Makefile for the Sarge command line argument parsing project.


GNATMAKE = gnatmake
MAKEDIR = mkdir -p
RM = rm -f

BIN_OUTPUT := sarge_test
ADAFLAGS := -aIsrc -D obj/

TEST_SOURCES := $(wildcard test/*.adb)

all: makedir build

build:
	$(GNATMAKE) -o bin/$(BIN_OUTPUT) $(TEST_SOURCES) $(ADAFLAGS)
	
makedir:
	$(MAKEDIR) bin
	$(MAKEDIR) obj

clean:
	rm -rf obj/
	rm -rf bin/
	
.PHONY: test src doc