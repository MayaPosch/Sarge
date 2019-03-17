# Sarge #

Sarge is a simple and powerful command line argument parser, consisting out of just 136 lines of well-commented C++ code, contained in a single class:


	github.com/AlDanial/cloc v 1.80  T=0.01 s (220.4 files/s, 25019.2 lines/s)
	-------------------------------------------------------------------------------
	Language                     files          blank        comment           code
	-------------------------------------------------------------------------------
	C++                              1             37             35            103
	C/C++ Header                     1             12              7             33
	-------------------------------------------------------------------------------
	SUM:                             2             49             42            136
	-------------------------------------------------------------------------------

 

Simply add the header file and source file to one's C++ project and use the class as in the project's test code:

    #include "../src/sarge.h"
	
	#include <iostream>
	
	
	int main(int argc, char** argv) {
		Sarge sarge;
		
		sarge.setArgument("h", "help", "Get help.", false);
		sarge.setArgument("k", "kittens", "K is for kittens. Everyone needs kittens in their life.", true);
		sarge.setDescription("Sarge command line argument parsing testing app. For demonstration purposes and testing.");
		sarge.setUsage("sarge_test <options>");
		
		if (!sarge.parseArguments(argc, argv)) {
			std::cerr << "Couldn't parse arguments..." << std::endl;
			return 1;
		}
		
		std::cout << "Number of flags found: " << sarge.flagCount() << std::endl;
		
		if (sarge.exists("help")) {
			sarge.printHelp();
		}
		else {
			std::cout << "No help requested..." << std::endl;
		}
		
		std::string kittens;
		if (sarge.getFlag("kittens", kittens)) {
			std::cout << "Got kittens: " << kittens << std::endl;
		}
		
		return 0;
	}

Only dependencies are a reasonably modern C++ compiler, capable of supporting at least C++11 (STL datastructure improvements).

## Supported flag types ##

Sarge supports both short and long options, prefixed by one or two dashes ('-') respectively.

Short option: `-h`.

Long option: `--help`.

Options can optionally be followed by a value string. This has to be noted when registering the flag with Sarge in one's code. 

It's also supported to supply multiple short options combined to Sarge, e.g.: `-hnklm`. Important here is that options which require a value to follow them have to always be at the end of such a chain.

Providing a string without flag associated with them is at this point not supported.

## Compiling the test application ##

A Makefile has been added to the root of the project. Simply execute `make` in the folder to compile the test binary into the `bin/` folder. Execute `bin/sarge_test` to get the following output:

	# ./bin/sarge_test.exe -hk Mew
	Number of flags found: 2
	
	Sarge command line argument parsing testing app. For demonstration purposes and testing.
	
	Usage:
	        sarge_test <options>
	
	Options:
	-h      --help          Get help.
	-k      --kittens               K is for kittens. Everyone needs kittens in their life.
	Got kittens: Mew

As you can see, no kittens were harmed in the production of this code :)

## Status ##

The project should not have any major bugs in it, but should be considered beta code. Please use with care and feel free to submit any issues you may encounter while using it.


