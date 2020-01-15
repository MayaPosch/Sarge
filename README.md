# Sarge #

Sarge is a simple and powerful command line argument parser, with the C++ version consisting out of <200 lines of well-commented C++ code, contained in a single class:


	-------------------------------------------------------------------------------
	Language                     files          blank        comment           code
	-------------------------------------------------------------------------------
	C++                              1             42             41            116
	C/C++ Header                     1             12              7             35
	-------------------------------------------------------------------------------
	SUM:                             2             54             48            151
	-------------------------------------------------------------------------------

 

Simply add the header file and source file to one's C++ project and use the class as in the project's test code:

    #include "../src/sarge.h"
	
	#include <iostream>
	
	
	int main(int argc, char** argv) {
		Sarge sarge;
		
		sarge.setArgument("h", "help", "Get help.", false);
		sarge.setArgument("k", "kittens", "K is for kittens. Everyone needs kittens in their life.", true);
		sarge.setArgument("n", "number", "Gimme a number. Any number.", true);
		sarge.setArgument("a", "apple", "Just an apple.", false);
		sarge.setArgument("b", "bear", "Look, it's a bear.", false);
		sarge.setArgument("", "snake", "Snakes only come in long form, there are no short snakes.", false);
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

		std::string textarg;
		if (sarge.getTextArgument(0, textarg)) {
			std::cout << "Got text argument: " << textarg << std::endl;
		}
		
		return 0;
	}

Only dependencies are a reasonably modern C++ compiler, capable of supporting at least C++11 (STL datastructure improvements).

## API ##

	void setArgument(std::string arg_short, std::string arg_long, std::string desc, bool hasVal);
	void setArguments(std::vector<Argument> args);
	void setDescription(std::string desc);
	void setUsage(std::string use);
	bool parseArguments(int argc, char** argv);
	bool getFlag(std::string arg_flag, std::string &arg_value);
	bool exists(std::string arg_flag);
	bool getTextArgument(uint32_t index, std::string &value);
	void printHelp();
	int flagCount();
	std::string executableName();

## Supported flag types ##

Sarge supports both short and long options, prefixed by one or two dashes ('-') respectively. The short option can be left empty, which will only enable the long option.

Short option: `-h`.

Long option: `--help`.

Options can optionally be followed by a value string. This has to be noted when registering the flag with Sarge in one's code. 

It's also supported to supply multiple short options combined to Sarge, e.g.: `-hnklm`. Important here is that options which require a value to follow them have to always be at the end of such a chain.

String without flag associated with them are made available using the `getTextArgument()` method after parsing. These arguments are only allowed to exist after the flags section.

## Compiling the test application ##

A Makefile has been added to the root of the project. Simply execute `make` in the folder to compile the test binary into the `bin/` folder. Execute `bin/sarge_test` to get the following output:

	# ./bin/sarge_test.exe -hk Mew
	Number of flags found: 2
	
	Sarge command line argument parsing testing app. For demonstration purposes and testing.
	
	Usage:
	        sarge_test <options>
	
	Options:
	-h, --help      Get help.
	-k, --kittens   K is for kittens. Everyone needs kittens in their life.
	-n, --number    Gimme a number. Any number.
	-a, --apple     Just an apple.
	-b, --bear      Look, it's a bear.
	    --snake     Snakes only come in long form, there are no short snakes.


As you can see, no kittens were harmed in the production of this code :)

# Ada version #

The Ada version of Sarge (found in the `ada/` folder) is pretty much a straight port of the C++ version. It consists out of a single package (Sarge), with <200 lines of code. 

	-------------------------------------------------------------------------------
	Language                     files          blank        comment           code
	-------------------------------------------------------------------------------
	Ada                              2             58             44            197
	-------------------------------------------------------------------------------
	SUM:                             2             58             44            197
	-------------------------------------------------------------------------------


Its biggest limitation compared to the C++ version at this point is that one cannot use multiple instances of Sarge since the relevant data structures are part of the package. This should not pose any issues in the average usage scenario, however.


## API ##

	procedure setArgument(arg_short: in Unbounded_String; arg_long: in Unbounded_String; desc: in Unbounded_String; hasVal: in boolean);
	procedure setDescription(desc: in Unbounded_String);
	procedure setUsage(usage: in Unbounded_String);
	function parseArguments return boolean;
	function getFlag(arg_flag: in Unbounded_String; arg_value: out Unbounded_String) return boolean;
	function exists(arg_flag: in Unbounded_String) return boolean;
	function getTextArgument(index: in Integer; value: out Unbounded_String) return boolean;
	procedure printHelp;
	function flagCount return integer;
	function executableName return Unbounded_String;

## Example ##

The test application has also been ported from the C++ version, showing the use of the package:

	with Sarge;
	with Ada.Text_IO;
	use Ada.Text_IO;
	with Ada.Strings.Unbounded;
	use Ada.Strings.Unbounded;
	with Ada.Strings.Unbounded.Text_IO;
	use Ada.Strings.Unbounded.Text_IO;


	procedure Sarge_Test is

	function "+"(S : in String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

	kittens: Unbounded_String;
	number: Unbounded_String;

	begin
	   Sarge.setArgument(+"h", +"help", +"Get help.", False);
		Sarge.setArgument(+"k", +"kittens", +"K is for kittens. Everyone needs kittens in their life.", True);
		Sarge.setArgument(+"n", +"number", +"Gimme a number. Any number.", True);
		Sarge.setArgument(+"a", +"apple", +"Just an apple.", False);
		Sarge.setArgument(+"b", +"bear", +"Look, it's a bear.", False);
		Sarge.setArgument(+"", +"snake", +"Snakes only come in long form, there are no short snakes.", False);
			Sarge.setDescription(+"Sarge command line argument parsing testing app. For demonstration purposes and testing.");
		Sarge.setUsage(+"sarge_test <options>");

		if Sarge.parseArguments /= True then
			put_line("Couldn't parse arguments...");
			return;
		end if;

		put_line("Number of flags found: " & Sarge.flagCount'Image);

		if Sarge.exists(+"help") /= False then
			Sarge.printHelp;
		else
			put_line("No help requested...");
		end if;

		-- Read out Kittens and Number.
		if Sarge.getFlag(+"kittens", kittens) = True then
			put_line("Got kittens: " & kittens);
		end if;

		if Sarge.getFlag(+"number", number) = True then
			put_line("Got number: " & number);
		end if;

		if Sarge.getTextArgument(0, textarg) = True then
			put_line("Got text argument: " & textarg);
		end if;

	end Sarge_Test; 

The Makefile in the `ada` folder should be used when compiling the test application. The Gnat Programming Studio project file is currently not maintained. 

The Sarge package is found in the `ada/src` folder. One can use it directly from there by including it one's project, or copying it into the project's source tree, depending on one's requirements.


