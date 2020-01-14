/*
	sarge_test.cpp - Implementation file for the Sarge command line argument parser test.
	
	Revision 0
	
	Features:
			- 
	
	Notes:
			-
			 
	2019/03/16, Maya Posch
	
*/


#include "../src/sarge.h"

#include <iostream>


int main(int argc, char** argv) {
	// Create Sarge instance, set stuff, parse stuff.
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
	
	std::string number;
	if (sarge.getFlag("number", number)) {
		std::cout << "Got number: " << number << std::endl;
	}
	
	std::string textarg;
	if (sarge.getTextArgument(0, textarg)) {
		std::cout << "Got text argument: " << textarg << std::endl;
	}
	
	return 0;
}

