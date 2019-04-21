--	sarge_test.adb - Implementation file for the Sarge command line argument parser test.
	
--	Revision 0
	
--	Features:
--			- 
	
--	Notes:
--			-
			 
--	2019/04/10, Maya Posch


with Sarge;
with Ada.Text_IO;
use Ada.Text_IO;

procedure Sarge_test is
begin
	-- Create Sarge instance, set stuff, parse stuff.
	Sarge.setArgument("h", "help", "Get help.", Boolean.False);
	Sarge.setArgument("k", "kittens", "K is for kittens. Everyone needs kittens in their life.", True);
	Sarge.setArgument("n", "number", "Gimme a number. Any number.", True);
	Sarge.setArgument("a", "apple", "Just an apple.", False);
	Sarge.setArgument("b", "bear", "Look, it's a bear.", False);
	Sarge.setDescription("Sarge command line argument parsing testing app. For demonstration purposes and testing.");
	Sarge.setUsage("sarge_test <options>");
	
	if Sarge.parseArguments /= True then
		put_line("Couldn't parse arguments...");
		return;
	end if;
	
	put_line("Number of flags found: " & Sarge.flagCount);
	
	if Sarge.exists("help") /= False then
		Sarge.printHelp;
	else
		put_line("No help requested...");
	end if;
	
	--
	
end Sarge_test;
