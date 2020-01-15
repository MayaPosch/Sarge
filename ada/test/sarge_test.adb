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
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;


procedure Sarge_Test is

function "+"(S : in String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

kittens: Unbounded_String;
number: Unbounded_String;
textarg: Unbounded_String;

begin
	-- Create Sarge instance, set stuff, parse stuff.
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
