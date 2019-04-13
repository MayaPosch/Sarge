--	sarge.ads - Specification file for the Sarge command line argument parser project.
	
--	Revision 0
	
--	Notes:
--			-
			 
--	2019/04/10, Maya Posch


with Ada.Strings;
use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
use Ada.Containers;


package Sarge is 
	type Argument is record
		arg_short: string;
		arg_long: string;
		description: string;
		hasValue: boolean := Boolean.False;
		value: string;
		parsed: boolean := Boolean.False;
	end record;
	
	type Argument_Access is access all Argument;
	
	procedure setArgument(arg_short: in string; arg_long: in string; desc: in string; hasVal: in boolean);
	procedure setDescription(desc: in string);
	procedure setUsage(usage: in string);
	function parseArguments return boolean;
	function getFlag(arg_flag: in string; arg_value: out arg_value) return boolean;
	function exists(arg_flag: in string) return boolean;
	procedure printHelp;
	function flagCount return integer;
	function executableName return string;
	
private
	package arg_vector is new Vectors(Natural, Argument);
	args: arg_vector.vector;
	package argNames_map is new Indefinite_Ordered_Maps(string, Argument_Access);
	argNames: argNames_map.map;
	parsed: boolean;
	flagCounter: Integer := 0;
	execName: string;
	description: string;
	usage: string;
end Sarge;
