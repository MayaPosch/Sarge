--	sarge.ads - Specification file for the Sarge command line argument parser project.
	
--	Revision 0
	
--	Notes:
--			-
			 
--	2019/04/10, Maya Posch


with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
use Ada.Containers;


package Sarge is 
	type Argument is record
		arg_short: Unbounded_String;
		arg_long: Unbounded_String;
		description: Unbounded_String;
		hasValue: boolean := False;
		value: Unbounded_String;
		parsed: boolean := False;
	end record;
	
	type Argument_Access is access all Argument;
	
	procedure setArgument(arg_short: in Unbounded_String; arg_long: in Unbounded_String; desc: in Unbounded_String; hasVal: in boolean);
	procedure setDescription(desc: in Unbounded_String);
	procedure setUsage(usage: in Unbounded_String);
	function parseArguments return boolean;
	function getFlag(arg_flag: in Unbounded_String; arg_value: out Unbounded_String) return boolean;
	function exists(arg_flag: in Unbounded_String) return boolean;
	procedure printHelp;
	function flagCount return integer;
	function executableName return Unbounded_String;
	
private
        function "+"(S : in String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	package arg_vector is new Vectors(Natural, Argument);
	args: arg_vector.vector;
	package argNames_map is new Indefinite_Ordered_Maps(Unbounded_String, Argument_Access);
	argNames: argNames_map.map;
	parsed: boolean;
	flagCounter: Integer := 0;
	execName: Unbounded_String;
	description: Unbounded_String;
	usageStr: Unbounded_String;
end Sarge;
