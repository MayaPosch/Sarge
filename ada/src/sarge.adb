--	sarge.adb - Implementation file for the Sarge command line argument parser project.
	
--	Revision 0
	
--	Features:
--			- 
	
--	Notes:
--			-
			 
--	2019/04/10, Maya Posch



with Ada.Command_Line;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;


package body Sarge is
    --- SET ARGUMENT ---
    procedure setArgument(arg_short: in Unbounded_String; arg_long: in Unbounded_String; desc: in Unbounded_String; hasVal: in boolean) is
	arg: aliased Argument := (arg_short => arg_short, arg_long => arg_long, description => desc, hasValue => hasVal, value => +"", parsed => False);
    begin
	args.append(arg);
		
	-- Set up links.
	if length(arg_short) > 0 then
	    argNames.include(arg_short, args.Last_Index);
	end if;
		
	if length(arg_long) > 0 then
	    argNames.include(arg_long, args.Last_Index);
	end if;
		
    end setArgument;
	
	
    --- SET DESCRIPTION ---
    procedure setDescription(desc: in Unbounded_String) is
    begin
	description := desc;
    end setDescription;
	
	
    --- SET USAGE ---
    procedure setUsage(usage: in Unbounded_String) is
    begin
	usageStr := usage;
    end setUsage;
	
	
    --- PARSE ARGUMENTS ---
    function parseArguments return boolean is
	flag_it: argNames_map.Cursor;
	expectValue: boolean := False;
	arg: Unbounded_String;
	short_arg: Unbounded_String;
    begin
	-- 
	execName := +Ada.Command_Line.command_name;
	for arg_i in 1..Ada.Command_Line.argument_count loop
	    arg := +Ada.Command_Line.Argument(arg_i);
	    -- Each flag will start with a '-' character. Multiple flags can be joined together in
	    -- the same string if they're the short form flag type (one character per flag).
	    if expectValue = True then
		-- Copy value.
		args.Reference(argNames_map.Element(flag_it)).value := arg;		
		expectValue := False;
	    elsif Ada.Strings.Unbounded.Slice(arg, 1, 1) = "-" then
		-- Parse flag.
		-- First check for the long form.
		if Ada.Strings.Unbounded.Slice(arg, 1, 2) = "--" then
		    -- Long form of the flag.
		    -- First delete the preceding dashes.
		    arg := Ada.Strings.Unbounded.Delete(arg, 1, 2);
		    if not argNames.contains(arg) then
			-- Flag wasn't found. Abort.
			Ada.Strings.Unbounded.Text_IO.put_line("Long flag " & arg & " wasn't found");
			return False;
		    end if;
					
		    -- Mark as found.
		    flag_it := argNames.find(arg);
		    args(argNames_map.Element(flag_it)).parsed := True;
		    flagCounter := flagCounter + 1;
					
		    if args(argNames_map.Element(flag_it)).hasValue = True then
			expectValue := True;
		    end if;
		else
		    -- Parse short form flag. Parse all of them sequentially. Only the last one
		    -- is allowed to have an additional value following it.
		    -- First delete the preceding dash.
		    arg := Ada.Strings.Unbounded.Delete(arg, 1, 1);
		    for i in 1 .. Ada.Strings.Unbounded.Length(arg) loop
			Ada.Strings.Unbounded.Append(short_arg, Ada.Strings.Unbounded.Element(arg, i));
			if argNames_map.Contains(argNames, short_arg) /= True then
			    -- Flag wasn't found. Abort.
			    put_line("Short flag " & short_arg & " wasn't found.");
			    return False;
			end if;
			
			flag_it := argNames.find(short_arg);
							
			-- Mark as found.
			args(argNames_map.Element(flag_it)).parsed := True;
			flagCounter := flagCounter + 1;
							
			if args(argNames_map.Element(flag_it)).hasValue = True then
			    if i /= (Ada.Strings.Unbounded.Length(arg)) then
				-- Flag isn't at end, thus cannot have value.
				put_line("Flag " & short_arg & " needs to be followed by a value string.");
				return False;
			    else
				expectValue := True;
			    end if;
			end if;
			
			Ada.Strings.Unbounded.Delete(short_arg, 1, 1);
		    end loop;
		end if;	
	    else
			-- Add to text argument vector.
			textArguments.append(arg);
	    end if;
	end loop;
		
	parsed := True;
		
	return True;
    end parseArguments;
	
	
    --- GET FLAG ---
    function getFlag(arg_flag: in Unbounded_String; arg_value: out Unbounded_String) return boolean is
	flag_it: argNames_map.Cursor;
	use argNames_map;
    begin
	if parsed /= True then
	    return False;
	end if;
		
	flag_it := argNames.find(arg_flag);
	if flag_it = argNames_map.No_Element then
	    return False;
	elsif args(argNames_map.Element(flag_it)).parsed /= True then
	    return False;
	end if;
		
	if args(argNames_map.Element(flag_it)).hasValue = True then
	    arg_value := args(argNames_map.Element(flag_it)).value;
	end if;
		
	return True;
    end getFlag;
	
	
    --- EXISTS ---
    function exists(arg_flag: in Unbounded_String) return boolean is
	flag_it: argNames_map.Cursor;
	use argNames_map;
    begin
	if parsed /= True then
	    return False;
	end if;
		
	flag_it := argNames.find(arg_flag);
	if flag_it = argNames_map.No_Element then
	    return False;
	elsif args(argNames_map.Element(flag_it)).parsed /= True then
		return False;
	end if;
		
	return True;
    end exists;
	
	
	--- GET TEXT ARGUMENT ---
	function getTextArgument(index: in Integer; value: out Unbounded_String) return boolean is
	begin
		if index < Integer(tArgVector.length(textArguments)) then
			value := textArguments(index);
			return True;
		end if;
		
		return False;
	end getTextArgument;
	
	
    --- PRINT HELP ---
    procedure printHelp is
	count: Integer := 1;
	spaceCnt: Integer;
    begin
	put_line("");
	put_line(description);
	put_line("Usage:");
	put_line(usageStr);
	put_line("");
	put_line("Options:");
	
	-- Determine whitespace needed between arg_long and description.
	for flag in args.Iterate loop
		if Integer(Ada.Strings.Unbounded.length(args(flag).arg_long)) > count then
			count := Integer(Ada.Strings.Unbounded.length(args(flag).arg_long));
		end if;
	end loop;
	
	count := count + 3; -- Number of actual spaces between the longest arg_long and description.
		
	-- Print out the options.
	for opt in args.Iterate loop
		--spaceStr := Unbound_String(count - Ada.Strings.Unbounded.length(args(opt).arg_long)
		spaceCnt := (count - Integer(Ada.Strings.Unbounded.length(args(opt).arg_long)));
		if Ada.Strings.Unbounded.length(args(opt).arg_short) < 1 then
			Ada.Strings.Unbounded.Text_IO.put_line("    " & args(opt).arg_short 
					    & "--" & args(opt).arg_long 
					    & spaceCnt * " " & args(opt).description);
		else
			Ada.Strings.Unbounded.Text_IO.put_line("-" & args(opt).arg_short 
					    & ", --" & args(opt).arg_long 
					    & spaceCnt * " " & args(opt).description);
		end if;
	end loop;
    end printHelp;
	
	
    --- FLAG COUNT ---
    function flagCount return integer is
    begin
	return flagCounter;
    end flagCount;
	
	
    --- EXECUTABLE NAME ---
    function executableName return Unbounded_String is
    begin
	return execName;
    end executableName;
end Sarge;

