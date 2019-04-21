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


package body Sarge is
    --- SET ARGUMENT ---
    procedure setArgument(arg_short: in Unbounded_String; arg_long: in Unbounded_String; desc: in Unbounded_String; hasVal: in boolean) is
	arg: aliased Argument := (arg_short => arg_short, arg_long => arg_long, description => desc, hasValue => hasVal, value => +"", parsed => False);
	aa: Argument_Access;
    begin
	args.append(arg);
		
	-- Set up links.
	if length(arg_short) > 0 then
	    aa := args.Last_Element'Access;
	    argNames.include(arg_short, aa);
	end if;
		
	if length(arg_long) > 0 then
	    argNames.include(arg_long, arg'Access);
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
    begin
	-- 
	execName := Ada.Command_Line.command_name;
	for arg in 1..Ada.Command_Line.argument_count loop
	    -- Each flag will start with a '-' character. Multiple flags can be joined together in
	    -- the same string if they're the short form flag type (one character per flag).
	    if expectValue = True then
		-- Copy value.
		argNames(flag_it).value := arg;
		expectValue := False;
	    elsif arg(arg'First) = '-' then
		-- Parse flag.
		-- First check for the long form.
		if arg(arg'First + 1) = '-' then
		    -- Long form of the flag.
		    if not argNames.contains(arg(arg'First + 2..arg'Last)) then
			-- Flag wasn't found. Abort.
			put_line("Long flag " & arg'Image & " wasn't found");
			return False;
		    end if;
					
		    -- Mark as found.
		    flag_it := argNames.find(arg);
		    argNames_map.Element(flag_it).parsed := True;
		    flagCounter := flagCounter + 1;
					
		    if argNames_map.Element(flag_it).hasValue = True then
			expectValue := True;
		    end if;
		else
		    -- Parse short form flag. Parse all of them sequentially. Only the last one
		    -- is allowed to have an additional value following it.
		    for i in arg'range loop
			flag_it := argNames.find(arg(arg'First + (1 + i)..arg'First + (2 + i)));
			if flag_it = argNames_map.No_Element then
			    -- Flag wasn't found. Abort.
			    put_line("Short flag " & arg(arg'First + (1 + i)..arg'First + (2 + i)) & 
		  " wasn't found.");
			    return False;
			end if;
							
			-- Mark as found.
			argNames_map.Element(flag_it).parsed := True;
			flagCounter := flagCounter + 1;
							
			if argNames_map.Element(flag_it).hasValue = True then
			    if i /= (arg'Length - 1) then
				-- Flag isn't at end, thus cannot have value.
				put_line("Flag " & arg(arg'First + (1 + i)..arg'First + (2 + i))
	     & " needs to be followed by a value string.");
				return False;
			    else
				expectValue := True;
			    end if;
			end if;
		    end loop;
		end if;	
	    else
		put_line("Expected flag, not value.");
		return False;
	    end if;
	end loop;
		
	parsed := True;
		
	return True;
    end parseArguments;
	
	
    --- GET FLAG ---
    function getFlag(arg_flag: in Unbounded_String; arg_value: out Unbounded_String) return boolean is
	flag_it: argNames_map.Cursor;
    begin
	if parsed /= True then
	    return False;
	end if;
		
	flag_it := argNames.find(arg_flag);
	if flag_it = No_Elements then
	    return False;
	elsif Element(flag_it).parsed /= True then
	    return False;
	end if;
		
	if Element(flag_it).hasValue = True then
	    arg_value := Element(flag_it).value;
	end if;
		
	return True;
    end getFlag;
	
	
    --- EXISTS ---
    function exists(arg_flag: in Unbounded_String) return boolean is
	flag_it: argNames_map.Cursor;
    begin
	if parsed /= True then
	    return False;
	end if;
		
	flag_it := argNames.find(arg_flag);
	if flag_it = No_Elements then
	    return False;
	elsif Element(flag_it).parsed /= True then
	    return False;
	end if;
		
	return True;
    end exists;
	
	
    --- PRINT HELP ---
    procedure printHelp is
    begin
	put_line;
	put_line(description);
	put_line("Usage:");
	put_line(usageStr);
	put_line;
	put_line("Options:");
		
	-- Print out the options.
	for opt in args.Iterate loop
	    put_line("-" & opt.arg_short & "    --" & opt.arg_long & "    " & opt.description);
	end loop;
    end printHelp;
	
	
    --- FLAG COUNT ---
    function flagCount return integer is
    begin
	return flagCount;
    end flagCount;
	
	
    --- EXECUTABLE NAME ---
    function executableName return Unbounded_String is
    begin
	return execName;
    end executableName;
end Sarge;

