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
	procedure setArgument(arg_short: in string, arg_long: in string, desc: in string, hasVal: in boolean) is
	begin
		arg: Argument := (arg_short => arg_short, arg_long => arg_long, description => desc, hasValue => hasVal);
		args.append(arg);
		
		-- Set up links.
		if arg_short'Length > 0 then
			argNames.include(arg_short, arg'Access);
		end if;
		
		if arg_long'Length > 0 then
			argNames.include(arg_long, arg'Access);
		end if;
		
	end setArgument;
	
	
	--- SET DESCRIPTION ---
	procedure setDescription(desc: in string) is
	begin
		description := desc;
	end setDescription;
	
	
	--- SET USAGE ---
	procedure setUsage(usage: in string) is
	begin
		usage := usage;
	end setUsage;
	
	
	--- PARSE ARGUMENTS ---
	function parseArguments() return boolean is
		flag_it: Cursor;
	begin
		-- 
		execName := Ada.Command_Line.command_name;
		expectValue: boolean := Boolean.False;
		for arg in 1..Ada.Command_Line.argument_count loop
			-- Each flag will start with a '-' character. Multiple flags can be joined together in
			-- the same string if they're the short form flag type (one character per flag).
			if expectValue is Boolean.True then
				-- Copy value.
				Get(flag_it).value := arg;
				expectValue := Boolean.False;
			elsif arg(arg'First) = '-' then
				-- Parse flag.
				-- First check for the long form.
				if arg(arg'First + 1) = '-' then
					-- Long form of the flag.
					if not argNames.contains(arg(arg'First + 2..arg'Last)) then
						-- Flag wasn't found. Abort.
						put_line("Long flag " & arg & " wasn't found");
						return Boolean.False;
					end if;
					
					-- Mark as found.
					flag_it := argNames.find(arg);
					Element(flag_it).parsed := Boolean.True;
					flagCounter := flagCounter + 1;
					
					if Element(flag_it).hasValue = Boolean.True then
						expectValue := Boolean.True;
					end if;
				else
					-- Parse short form flag. Parse all of them sequentially. Only the last one
					-- is allowed to have an additional value following it.
					for i in arg'range loop
						flag_it := argNames.find(arg(arg'First + (1 + i)..arg'First + (2 + i)));
						if flag_it = No_Elements then
							-- Flag wasn't found. Abort.
							put_line("Short flag " & arg(arg'First + (1 + i)..arg'First + (2 + i)) & 
									" wasn't found.");
							return Boolean.False;
							
							-- Mark as found.
							Element(flag_it).parsed := Boolean.True;
							flagCounter := flagCounter + 1;
							
							if Element(flag_it).hasValue = Boolean.True then
								if i /= (arg'Length - 1) then
									-- Flag isn't at end, thus cannot have value.
									put_line("Flag " & arg(arg'First + (1 + i)..arg'First + (2 + i))
													& " needs to be followed by a value string.");
									return Boolean.False;
								else
									expectValue := Boolean.True;
								end if;
							end if;
					end loop;
				end if;			
			else
				put_line("Expected flag, not value.");
				return Boolean.False;
			end if;
		end loop
		
		parsed := Boolean.True;
		
		return Boolean.True;
	end parseArguments;
	
	
	--- GET FLAG ---
	function getFlag(arg_flag: in string, arg_value: out arg_value) return boolean is
		flag_it: Cursor;
	begin
		if parsed /= Boolean.True then
			return Boolean.False;
		end if;
		
		flag_it := argNames.find(arg_flag);
		if flag_it = No_Elements then
			return Boolean.False;
		elsif Element(flag_it).parsed /= Boolean.True then
			return Boolean.False;
		end if;
		
		if Element(flag_it).hasValue = Boolean.True then
			arg_value := Element(flag_it).value;
		end if;
		
		return Boolean.True;
	end getFlag;
	
	
	--- EXISTS ---
	function exists(arg_flag: in string) returns boolean is
		flag_it: Cursor;
	begin
		if parsed /= Boolean.True then
			return Boolean.False;
		end if;
		
		flag_it := argNames.find(arg_flag);
		if flag_it = No_Elements then
			return Boolean.False;
		elsif Element(flag_it).parsed /= Boolean.True then
			return Boolean.False;
		end if;
		
		return Boolean.True;
	end exists;
	
	
	--- PRINT HELP ---
	procedure printHelp() is
	begin
		put_line();
		put_line(description);
		put_line("Usage:");
		put_line(usage);
		put_line();
		put_line("Options:");
		
		-- Print out the options.
		for opt in args.Iterate loop
			put_line("-" & opt.arg_short & "    --" & opt.arg_long & "    " & opt.description);
		end loop;
	end printHelp;
	
	
	--- FLAG COUNT ---
	function flagCount() return integer is
	begin
		return flagCount;
	end flagCount;
	
	
	--- EXECUTABLE NAME ---
	function executableName() return string is
	begin
		return execName;
	end executableName;
end Sarge;

