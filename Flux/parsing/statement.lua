
local types = require "common.types"

types.parseMany [[
IfStatement: { "type" = "IfStatement", "condition" = Expression, "block" = Block, "elseblock" = Block | nil } & HasPosition
WhileLoop: { "type" = "WhileLoop", "condition" = Expression, "block" = Block } & HasPosition
RepeatLoop: { "type" = "RepeatLoop", "condition" = Expression, "block" = Block } & HasPosition
DoStatement: { "type" = "DoStatement", "block" = Block } & HasPosition
ForLoop: { "type" = "ForLoop", "init_class" = Type, "init_name" = string, "init_value" = Expression | nil, "test" = Expression, "increment" = Expression, "block" = Block } & HasPosition
ForeachLoop: { "type" = "ForeachLoop", "name1" = string | false, "name2" = string, "expression" = Expression, "block" = Block } & HasPosition
SwitchStatement: { "type" = "SwitchStatement", "expression" = Expression, "cases" = { number = { "case" = Expression, "block" = Block } } | {}, "default" = Block | nil } & HasPosition
TryStatement: { "type" = "TryStatement", "block" = Block, "catches" = { number = { "class" = Type, "name" = string, "block" = Block } } | {}, "default" = Block | nil } & HasPosition
ReturnStatement: { "type" = "ReturnStatement", "value" = Expression } & HasPosition
BreakStatement: { "type" = "BreakStatement" } & HasPosition
ContinueStatement: { "type" = "ContinueStatement" } & HasPosition
ExpressionStatement: { "type" = "ExpressionStatement", "value" = Expression } & HasPosition

Statement:
	  IfStatement
	| WhileLoop
	| RepeatLoop
	| DoStatement
	| ForLoop
	| ForeachLoop
	| SwitchStatement
	| TryStatement
	| ReturnStatement
	| BreakStatement
	| ContinueStatement
	| Definition
	| TemplateDefinition
	| ExpressionStatement
]]

local function parseIfStatement( source, pos )
	local lexer = source.lexer
	local condition = parseExpression( source ) or throw( lexer, "expected condition after 'if'" )
	local block = parseBlock( source, "general" )
	local elseblock = lexer:skip( "Keyword", "else" ) and parseBlock( source ) or nil

	source:push( wrapIfStatement( condition, block, elseblock, pos ) )
end

local function parseWhileLoop( source, pos )
	local lexer = source.lexer
	local condition = parseExpression( source ) or throw( lexer, "expected condition after 'while'" )
	local block = parseBlock( source, "whileloop" )

	source:push {
		type = "WhileLoop";
		condition = condition;
		block = block;
		position = pos;
	}
end

local function parseRepeatLoop( source, pos )
	local lexer = source.lexer
	local block = parseBlock( source, "repeatloop" )
	local isWhile = lexer:skip( "Keyword", "while" )
	local isUntil = isWhile or lexer:skip( "Keyword", "until" ) or throw( lexer, "expected 'until' or 'while' after 'repeat' block" )
	local condition = parseExpression( source ) or throw( lexer, "expected condition after '" .. (isWhile and "while'" or "until'") )

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';' after condition" )
	end

	source:push {
		type = "RepeatLoop";
		condition = isWhile and logicalNotExpression( condition ) or condition;
		block = block;
		position = pos;
	}
end

local function parseDoStatement( source, pos )
	local lexer = source.lexer
	local block = parseBlock( source, "general" )

	source:push {
		type = "DoStatement";
		block = block;
		position = pos;
	}
end

local function parseForLoop( source, pos )
	local lexer = source.lexer
	local openbracket = lexer:skip( "Symbol", "(" )
	local init_class = lexer:skipValue( "Keyword", "auto" ) or assertType( parseType( source ) )
	local init_name = lexer:skipValue "Identifier" or throw( lexer, "expected initialiser variable name" )
	local init_value = lexer:skip( "Symbol", "=" ) and (parseExpression( source ) or throw( lexer, "expected expression after '='" )) or nil
	local comma = lexer:skip( "Symbol", ";" ) or lexer:skip( "Symbol", "," ) or throw( lexer, "expected ';' after for loop initialiser" )
	local test_expr = parseExpression( source ) or throw( lexer, "expeced for loop test expression" )
	local comma = lexer:skip( "Symbol", ";" ) or lexer:skip( "Symbol", "," ) or throw( lexer, "expected ';' after for loop test" )
	local inc_expr = parseExpression( source ) or throw( lexer, "expected for loop increment expression" )
	local close = openbracket and (lexer:skip( "Symbol", ")" ) or throw( lexer, "expected ')'" ))
	local block = parseBlock( source )

	source:push {
		type = "ForLoop";
		init_class = init_class;
		init_name = init_name;
		init_value = init_value;
		test = test_expr;
		increment = inc_expr;
		block = block;
		position = pos;
	}
end

local function parseForeachLoop( source, pos )
	local lexer = source.lexer
	local name1 = lexer:skipValue "Identifier" or throw( lexer, "expected name after 'foreach'" )
	local name2 = lexer:skip( "Symbol", "," ) and lexer:skipValue "Identifier" or false
	local expr = lexer:skip( "Keyword", "in" ) and (parseExpression( source ) or throw( lexer, "expected expression after 'in'" )) or throw( lexer, "expected 'in' after name" )
	local block = parseBlock( source, "foreachloop" )

	if not name2 then
		name1, name2 = name2, name1
	end

	source:push {
		type = "ForeachLoop";
		name1 = name1;
		name2 = name2;
		expression = expr;
		block = block;
		position = pos;
	}
end

local function parseSwitchStatement( source, pos )
	local lexer = source.lexer
	local expr = parseExpression( source ) or throw( lexer, "expected expression to switch" )
	local cases = {}
	local default

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';' after switch expression" )
	end

	while lexer:skip( "Keyword", "case" ) do
		local case = parseExpression( source ) or throw( lexer, "expected case expression" )
		local block = parseBlock( source, "general" )

		cases[#cases + 1] = { case = case, block = block }
	end

	if #cases == 0 then
		throw( lexer, "expected at least one case to switch" )
	end

	if lexer:skip( "Keyword", "default" ) then
		default = parseBlock( source, "general" )
	end

	source:push {
		type = "SwitchStatement";
		expression = expr;
		cases = cases;
		default = default;
		position = pos;
	}
end

local function parseTryStatement( source, pos )
	local lexer = source.lexer
	local block = parseBlock( source )
	local catches = {}
	local default

	while lexer:skip( "Keyword", "catch" ) do
		local class = assertType( parseType( source ) )
		local name = lexer:skipValue "Identifier" or throw( lexer, "expected exception name after type" )
		local block = parseBlock( source, "general" )

		catches[#catches + 1] = { class = class, name = name, block = block }
	end

	if #catches == 0 then
		throw( lexer, "expected at least one catch after try" )
	end

	if lexer:skip( "Keyword", "default" ) then
		default = parseBlock( source, "general" )
	end

	source:push {
		type = "TryStatement";
		block = block;
		catches = catches;
		default = default;
		position = pos;
	}
end

function parseNewStatement( source, pos )
	local lexer = source.lexer
	local class = assertType( parseType( source ) )
	local name = parseName( source ) or throw( lexer, "expected name" )
	local parameters = {}

	if lexer:skip( "Symbol", "(" ) then
		parameters = parseFunctionCallParameters( source )
	end

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';'" )
	end

	source:push {
		type = "Definition";
		name = name;
		class = class;
		const = false;
		value = wrapNewExpression( class, parameters, pos );
		position = pos;
	}
end

local function parseReturnStatement( source, pos )
	source:push( wrapReturnStatement( parseExpression( source ) or nullExpression( pos ) ) )

	if not expectSemicolon( source.lexer ) then
		throw( source.lexer, "expected ';' after return statement" )
	end
end

local function parseBreakStatement( source, pos )
	--[[ if not source:isLoop() then
		source.lexer:back()
		throw( source.lexer, "unexpected 'break' outside of loop" )
	end ]]

	source:push {
		type = "BreakStatement";
		position = pos;
	}

	if not expectSemicolon( source.lexer ) then
		throw( source.lexer, "expected ';' after break statement" )
	end
end

local function parseContinueStatement( source, pos )
	if not source:isLoop() then
		source.lexer:back()
		throw( source.lexer, "unexpected 'break' outside of loop" )
	end

	source:push {
		type = "ContinueStatement";
		position = pos;
	}

	if not expectSemicolon( source.lexer ) then
		throw( source.lexer, "expected ';' after continue statement" )
	end
end

function parseLetStatement( source, pos, expectFunction )
	local lexer = source.lexer
	local const = lexer:skip( "Keyword", "const" ) and true or false

	repeat
		local name1 = parseName( source )
		local cast1 = name1 and lexer:skip( "Symbol", "->" ) and assertType( parseType( source ) ) or "auto"
		local backtick = lexer:skipValue "Backtick"
		local name2 = backtick and parseName( source ) or not name1 and throw( lexer, "expected name" )
		local cast2 = name2 and lexer:skip( "Symbol", "->" ) and assertType( parseType( source ) ) or "auto"
		local function_parameters

		if not backtick and lexer:skip( "Symbol", "(" ) then
			function_parameters = {}

			if not lexer:skip( "Symbol", ")" ) then
				while true do
					local name = lexer:skipValue "Identifier" or throw( lexer, "expected parameter name" )
					local class = lexer:skip( "Symbol", "->" ) and assertType( parseType( source ) ) or "auto"

					function_parameters[#function_parameters + 1] = { name = name, class = class, nullable = false }

					if not lexer:skip( "Symbol", "," ) then
						break
					end
				end

				if not lexer:skip( "Symbol", ")" ) then
					throw( lexer, "expected ')'" )
				end
			end

		elseif backtick then
			function_parameters = { { name = name1 or name2, class = cast1 or cast2, nullable = false }, name1 and name2 and { name = name2, class = cast2, nullable = false } or nil }
			name2 = nil
			name1 = backtick

		elseif expectFunction then
			throw( lexer, "expected '(' for function definition" )

		end

		local expr = lexer:skip( "Symbol", "=" ) and (parseExpression( source ) or throw( lexer, "expected expression after '='" )) or throw( lexer, "expected '='" )

		if function_parameters then
			source:begin "function"

			while lexer:skip( "Keyword", "where" ) do
				local position = lexer:peek(-1).position
				local name = lexer:skipValue "Identifier" or throw( lexer, "expected name" )
				local value = lexer:skip( "Symbol", "=" ) and (parseExpression( source ) or throw( lexer, "expected expression after '='" )) or throw( lexer, "expected '='" )

				source:push( wrapDefinition( name, "auto", value, true, nil, position ) )
			end

			source:push( wrapReturnStatement( expr ) )

			local block = source:pop()
			local function_parameter_types = {}

			for i = 1, #function_parameters do
				function_parameter_types[i] = function_parameters[i].class
			end

			source:push {
				type = "Definition";
				name = source:resolveDefinitionName( name1 );
				class = wrapFunctionType( "auto", function_parameter_types );
				value = wrapFunction( "auto", function_parameters, block );
				const = const;
				position = pos;
			}

			break

		else
			source:push {
				type = "Definition";
				class = "auto";
				name = source:resolveDefinitionName( name1 );
				value = expr;
				const = const;
				position = pos;
			}

		end

		pos = lexer:get().position

	until not lexer:skip( "Symbol", "," )

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';'" )
	end
end

function parseStatement( source )
	local lexer = source.lexer
	local position = lexer:get().position
	local keyword = lexer:skip "Keyword"

	if keyword then

		if keyword.value == "if" then
			return parseIfStatement( source, position )

		elseif keyword.value == "while" then
			return parseWhileLoop( source, position )

		elseif keyword.value == "repeat" then
			return parseRepeatLoop( source, position )

		elseif keyword.value == "do" then
			return parseDoStatement( source, position )

		elseif keyword.value == "for" then
			return parseForLoop( source, position )

		elseif keyword.value == "foreach" then
			return parseForeachLoop( source, position )

		elseif keyword.value == "switch" then
			return parseSwitchStatement( source, position )

		elseif keyword.value == "try" then
			return parseTryStatement( source, position )

		elseif keyword.value == "return" then
			return parseReturnStatement( source, position )

		elseif keyword.value == "break" then
			return parseBreakStatement( source, position )

		elseif keyword.value == "continue" then
			return parseContinueStatement( source, position )

		elseif keyword.value == "let" then
			return parseLetStatement( source, position )

		elseif keyword.value == "template" then
			return parseFunctionTemplate( source, position )

		elseif keyword.value == "new" then
			local pos = lexer:mark()

			if parseType( source ) and lexer:test "Identifier" then
				lexer:jump( pos )
				return parseNewStatement( source )
			else
				lexer:jump( pos )
			end

		end

		lexer:back()
	end

	if parseDefinition( source ) then
		return
	end

	local expr = parseExpression( source ) or throw( lexer, "unexpected token: " .. lexer:get():tostring() )

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';' after expression" )
	end

	source:push( wrapExpressionStatement( expr, position ) )

end

function serializeStatement( t )

	if t.type == "IfStatement" then
		return "if " .. serializeExpression( t.condition ) .. " " .. serializeBlock( t.block )
		.. (t.elseblock and "\nelse " .. (#t.elseblock == 1 and t.elseblock[1].type == "IfStatement" and serializeStatement( t.elseblock[1] ) or serializeBlock( t.elseblock )) or "")

	elseif t.type == "WhileLoop" then
		return "while " .. serializeExpression( t.condition ) .. " " .. serializeBlock( t.block )

	elseif t.type == "RepeatLoop" then
		return "repeat " .. serializeBlock( t.block ) .. "\nuntil " .. serializeExpression( t.condition ) .. ";"

	elseif t.type == "DoStatement" then
		return "do " .. serializeBlock( t.block )

	elseif t.type == "ForLoop" then
		return "for (" .. serializeType( t.init_class ) .. " " .. t.init_name .. (t.init_value and " = " .. serializeExpression( t.init_value ) or "") .. "; " .. serializeExpression( t.test ) .. "; " .. serializeExpression( t.increment ) .. ") " .. serializeBlock( t.block )

	elseif t.type == "ForeachLoop" then
		return "foreach " .. (t.name1 and t.name1 .. ", " .. t.name2 or t.name2) .. " in " .. serializeExpression( t.expression ) .. " " .. serializeBlock( t.block )

	elseif t.type == "SwitchStatement" then
		local c = {}

		for i = 1, #t.cases do
			c[i] = "\n" .. "case " .. serializeExpression( t.cases[i].case ) .. " " .. serializeBlock( t.cases[i].block )
		end

		return "switch " .. serializeExpression( t.expression ) .. ";" .. table.concat( c ) .. (t.default and "\ndefault " .. serializeBlock( t.default ) or "")

	elseif t.type == "TryStatement" then
		local c = {}

		for i = 1, #t.catches do
			c[i] = "\n" .. "catch " .. serializeType( t.catches[i].class ) .. " " .. t.catches[i].name .. " " .. serializeBlock( t.catches[i].block )
		end

		return "try " .. serializeBlock( t.block ) .. table.concat( c ) .. (t.default and "\ndefault " .. serializeBlock( t.default ) or "")

	elseif t.type == "ReturnStatement" then
		return "return " .. serializeExpression( t.value ) .. ";"

	elseif t.type == "BreakStatement" then
		return "break;"

	elseif t.type == "ContinueStatement" then
		return "continue;"

	elseif t.type == "Definition" or t.type == "TemplateDefinition" then
		return serializeDefinition( t )

	elseif t.type == "ExpressionStatement" then
		return serializeExpression( t.value ) .. ";"

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end

end

function compileStatement( emitter, t )

	if t.type == "IfStatement" then
		emitter:pushWord "if"
		compileExpression( emitter, t.condition )
		emitter:pushWord "then"
		compileBlock( emitter, t.block )

		if t.elseblock then
			if #t.elseblock == 1 and t.elseblock[1].type == "IfStatement" then
				emitter:pushLineBreak()
				emitter:pushSymbol "else"
				return compileStatement( emitter, t.elseblock[1] )
			else
				emitter:pushWord "else"
				compileBlock( emitter, t.elseblock )
			end
		end

		emitter:pushLineBreak()
		emitter:pushWord "end"

	elseif t.type == "WhileLoop" then
		emitter:pushWord "while"
		compileExpression( emitter, t.condition )
		emitter:pushWord "do"
		compileBlock( emitter, t.block )
		emitter:pushLineBreak()
		emitter:pushWord "end"

	elseif t.type == "RepeatLoop" then
		emitter:pushWord "repeat"
		compileBlock( emitter, t.block )
		emitter:pushLineBreak()
		emitter:pushWord "until"
		compileExpression( emitter, t.condition )
		emitter:pushSymbol ";"

	elseif t.type == "DoStatement" then
		emitter:pushWord "do"
		compileBlock( emitter, t.block )
		emitter:pushLineBreak()
		emitter:pushWord "end"

	elseif t.type == "ForLoop" then
		emitter:define( t.init_name )
		emitter:pushWord "local"
		emitter:pushWord( t.init_name )
		emitter:pushOperator "="

		compileExpression( emitter, t.init_value )

		emitter:pushLineBreak()
		emitter:pushWord "while"

		compileExpression( emitter, t.test )

		emitter:pushWord "do"

		compileBlock( emitter, t.block )

		emitter:indent( 1 )
		emitter:pushLineBreak()

		compileExpressionStatement( emitter, t.increment )

		emitter:indent( -1 )
		emitter:pushLineBreak()
		emitter:pushWord "end"

	elseif t.type == "ForeachLoop" then

		if t.name1 == "k" then
			emitter:pushWord "for"
			emitter:pushWord "k"
			emitter:pushDelimiter ","
			emitter:pushWord( t.name2 )
			emitter:pushWord "in"
			emitter:pushWord "pairs"
			emitter:pushSymbol "( "

			compileExpression( emitter, t.expression )

			emitter:pushSymbol " )"
			emitter:pushWord "do"

			compileBlock( emitter, t.block )
		else
			local name = t.expression.type == "Reference" and t.expression.name or emitter:getName()
			local name1 = t.name1 or emitter:getName()

			if t.expression.type ~= "Reference" then
				emitter:pushWord "local"
				emitter:pushWord( name )
				emitter:pushOperator "="

				compileExpression( emitter, t.expression )
			end

			emitter:pushLineBreak()
			emitter:pushWord "for"
			emitter:pushWord( name1 )
			emitter:pushOperator "="
			emitter:pushWord "1"
			emitter:pushDelimiter ","
			emitter:pushSymbol "#"
			emitter:pushWord( name )
			emitter:pushWord "do"
			emitter:indent( 1 )
			emitter:pushLineBreak()
			emitter:pushWord "local"
			emitter:pushWord( t.name2 )
			emitter:pushOperator "="
			emitter:pushSymbol( name .. "[" .. name1 .. "]" )
			emitter:indent( -1 )

			compileBlock( emitter, t.block )

		end

		emitter:pushLineBreak()
		emitter:pushWord "end"

	elseif t.type == "SwitchStatement" then
		local name = emitter:getName()

		emitter:pushWord "local"
		emitter:pushWord( name )
		emitter:pushOperator "="

		compileExpression( emitter, t.expression )

		emitter:pushLineBreak()

		emitter:pushWord "if"
		emitter:pushWord( name )
		emitter:pushOperator "=="

		compileExpression( emitter, t.cases[1].case )

		emitter:pushWord "then"

		compileBlock( emitter, t.cases[1].block )

		emitter:pushLineBreak()

		for i = 2, #t.cases do

			emitter:pushWord "elseif"
			emitter:pushWord( name )
			emitter:pushOperator "=="

			compileExpression( emitter, t.cases[i].case )

			emitter:pushWord "then"

			compileBlock( emitter, t.cases[i].block )

			emitter:pushLineBreak()

		end

		if t.default then
			emitter:pushWord "else"

			compileBlock( emitter, t.default )

			emitter:pushLineBreak()

		end

		emitter:pushWord "end"

	elseif t.type == "TryStatement" then
		local nameOK = emitter:getName()
		local nameErr = emitter:getName()
		local nameErrName = emitter:getName()

		emitter:pushBlockText( "local " .. nameOK .. ", " .. nameErr .. " = pcall( function()" )

		compileBlock( emitter, t.block )

		emitter:pushLineBreak()
		emitter:pushWord "end"
		emitter:pushSymbol " )"
		emitter:pushLineBreak()
		emitter:pushBlockText( "local " .. nameErrName .. " = not " .. nameOK .. " and " .. nameErr .. ":match '^(.-):' or false" )
		emitter:pushLineBreak()
		emitter:pushWord "if"
		emitter:pushWord( nameErrName )
		emitter:pushOperator "=="
		emitter:pushString( t.catches[1].class.name )
		emitter:pushWord "then"
		emitter:indent( 1 )
		emitter:pushLineBreak()
		emitter:pushWord "local"
		emitter:pushWord( t.catches[1].name )
		emitter:pushOperator "="
		emitter:pushBlockText( nameErr .. ":match ':(.+)$' or ''" )
		emitter:indent( -1 )

		compileBlock( emitter, t.catches[1].block )

		emitter:pushLineBreak()

		for i = 2, #t.catches do
			emitter:pushWord "elseif"
			emitter:pushWord( nameErrName )
			emitter:pushOperator "=="
			emitter:pushString( t.catches[i].class.name )
			emitter:pushWord "then"
			emitter:indent( 1 )
			emitter:pushLineBreak()
			emitter:pushWord "local"
			emitter:pushWord( t.catches[i].name )
			emitter:pushOperator "="
			emitter:pushBlockText( nameErr .. ":match ':(.+)$' or ''" )
			emitter:indent( -1 )

			compileBlock( emitter, t.catches[i].block )

			emitter:pushLineBreak()
		end

		if t.default then
			emitter:pushBlockText( "elseif not " .. nameOK .. " then" )

			compileBlock( emitter, t.default )

			emitter:pushLineBreak()
		else
			emitter:pushBlockText( "elseif not " .. nameOK .. " then\n\terror( " .. nameErr .. ", 0 )\n" )

		end

		return emitter:pushWord "end"

	elseif t.type == "ReturnStatement" then
		emitter:pushWord "return"

		compileExpression( emitter, t.value )

	elseif t.type == "BreakStatement" then
		emitter:pushWord "break"

	elseif t.type == "ContinueStatement" then
		emitter:pushWord "continue"

	elseif t.type == "Definition" or t.type == "TemplateDefinition" then
		return compileDefinition( emitter, t )

	elseif t.type == "ExpressionStatement" then
		return compileExpressionStatement( emitter, t )

	else
		emitter:push( "<compilation of " .. t.type .. " isn't written>" )

	end

end
