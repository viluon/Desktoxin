 -- Warning: this needs to be tidied up!
 -- Flux doesn't generate the nicest, most optimised outputs just yet :/
Position = { class = ' Position ' }
function Position:new(...)
	local obj = setmetatable( {}, {__index = self} )
	obj:Position(...)
	return obj
end

Position.Position = function(self, source, line, character, strline)
	self.source = source;
	self.line = line;
	self.character = character;
	self.strline = strline;
	return self
end;

Position.get_line_ptr = function(self)
	return self.strline:gsub("\9", " ") .. "\
" .. (" "):rep(self.character - 1) .. "^"
end;

Position.get_src_pos = function(self)
	return self.source .. "[" .. self.line .. "]"
end;

local function getTokenTypeString()end

function getTokenTypeString(type)
	return(function(a)
		if a == nil then
			return"null"
		else
			return type
		end
	end)(type)
end

Token = { class = ' Token ' }
function Token:new(...)
	local obj = setmetatable( {}, {__index = self} )
	obj:Token(...)
	return obj
end

Token.Token = function(self, type, value, position)
	self.type = type;
	self.value = value;
	self.position = position;
	return self
end;

Token.EOF = function(self, position)
	return Token:new(TokenType.EOF, "EOF", position)
end;

Token.tostring = function(self)
	return getTokenTypeString(self.type) .. " <" .. self.value .. ">"
end;

Token.get_near_string = function(self)
	return(function(b)
		if b == TokenType.String then
			return"<string>"
		elseif b == TokenType.EOF then
			return"<eof>"
		else
			return self:tostring()
		end
	end)(self.type)
end;

TokenType = {
	String = "String";
	Character = "Character";
	Backtick = "Backtick";
	Float = "Float";
	Integer = "Integer";
	Byte = "Byte";
	Hexadecimal = "Hexadecimal";
	Binary = "Binary";
	Boolean = "Boolean";
	Null = "Null";
	Symbol = "Symbol";
	Identifier = "Identifier";
	Keyword = "Keyword";
	Instruction = "Instruction";
	EOF = "EOF";
}

local keywords = {
	["import"] = true;
	["let"] = true;
	["define"] = true;
};

local symbols = {
	["("] = true;
	[")"] = true;
	["{"] = true;
	["}"] = true;
	["["] = true;
	["]"] = true;
	["."] = true;
	[":"] = true;
	[","] = true;
	[";"] = true;
	["="] = true;
	["+"] = true;
	["-"] = true;
};

local function getline()end

local escaped_characters = setmetatable({
	["n"] = "\
";
	["0"] = "\0";
	["r"] = "\13";
	["t"] = "\9";
}, {
	["__index"] = function(_, char)
		return"\\" .. char
	end;
});

Lexer = { class = ' Lexer ' }
function Lexer:new(...)
	local obj = setmetatable( {}, {__index = self} )
	obj:Lexer(...)
	return obj
end

Lexer.position = 1;

Lexer.text = "";

Lexer.source_name = "string";

Lexer.character = 1;

Lexer.line = 1;

Lexer.strline = "";

Lexer.buffer_position = 1;

Lexer.Lexer = function(self, text, source_name)
	self.source_name = source_name;
	self.text = text;
	self.buffer = {};
	self.mark_stack = {};
	self.macros = {};
	return self
end;

function getline(text, line)
	local i = 1
	while i <= line - 1 do
		text = text:gsub("^.-\
", "");
		i = i + 1;
	end
	return text:gsub("\
.*$", "")
end

Lexer.getPosition = function(self, n)
	if n == nil then
		n = 0;
	end
	local character = self.character;
	local c = self
	c.character = c.character + n;
	return Position:new(self.source_name, self.line, character, self.strline)
end;

Lexer.isEOF = function(self)
	while true do
		if self.text:sub(self.position, self.position) == "\
"then
			self:skipNewline();
		elseif self.text:sub(self.position, self.position + 1) == "//"or self.text:sub(self.position, self.position + 1) == "/*"then
			self:skipComment();
		elseif self.text:find("^%s", self.position)then
			self:skipWhitespace();else
			break
		end
	end
	return self.position > #self.text
end;

Lexer.error = function(self, err, position)
	if position == nil then
		position = self:getPosition();
	end
	return error( 'LexerException:' .. position:get_src_pos() .. ": " .. err .. "\
" .. position:get_line_ptr(), 0 )
end;

Lexer.consumePattern = function(self, type, pattern)
	local value = string["match"](self.text, "^" .. pattern, self.position);
	if not value then
		return nil
	end
	local d = self
	d.position = d.position + #value;
	self.token_line = self.line;
	return Token:new(type, value, self:getPosition(#value))
end;

Lexer.consumeString = function(self)
	local s = {};
	local pos = self:getPosition(1);
	local opening = self.text:sub(self.position, self.position);
	local i = self.position + 1
	while i <= #self.text do
		local char = self.text:sub(i, i);
		local e = self
		e.character = e.character + 1;
		if char == "\\"then
			i = i + 1;
			local char = self.text:sub(i, i);
			s[#s + 1] = escaped_characters[char];
			if char == "\
"then
				local f = self
				f.line = f.line + 1;
				self.strline = getline(self.text, self.line);
				self.character = 1;
			end
		elseif char == "\
"then
			s[#s + 1] = "\
";
			local g = self
			g.line = g.line + 1;
			self.strline = getline(self.text, self.line);
			self.character = 1;
		elseif char == opening then
			self.position = i + 1;
			self.token_line = self.line;
			self.buffer[#self.buffer + 1] = Token:new(TokenType.String, table.concat(s), pos);
			return nil else
			s[#s + 1] = char;
		end
		i = i + 1;
	end
	return self:error("missing end of string", pos)
end;

Lexer.consumeWord = function(self)
	local token = self:consumePattern(TokenType.Identifier, "[%w_]+");
	if self.macros[token.value]then
		local h = self.macros[token.value]
		for i = 1, #h do
			local v = h[i]
			table.insert(self.buffer, self.buffer_position + i - 1, v);
		end else
		if keywords[token.value]then
			token.type = TokenType.Keyword;
		elseif token.value == "true"or token.value == "false"then
			token.type = TokenType.Boolean;
		end
		self.buffer[#self.buffer + 1] = token;
	end
end;

Lexer.consumeNumber = function(self)
	self.buffer[#self.buffer + 1] = self:consumePattern(TokenType.Float, "%d*%.%d+e[%+%-]?%d+")or self:consumePattern(TokenType.Float, "%d*%.%d+")or self:consumePattern(TokenType.Byte, "%d+e[%+%-]?%d+b")or self:consumePattern(TokenType.Byte, "%d+b")or self:consumePattern(TokenType.Integer, "%d+e[%+%-]?%d+")or self:consumePattern(TokenType.Integer, "%d+")or self:error("Something bad has happened. This is bad :/");
end;

Lexer.consumeHexadecimal = function(self)
	self.buffer[#self.buffer + 1] = self:consumePattern(TokenType.Hexadecimal, "0x%x+")or self:error("Crap.");
end;

Lexer.consumeBinary = function(self)
	self.buffer[#self.buffer + 1] = self:consumePattern(TokenType.Binary, "0b[01]+")or self:error("Crap.");
end;

Lexer.consumeSymbol = function(self)
	local position = self.position;
	local s3 = self.text:sub(position, position + 2);
	local s2 = self.text:sub(position, position + 1);
	local s1 = self.text:sub(position, position);
	self.token_line = self.line;
	if symbols[s3]then
		local j = self
		j.position = j.position + 3;
		self.buffer[#self.buffer + 1] = Token:new(TokenType.Symbol, s3, self:getPosition(3));
	elseif symbols[s2]then
		local k = self
		k.position = k.position + 2;
		self.buffer[#self.buffer + 1] = Token:new(TokenType.Symbol, s2, self:getPosition(2));
	elseif symbols[s1]then
		local l = self
		l.position = l.position + 1;
		self.buffer[#self.buffer + 1] = Token:new(TokenType.Symbol, s1, self:getPosition(1));else
		return self:error("unexpected symbol '" .. s1 .. "'")
	end
end;

Lexer.define_macro = function(self, name, tokens)
	self.macros[name] = tokens;
end;

Lexer.skipWhitespace = function(self)
	local len = #string["match"](self.text, "[^%S\
]+", self.position);
	local m = self
	m.character = m.character + len;
	local n = self
	n.position = n.position + len;
end;

Lexer.skipComment = function(self)
	if self.text:sub(self.position, self.position + 1) == "//"then
		local o = self
		o.line = o.line + 1;
		self.strline = getline(self.text, self.line);
		self.character = 1;
		local p = self
		p.position = p.position + #(string["match"](self.text, "^.-\
", self.position)or self.text:sub(self.position));else
		local finish = self.text:find("%*/", self.position);
		local newlines = finish and select(2, self.text:sub(self.position, finish):gsub("\
", ""));
		if finish then
			local q = self
			q.line = q.line + newlines;
			self.strline = getline(self.text, self.line);
			self.character = newlines > 0 and#self.text:sub(self.position, finish):gsub(".+\
", "") + 2 or self.character + finish - self.position + 2;
			self.position = finish + 2;else
			return self:error("missing end of comment '*/'")
		end
	end
end;

Lexer.skipNewline = function(self)
	local r = self
	r.line = r.line + 1;
	self.character = 1;
	local t = self
	t.position = t.position + 1;
	self.strline = getline(self.text, self.line);
end;

Lexer.skipAllWhitespace = function(self)
	while true do
		if self.text:find("^\
", self.position)then
			self:skipNewline();
		elseif self.text:sub(self.position, self.position + 1) == "//"or self.text:sub(self.position, self.position + 1) == "/*"then
			self:skipComment();
		elseif self.text:find("^%s", self.position)then
			self:skipWhitespace();else
			break
		end
	end
end;

Lexer.consume = function(self)
	self:skipAllWhitespace();
	if self.position > #self.text then
		self.buffer[#self.buffer + 1] = Token.EOF(self:getPosition());
	elseif self.text:find("^@[%w_%-]", self.position)then
		self:consumeInstruction();
	elseif self.text:find("^[\"']", self.position)then
		self:consumeString();
	elseif self.text:find("^0x%x+", self.position)then
		self:consumeHexadecimal();
	elseif self.text:find("^0b[01]+", self.position)then
		self:consumeBinary();
	elseif self.text:find("^%d+b?%W", self.position)or self.text:find("^%d+b?$", self.position)or self.text:find("^%d+e[%+%-]?%d+b?%W", self.position)or self.text:find("^%d+e[%+%-]?%d+b?$", self.position)or self.text:find("^%d*%.%d+%W", self.position)or self.text:find("^%d*%.%d+$", self.position)or self.text:find("^%d*%.%d+e[%+%-]?%d+%W", self.position)or self.text:find("^%d*%.%d+e[%+%-]?%d+$", self.position)then
		self:consumeNumber();
	elseif self.text:find("^[%w_]+", self.position)then
		self:consumeWord();else
		self:consumeSymbol();
	end
end;

Lexer.mark = function(self)
	self.mark_stack[#self.mark_stack + 1] = self.buffer_position;
end;

Lexer.back = function(self)
	self.buffer_position = table.remove(self.mark_stack, #self.mark_stack);
	while#self.mark_stack == 0 and self.buffer_position > 1 do
		table.remove(self.buffer, 1);
		local u = self
		u.buffer_position = u.buffer_position - 1;
	end
end;

Lexer.pop_mark = function(self)
	self.mark_stack[#self.mark_stack] = nil;
	while#self.mark_stack == 0 and self.buffer_position > 1 do
		table.remove(self.buffer, 1);
		local v = self
		v.buffer_position = v.buffer_position - 1;
	end
end;

Lexer.get = function(self, position)
	if position == nil then
		position = self.buffer_position;
	end
	local buffer = self.buffer;
	while not buffer[position]do
		self:consume();
	end
	return buffer[position]
end;

Lexer.peek = function(self, lookahead)
	if lookahead == nil then
		lookahead = 1;
	end
	return self:get(self.buffer_position + lookahead)
end;

Lexer.next = function(self)
	local t = self:get(self.buffer_position);
	local w = self
	w.buffer_position = w.buffer_position + 1;
	while#self.mark_stack == 0 and self.buffer_position > 1 do
		table.remove(self.buffer, 1);
		local x = self
		x.buffer_position = x.buffer_position - 1;
	end
	return t
end;

Lexer.next_same_line = function(self)
	self:skipAllWhitespace();
	return self.token_line == self.line
end;

Lexer.test = function(self, type, value, lookahead)
	if lookahead == nil then
		lookahead = 0;
	end
	local token = self:peek(lookahead);
	return token.type == type and(not value or token.value == value)and token or nil
end;

Lexer.skip = function(self, type, value)
	local token = self:get();
	return token.type == type and(not value or token.value == value)and self:next()or nil
end;

Lexer.testValue = function(self, type, value, lookahead)
	local token = self:test(type, value, lookahead);
	return token and token.value or nil
end;

Lexer.skipValue = function(self, type, value)
	local token = self:skip(type, value);
	return token and token.value or nil
end;

local function parse_error()end

local function parse()end

function error(position, err)
	return error( 'ParserException:' .. position:get_src_pos() .. ": " .. err .. "\
" .. position:get_line_ptr(), 0 )
end

function parse(lexer)
	while not lexer:isEOF()do
		if lexer:skip(TokenType.Keyword, "define")then
			local name = lexer:skipValue(TokenType.Identifier)or parse_error(lexer:get().position, "expected macro name after 'define'");
			local list = {};
			while not lexer:isEOF()and lexer:next_same_line()do
				list[#list + 1] = lexer:next();
			end
			lexer:define_macro(name, list);
		elseif lexer:skip(TokenType.Keyword, "let")then
			local name = lexer:skipValue(TokenType.Identifier)or parse_error(lexer:get().position, "expected macro name after 'let'");
			local list = {};
			if not lexer:skipValue(TokenType.Symbol)then
				parse_error(lexer:get().position, "expected '=' after macro name");
			end
			while not lexer:isEOF()and lexer:next_same_line()do
				list[#list + 1] = lexer:next();
			end
			lexer:define_macro(name, list);else
			print(lexer:next():tostring());
		end
	end
end

local code = "\
define dark_button dark button\
let red = rgb(255, 0, 0)\
let blue = rgb(0, 0, 255)\
\
dark_button x {\
\9text = \"hi\"\
\9colour = blue\
}\
";

function main()
	local lexer = Lexer:new(code, "src");
	parse(lexer);
end

