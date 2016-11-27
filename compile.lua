
local Flux = require "Flux"

local source = Source "."
local emitter = Emitter()

if source.hasMainFile then
	source:import "main"
else
	error( "path '.' has no main file", 0 )
end

for i = 1, #source.statements do
	compileRootStatement( emitter, source.statements[i] )
	emitter:pushLineBreak()
	emitter:pushLineBreak()
end

local h = io.open( "desktoxin_parser.lua", "w" )
if h then
	h:write " -- Warning: this needs to be tidied up!\n"
	h:write " -- Flux doesn't generate the nicest, most optimised outputs just yet :/\n"
	h:write( emitter.output )
	h:close()
else
	error( "Failed to open 'desktoxin_parser.lua' to write", 0 )
end

assert( (loadstring or load)( emitter.output ) )()
main()
