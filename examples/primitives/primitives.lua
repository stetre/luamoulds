-- LuaMoulds example: primitives.lua
local moulds = require("moulds")

-- create a mould set:
local ms = moulds()

-- create an instance of the primitive type 'char':
local c = ms:new('char')

-- set its value:
ms:pset(c, 123)
ms:print(c)       --> char 123
print(ms:pget(c)) --> 123

-- reset its value:
ms:pset(c)
ms:print(c)       --> char ?
print(ms:pget(c)) --> ?
print(ms:pget(c) == ms:np()) --> true

-- this would cause an error because the value is out of range:
-- ms:pset(c, 300)

-- define a custom primitive type:
ms:primitive("mynumber", 
              function(x) return type(x)=="number" and x>=0 and x<1 end)

local n = ms:new('mynumber')
ms:pset(n, 0.5)
ms:print(n)
print(ms:pget(n))

-- this would cause an error because the value is out of range:
-- ms:pset(n, 2)

