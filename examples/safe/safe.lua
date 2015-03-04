-- LuaMoulds example: safe.lua
local moulds = require("moulds")

-- create a mould set:
local ms = moulds()

-- add a type to the mould set:
ms:typedef([[
typedef struct {
   boolean  b
   int      i
} mytype
]])

-- create an instance:
local x = ms:new('mytype')

-- safely write values in the terminal fields:
ms:tset(x, "b", true)
ms:tset(x, "i", 10)

ms:print(x)

-- this should cause an error, because the value is not
-- valid for the field's type:
ms:tset(x, "i", true)

