-- LuaMoulds example: fromfile.lua
local moulds = require("moulds")

-- create a mould set:
local ms = moulds()

-- add the types to the mould set:
ms:ftypedef("definitions.lua")

-- create an instance of mytype:
local x = ms:new('mytype')

ms:set(x, "e2", 12, 34)
ms:print(x)
ms:print(x, "e2")
ms:print(x, "e2.a")
ms:print(x, "e2.b")
ms:print(x, "e2.c")

-- copy field e2 to field e1
ms:set(x, "e1", ms:get(x, "e2"))
ms:print(x)
ms:print(x, "e1")
