-- LuaMoulds example: basic.lua
local moulds = require("moulds")

-- create a mould set:
local ms = moulds()

-- prepare a string with some type definitions:
local mydefs = [[
-- these are just aliases of primitive types:
typedef char mychar
typedef int myint

-- a structured type: 
typedef struct {
   boolean  b
   myint    i
   mychar   c[6]
   double   d
} mystruct
]]

-- add the types to the mould set:
ms:typedef(mydefs)

-- create an instance of mystruct:
local x = ms:new('mystruct')

-- some prints...
ms:print(x)       --> mystruct ? ? ? ? ? ? ? ? ?
ms:print(x,'c')   --> mystruct.c ? ? ? ? ? ?
print(ms:sizeof(x))    --> 9 2 10 struct{ ... }
print(#x)     --> 10 (the size + 1 for the type name)

-- write some fields:
ms:set(x, 'b', true)
ms:set(x, 'i', 123)
ms:set(x, 'c', 10, 20, 30)
ms:set(x, 'd', 1.0e20)

-- read some fields:
print(ms:get(x, '*'))   --> mystruct true 123 10 20 30 ? ? ? 1e+20
print(ms:get(x, 'b'))   --> true
print(ms:get(x, 'i'))   --> 123
print(ms:get(x, 'c.1')) --> 10
print(ms:get(x, 'c.6')) --> ?
print(ms:get(x, 'c'))   --> 10 20 30 ? ? ?

