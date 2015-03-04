-- LuaMoulds example: matrix.lua
local moulds = require("moulds")

local ms = moulds()

local nrows = 3 -- number of rows
local ncols = 4 -- number of columns

-- define the type 'nrows x ncols matrix of strings':
ms:typedef(string.format("typedef string matrix[%u][%u]",ncols,nrows))

-- instantiate a matrix:
local M = ms:new('matrix')

-- write in each element a string with its indices:
for i = 1, nrows do 
   for j = 1, ncols do
      ms:tset(M,string.format('%s.%s',i,j),string.format('(%u,%u)',i,j))
   end
end

ms:print(M)

-- write something in M(2,3):
ms:tset(M,'2.3', "hello!")

ms:print(M)

