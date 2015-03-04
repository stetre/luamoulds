--=============================================================================
-- LuaMoulds - moulds for structured weak-typed sequences
--=============================================================================

local lpeg = require("lpeg")
local re = require("re")
local P = lpeg.P
local V = lpeg.V
local S = lpeg.S
local R = lpeg.R
local Cs = lpeg.Cs
local Ct = lpeg.Ct
local C = lpeg.C

local moulds = {}
moulds._VERSION = "LuaMoulds v0.1"
moulds.__index = moulds

function moulds.version(self) return self._VERSION end

--------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------

local function notraceback(m) return m end -- replace debug.traceback

local function assertf(level, condition, ...)
   if condition then return condition end
   local errmsg = next({...}) and string.format(...) or "assertf failed"
   error(errmsg,level+1)
end

local fmt = string.format

function deepprint(t, nonl)
-- deep print of a table (for debugs)
   local b=false
   io.write("{")
   for k, v in pairs(t) do
      if b then io.write(", ") else b = true end
      if type(v)=="table" then
         deepprint(v,true)
      else
         io.write(fmt("%s",v))
      end
   end
   io.write("}")
   if not nonl then io.write("\n") end
end


local function truncate(x,n)
-- truncate a long value (for prints)
   local x = tostring(x)
   return x:len() > n and x:sub(1,n) .." ..." or x
end


--------------------------------------------------------------------------
-- Mouldset creator
--------------------------------------------------------------------------

local check_number
local check_boolean
local check_string
local check_table
local check_thread
local check_userdata
local check_function
local check_bit
local check_uchar
local check_char
local check_ushort
local check_short
local check_uint
local check_int
local check_ulong
local check_long
local check_float
local check_double
local check_bitstr
local check_hexstr

local defineprimitives

local function new(np)
-- creates a moulds database
   local np = np or '?'
   assertf(2,type(np)=="string", "invalid value '%s' for 'not present' (must be a string)", np)
   local self = {}
   setmetatable(self, moulds)
   self.NP = np
   self.T = {} -- types database, indexed by typename
   defineprimitives(self)
   return self
end


function moulds.primitive(self, name, checkfunc) 
   assertf(2, not self.T[name], "duplicated type '%s'", name)
   assertf(2, type(checkfunc)=="function", "missing check function")
   self.T[name] = { 1, 0, name, checkfunc, name }
end


function moulds.np(self) return self.NP end

defineprimitives = function(self)
   self.T["void"] = { 0, 0, "void", nil, "void" }
   self:primitive("number", check_number)
   self:primitive("boolean", check_boolean)
   self:primitive("string", check_string)
   self:primitive("table", check_table)
   self:primitive("function", check_function)
   self:primitive("thread", check_thread)
   self:primitive("userdata", check_userdata)
   self:primitive("bit", check_bit)
   self:primitive("char", check_char)
   self:primitive("uchar", check_uchar)
   self:primitive("short", check_short)
   self:primitive("ushort", check_ushort)
   self:primitive("int", check_int)
   self:primitive("uint", check_uint)
   self:primitive("long", check_long)
   self:primitive("ulong", check_ulong)
   self:primitive("float", check_float)
   self:primitive("double", check_double)
   self:primitive("bitstr", check_bitstr)
   self:primitive("hexstr", check_hexstr)
end

-------------------------------------------------------------------------------
-- Comments remover
-------------------------------------------------------------------------------

-- Removes C style comments (/* ... */)
local ansiccomments = Cs ( (re.compile([[
   S <- '/*' <C>
   C <- '*/' / . <C>]])/'' + 1 )^1 )

-- Removes C preprocessor directives (# ... )
local cpreprocessor = Cs ( (re.compile([[
   S <- %nl '#' <C>
   C <- %nl / . <C>]]
)/'' + 1 )^1 )

-- Removes C++ style comments (// ... )
local cppcomments = Cs( (re.compile([[
   S <- '//' <C>
   C <- %nl / . <C>]])/'\n' + 1 )^1 )

-- Removes Lua-style one line comments (-- ... )
local luacomments = Cs( (re.compile([[
   S <- '--' <C>
   C <- %nl / . <C>]])/'\n' + 1 )^1 )

local compressnl = Cs( (re.compile("(%nl)+")/'\n' + 1 )^1 )

local function stripcomments(s)
-- strips ANSI C comments and one-line Lua comments 
   local s = '\n' .. s
   s = ansiccomments:match(s)
   s = cpreprocessor:match(s)
   s = cppcomments:match(s)
   s = luacomments:match(s)
   return compressnl:match(s)
end

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- whitespaces:
local SP = S(' \t\n')
local SP0 = SP^0 -- SP*
local SP1 = SP^1 -- SP+
-- fields separator(s):
local SEP = (SP0 * (P',' + P';') * SP0) + SP1

local ALPHA = R('az','AZ') + P'_'
local ALPHANUM = ALPHA + R('09')
local IDENTIFIER = ALPHA * ALPHANUM^0
local NAME = IDENTIFIER
local TYPE = IDENTIFIER
local TYPEDEF = P'typedef'
local VOID = P'void' * #SP
local STRUCT = P'struct'
local UNION = P'union'

-- Splits a string containing multiple typedefs in one string per typedef:
local splittypedefs = 
Ct( (re.compile([[
S <- 'typedef' <C>
C <- &'typedef' / !. / . <C>]])/'%0' + SP^1)^1 )

local function Size(x) 
   local n = tonumber(x)
   if not n or n < 1 or (n%1~=0) then error(fmt("invalid array size '%s'",x),4) end
   return n
end

-- Parses a string containing one typedef, and builds the intermediate table:
local typedefpattern = P{
   "typedecl", 
   typedecl  = TYPEDEF * SP1 * V'field' * SEP^-1 * -P(1),
   field     = Ct(V'basetype' * SP1 * (NAME/'%0') * V'array'),
   array     = Ct(( SP0 * '[' * ((1 -S'[]')^1/Size) * ']')^0),
   basetype  = VOID/'%0' + Ct(V'structorunion') + TYPE/'%0',
   structorunion = (STRUCT + UNION)/'%0' * SP0 * V'fieldlist',
   fieldlist = '{' * SP0 * V'field' * (SEP * V'field')^0 * SEP^-1 * '}',
}

-------------------------------------------------------------------------------
-- Typedef
-------------------------------------------------------------------------------

local function sizeof(self, typename)
   local td = self.T[typename]
   assertf(2, td, fmt("unknown type '%s'",typename))
   return td[1]
end

local function prepend_dot(name)
   if name:sub(1,1) ~= "." then return "."..name else return name end
end 

local function fullname(typename, fieldname)
   local name = fieldname and typename .. '.' .. fieldname or typename
   if name:sub(1,1)=='.' then return name:sub(2) else return name end
end 

local function kindof(kind)
   if kind==0 then return "terminal"
   elseif kind == 1 then return "array"
   elseif kind == 2 then return "struct"
   elseif kind == 3 then return "union"
   end
   error("invalid kind=%s",kind)
end

---------------------------------------------------
-- TYPE DESCRIPTOR
-- 
-- self.T[typename] = {   -- 'self' is a mould set
--    [1] = size (no. of terminal fields)
--    [2] = kind (0=primitive, 1=array, 2 =struct, 3=union)
--    [3] = N (array size, for arrays)
--          { fieldname1, ..., fieldnameN } (for structs and unions)
--          primitive typename (for terminals)
--    [4] = check function (for primitives only)
--    [5] = description (typedef string)
--    For arrays only:
--    ["basetype"] = base_typename
--    For unions and structs only:
--    [fieldname1] = { field_typename, position }
--       ...
--    [fieldnameN] = { field_typename, position }
--    }
--
--    'position' is the index (starting from 1) where the field begins
--    within the struct or union
--
-- intermediate_table = 
-- { typename name {N1, ... Nn} } or
-- { {struct {field1} {field2} ... {fieldN}} name {N1, ... Nn} } or
-- { {union {field1} {field2} ... {fieldN}} name {N1, ... Nn} } or
--
-- fieldK = intermediate_table
-----------------------------------------------------

local function description(t, s)
-- creates a string describing a type, from the intermediate table t
   local s = s or ""
   local typ, name, arr = t[1], t[2], t[3]
   if type(typ) == "string" then
      s = s .. typ
   else -- type(typ) == "table" then
      s = s .. typ[1] .. '{ ' -- "struct" or "union"
      for i=2,#typ do
         s = s .. description(typ[i])
      end
      s = s .. '}' 
   end
-- s = s .. ' ' .. name
   if #arr > 0 then
      for i = 1,#arr do
         s = s .. fmt("[%s]",arr[i])
      end
   end
   return s .. ' '
end

local function definetype(self, typ, name, arr)
-- creates the type entry in self.T
-- returns the type name
   assertf(2, not self.T[name], fmt("duplicated typedef of '%s'",name))

   -- array ----------------------------------------------------
   if #arr > 0 then
      local n = arr[#arr]
      local td = {}
      td[2] = 1 -- "array"
      td[3] = n
      td[5] = description({typ, name, arr})
      arr[#arr]=nil
      if (#arr == 0) and type(typ)=="string" then
         local td1 = self.T[typ]
         assertf(2, td1, "unknown type '%s'",typ)
         td["basetype"] = typ
         td[1] = sizeof(self, typ)*n
      else
         --local bname =  name .. "_1"
         local bname =  name .. ".@"
         bname = prepend_dot(bname)
         definetype(self, typ, bname, arr)
         td["basetype"] = bname
         td[1] = sizeof(self, bname) * n
      end
      self.T[name] = td
      return name
   end

   -- alias -------------------------------------------------------
   if type(typ)=="string" then 
      local td = self.T[typ] 
      assertf(2, td, "unknown type '%s'",typ)
      self.T[name] = td
      return name
   end

   -- struct or union ---------------------------------------------
   local flist ={} -- create the field list
   local td = {}
   td[5] = description({typ, name, arr})
   for i=2,#typ do
      local tt = typ[i]
      local fname = tt[2]
      assertf(2, not td[fname], "duplicate field '%s'",fname)
      flist[#flist+1] = fname
      local tname =  name .."."..fname
      tname = prepend_dot(tname)
      td[fname] = { definetype(self, tt[1], tname, tt[3]) }
   end
   local kind = typ[1] == "struct" and 2 or 3
   td[2] = kind
   td[3] = flist
   -- compute size and fields positions
   if kind == 2 then --"struct"
      local pos = 0
      for _,f in ipairs(flist) do
         local sz = sizeof(self, td[f][1])
         td[f][2] = sz == 0 and pos or pos + 1
         pos = pos + sz
      end
      td[1] = pos
   elseif kind == 3 then -- kind == "union"
      local pos, maxsz = 0, 0
      for _,f in ipairs(flist) do
         local sz = sizeof(self, td[f][1])
         td[f][2] = sz == 0 and 0 or 1
         if sz > maxsz then maxsz = sz end
      end
      td[1] = maxsz
   else
      error("syntax error",2)
   end
   self.T[name] = td
   return name
end

local errlvl = 2

function moulds.ftypedef(self, filename)
-- defines types from a file
   local file, errmsg = io.open(filename,"r")
   if not file then error(errmsg,2) end
   local s = file:read("a")
   --print(s)
   errlvl = errlvl+1
   local names = { self:typedef(s) }
   errlvl = errlvl-1
   return table.unpack(names)
end

function moulds.typedef(self, defs)
   local defs = stripcomments(defs)
   -- separate the typedefs
   local defs1 = splittypedefs:match(defs)
   local names = {}
   for _,d in ipairs(defs1) do
      -- parse the definition and create the intermediate table
      local t = typedefpattern:match(d)
      if not t then error(fmt("syntax error in:\n%s\n",d),errlvl) end
      --deepprint(t)
      -- define the type:
      local ok, name = xpcall(function (...)
         return definetype(...)
      end, notraceback, self, t[1], t[2],t[3])
      if not ok then
         errmsg = name .. fmt(", in:\n%s\n",d)
         error(errmsg,errlvl) end
      names[#names+1] = name
   end
   assertf(errlvl, #names > 0, "no type definitions found")
   return table.unpack(names)
end


-------------------------------------------------------------------------------
-- Tostring functions
-------------------------------------------------------------------------------

function moulds.__tostring(self)
   local s = {}
   local ot = {} --ordered table

   for name, td in pairs(self.T) do ot[#ot+1] = { name, td } end
   table.sort(ot, function(x,y)
                     local a,b = x[1], y[1]
                     local a = a:sub(1,1) == '.' and a:sub(2) or a
                     local b = b:sub(1,1) == '.' and b:sub(2) or b
                     return a<b end)

   for _,t in ipairs(ot) do
      local name, td = t[1], t[2]
      local size, kind = td[1], td[2]
      if kind == 0 then
         s[#s+1] = fmt("%s (%s) %s", name,size, td[3])
      elseif kind == 1 then -- array then
         s[#s+1] = fmt("%s (%s) %s[%u]", name,size,td.basetype,td[3])
      else -- struct or union
         s[#s+1] = fmt("%s (%s) %s{%s}", name,size,kindof(kind),table.concat(td[3],","))
         for _,f in ipairs(td[3]) do
            local fd = td[f]
            s[#s+1] = fmt("  %4s %s (%s) %s",fd[2], f,sizeof(self,fd[1]),fd[1])
         end
      end
   end
   return table.concat(s,"\n")
end


local function concat(x, sep)
   local sep = sep or ' '
   local s = {}
   for k,v in ipairs(x) do s[k] = tostring(v) end
   return table.concat(s, sep)
end

function moulds.tostring(self, x, fieldname, sep)
   if not fieldname or fieldname == '*' then return concat(x, sep) end
   local size, first, last = self:sizeof(x, fieldname)
   local t = { x[1] .. '.' .. fieldname }
   for i=first,last do t[#t+1] = x[i] end
   return concat(t, sep)
end

function moulds.print(self, x, fieldname, sep)
   print(self:tostring(x, fieldname, sep))
end


--@@@@@@@@@@@ WORK IN PROGRESS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function moulds.terminals(self, typename, fieldname, basename, s, pos) --@@ 
-- builds the terminals table
-- s[1] = {}
-- s[pos] = {{fieldname, type}, ..., {fieldname,type}} (for unions)
   local td = self.T[typename]
   if not td then
      error(fmt("unknown type '%s'",typename),2)
   end
   local fieldname = fieldname or ""
   local fieldname = (basename and basename~="") and basename .. '.' .. fieldname or fieldname
   local s = s or { {} }
   local pos = pos or 1
   local size, kind = td[1], td[2]
   if kind == 0 then -- size is 0 or 1
      pos = size == 0 and pos or pos+1
      if size == 1 then
         if not s[pos] then s[pos] = {} end
         local ss = s[pos]
         ss[#ss+1] = { fieldname, td[3] }
--       s[fieldname] = { td[3], pos }
      end
      return s, pos
   end
   local bname = fieldname
   if kind == 1 then -- array
      local N = td[3]
      for i = 1,N do 
         s, pos = self:terminals(td.basetype, tostring(i), bname, s, pos) 
      end
      return s, pos
   end
   if kind == 2 then -- struct
      for _,f in ipairs(td[3]) do
         s, pos = self:terminals(td[f][1], f, bname, s, pos)
      end
   else -- union
      local posmax = 0
      local pos1
      for _,f in ipairs(td[3]) do
         s, pos1 = self:terminals(td[f][1], f, bname, s, pos)
         if pos1 > posmax then posmax = pos1 end
      end
      pos = posmax
   end
   return s, pos
end

function moulds.termstring(self, typename) --@@
   local s = self:terminals(typename)
   local t = {}
   for pos, ss in ipairs(s) do
      for _, tt in ipairs(ss) do
         if tt[1] then  t[#t+1]= fmt("%s\t%s\t%s",pos,tt[2],tt[1]) end
      end
   end
   --deepprint(s)
   return table.concat(t,"\n")
end

function moulds.dump(self, x) --@@
   local typename = x[1]
   local s = self:terminals(typename)
   local t = {}
   for pos, ss in ipairs(s) do
      for _, tt in ipairs(ss) do
         if tt[1] and x[pos]~=self.NP then
            t[#t+1]= fmt("%s\t%s\t%s",pos,x[pos],tt[1])
         end
      end
   end
   print(table.concat(t,"\n"))

end
--@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


-------------------------------------------------------------------------------
-- indicesof
-------------------------------------------------------------------------------

local function outerfield(fieldname)
-- splits a dotted fieldname in the outer field name and the rest
   if not fieldname or fieldname == '*' or fieldname == '' then return nil end
   local i = string.find(fieldname,'[.]')
   if not i then return fieldname end
   return string.sub(fieldname,1,i-1), string.sub(fieldname,i+1)
end


local function indicesof(self, typename, fieldname, pos)
-- given the field 'fieldname' in the type 'typename', 
-- returns: size, first, last, td
-- fieldname is the 'dot' field name, or nil or '' for the whole type

   local td = self.T[typename]
   if not td then error(fmt("invalid type '%s'",typename),2) end

   local pos = pos or 1 -- stay behind of one position (type may be void)
   local size = td[1]
   if not fieldname or fieldname == "*" or fieldname == '' then
      if size == 0 then return 0, 0, 0, td  end
      return size, pos+1, pos+size, td
   end

   local kind = td[2]
   local outer, subfieldname = outerfield(fieldname)

   if kind == 0 then -- type has no named fields
      error(fmt("invalid field name '%s' (%s)",outer,fullname(typename)),2)
   end

   local subtype

   if kind == 1 then -- "array"
      local N = td[3]
      local i = tonumber(outer) -- should be a number between 1 and N
      if not i or (i%1~=0) or i<1 or i>N then 
         error(fmt("invalid field name '%s' (%s)",outer,fullname(typename)),2)
      end
      subtype = td["basetype"]
      pos = pos + (i-1)*sizeof(self, subtype)
   else -- if kind == 2 ("struct") or kind == 3 ("union")
      -- local fields = td[3]
      local fd = td[outer] -- should be a valid field descriptor
      if not fd then
         error(fmt("invalid field name '%s' (%s)",outer,fullname(typename)),2)
      end
      subtype = fd[1]
      pos = pos + fd[2]-1
   end
      
   return indicesof(self, subtype, subfieldname, pos)
end


-------------------------------------------------------------------------------
-- new(), get(), set()
-------------------------------------------------------------------------------

function moulds.new(self, typename)
-- returns an unitialized sequence of the specified type
   local td = self.T[typename]
   assertf(2, td, "unknown type '%s'", typename)
   local NP = self.NP
   local s = { typename }
   for i=2, td[1]+1 do s[i]=NP end
   return s
end


function moulds.clone(self, s)
   assertf(2, type(s)=="table", "invalid type instance")
   local len = sizeof(s[1])
   assertf(2, s:len() == (sz+1), "type instance has an invalid length")
   return { table.unpack(s) }
end


function moulds.set(self, s, fieldname, ...)
-- set field 'fieldname' of the sequence s (no type-checking)
   assertf(2, type(s)=="table", "missing or invalid type instance")
   assertf(2, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local ok, size, first, last = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename, fieldname)
   if not ok then error(size,2) end
   if size == 0 then return end -- void type
   local val = {...}
   local nval = #val
   local j = 1
   if nval == 0 then -- reset field
      local NP = self.NP
      for i = first, last do s[i] = NP j=j+1 end
   else -- set only the passed values
      for i = first, first + nval - 1 do 
         s[i] = val[j] j=j+1 end
   end
end


function moulds.get(self, s, fieldname)
-- get field 'fieldname' from the sequence s (no type-checking)
   assertf(2, type(s)=="table", "missing or invalid type instance")
   assertf(2, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local ok, size, first, last = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename, fieldname)
   if not ok then error(size,2) end
   if size == 0 then return nil end -- void type
   local f, j = {}, 1
   for i = first, last do f[j] = s[i] j=j+1 end
   return table.unpack(f)
end


function moulds.sizeof(self, typename, fieldname)
   local typename = typename
   if type(typename)=="table" then typename = typename[1] end
   assertf(2, type(typename)=="string", "invalid type name")
   local ok, size, first, last, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename, fieldname)
   if not ok then error(size,2) end
   return size, first, last, td[5]
end


function moulds.tset(self, s, fieldname, val)
-- safely set a terminal field
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid type instance")
   assertf(errlvl, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename, fieldname)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to set void field '%s'", fieldname, errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a terminal field", fieldname)
   if not val or val == self.NP then s[pos] = self.NP return end
   if not td[4](val) then
      error(fmt("invalid value '%s' for terminal field '%s' (%s)"
                  ,truncate(val,20),fieldname,td[3]),errlvl)
   end
   s[pos] = val
end


function moulds.tget(self, s, fieldname)
-- safely get a terminal field's value
-- on error, including if fieldname is not a terminal field, it raises an error()
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid type instance")
   assertf(errlvl, type(fieldname)=="string", "missing or invalid field name")
   local typename = s[1]
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename, fieldname)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to get void field '%s'", fieldname, errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a terminal field", fieldname)
   local val = s[pos]
   if val ~= self.NP and not td[4](val) then
      error(fmt("invalid value '%s' in terminal field '%s' (%s)"
                  ,truncate(val,20),fieldname,td[3]),errlvl)
   end
   return val
end

function moulds.pset(self, s, val)
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid type instance")
   local typename = s[1]
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to set void field", errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a primitive type", typename)
   if not val or val == self.NP then s[pos] = self.NP return end
   if not td[4](val) then
      error(fmt("invalid value '%s' for type '%s'",truncate(val,20),td[3]),errlvl)
   end
   s[pos] = val
end

function moulds.pget(self, s)
   local errlvl = 2
   assertf(errlvl, type(s)=="table", "missing or invalid type instance")
   local typename = s[1]
   local ok, size, pos, _, td = xpcall(function(...)
      return indicesof(...)
   end, notraceback, self, typename)
   if not ok then error(size,errlvl) end
   if size == 0 then 
      error("attempt to set void field", errlvl)
   end
   assertf(errlvl, td[2]==0, "'%s' is not a primitive type", typename)
   local val = s[pos]
   if val ~= self.NP and not td[4](val) then
      error(fmt("invalid value '%s' in type '%s'",truncate(val,20),td[3]),errlvl)
   end
   return val
end

-------------------------------------------------------------------------------
-- Type-check functions
-------------------------------------------------------------------------------

check_number = function(x) return type(x)=="number" end
check_boolean = function(x) return type(x)=="boolean" end
check_string = function(x) return type(x)=="string" end
check_table = function(x) return type(x)=="table" end
check_thread = function(x) return type(x)=="thread" end
check_userdata = function(x) return type(x)=="userdata" end
check_function = function(x) return type(x)=="function" end

check_bit = function(x)
   return type(x)=="number" and (x==0 or x==1)
end

local UCHARMIN = 0
local UCHARMAX = 255

check_uchar = function(x)
   if type(x)~="number" or x < UCHARMIN or x > UCHARMAX then return false end
   return (x % 1)==0
end

local CHARMIN = -128
local CHARMAX = 127

check_char = function(x)
   if type(x)~="number" or x < CHARMIN or x > CHARMAX then return false end
   return (x % 1) == 0
end

local USHORTMIN = 0
local USHORTMAX = 65535

check_ushort = function(x)
   if type(x)~="number" or x < USHORTMIN or x > USHORTMAX then return false end
   return (x % 1)==0
end

local SHORTMIN = -32768
local SHORTMAX = 32767

check_short = function(x)
   if type(x)~="number" or x < SHORTMIN or x > SHORTMAX then return false end
   return (x % 1)==0
end

local UINTMIN = 0
local UINTMAX = 4294967295

check_uint = function(x)
   if type(x)~="number" or x < UINTMIN or x > UINTMAX then return false end
   return (x % 1)==0
end

local INTMIN = -2147483648 -- -2^31
local INTMAX = 2147483647 -- 2^31 - 1

check_int = function(x)
   if type(x)~="number" or x < INTMIN or x > INTMAX then return false end
   return (x % 1)==0
end

check_ulong = function(x)
   if type(x)~="number" then return false end
   return (x % 1)==0
end

check_long = function(x)
   if type(x)~="number" then return false end
   return (x % 1)==0
end

local FLTMAX = 3.402823e+38 -- 340282346638528859811704183484516925440.000000
check_float = function(x)
   return type(x)=="number" and x > -FLTMAX and x < FLTMAX
end

check_double = function(x)
   return type(x)=="number"
end

local bitstrp = re.compile("[01]+ !. ")
check_bitstr = function(x)
   return type(x)=="string" and (bitstrp:match(x) ~= nil)
end

local hexstrp = re.compile("([0-9] / [a-f] / [A-F])+ !.")
check_hexstr = function(x)
   return type(x)=="string" and ((x:len() % 2)==0) and (hexstrp:match(x) ~= nil)
end

return new
