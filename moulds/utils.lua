--=================================================================================
-- utils.lua   hexstr and bitstr utilities
--=================================================================================

--@@ UNDER CONSTRUCTION 


function assertf(level, condition, ...)
   if condition then return condition end
   local errmsg = next({...}) and string.format(...) or "assertf failed"
   error(errmsg,level+1)
end

local M = {}

-- Unless otherwise specified:
-- s is a string
-- b is a bitstr, ie a string of 0s and 1s
-- h is an hexstr, ie a string with characters '0'..'9', 'a'..'f', 'A'..'F' 
--                 (uppercase and lowercase not mixed)
-- bin is a binary string
-- n, n1,.. are integers

-------------------------------------------------------------------------------
-- Conversion tables                                                       
-------------------------------------------------------------------------------

local BBBBTOH = {
   ['0000'] = '0', ['0001'] = '1', ['0010'] = '2', ['0011'] = '3', 
   ['0100'] = '4', ['0101'] = '5', ['0110'] = '6', ['0111'] = '7',
   ['1000'] = '8', ['1001'] = '9', ['1010'] = 'a', ['1011'] = 'b',
   ['1100'] = 'c', ['1101'] = 'd', ['1110'] = 'e', ['1111'] = 'f'
}

local HTOBBBB = {
   ['0'] = '0000', ['1'] = '0001', ['2'] = '0010', ['3'] = '0011',
   ['4'] = '0100', ['5'] = '0101', ['6'] = '0110', ['7'] = '0111',
   ['8'] = '1000', ['9'] = '1001', ['a'] = '1010', ['b'] = '1011',
   ['c'] = '1100', ['d'] = '1101', ['e'] = '1110', ['f'] = '1111',
   ['A'] = '1010', ['B'] = '1011', ['C'] = '1100', ['D'] = '1101',
   ['E'] = '1110', ['F'] = '1111' 
}

local HHTON = {} -- HHTON["00"]=0 ... HHTON["ff"]=255
local NTOHH = {} -- NTOHH[0]="00" ... NTOHH[255]="ff"
for i=0,255 do 
   NTOHH[i] = string.format("%.2x",i)
   HHTON[string.format("%.2x",i)]=i
   HHTON[string.format("%.2X",i)]=i
end

-------------------------------------------------------------------------------
-- Indices translation                                                        
-------------------------------------------------------------------------------

local function fixindex(n1,n2)
-- Translates telecom-ish indices into lua-ish indices
-- (ie. indices starting from 0 into indices starting from 1).
-- Negative indices are left unchanged (i.e. -1 = last).
   local n1 = n1 or 0
   local n2 = n2 or n1
   return n1<0 and n1 or n1+1 , n2<0 and n2 or n2+1
end 

-------------------------------------------------------------------------------
-- To/from binary strings conversions                                         
-------------------------------------------------------------------------------

function M.htobin(h)
-- converts an hexstr to a binary string
   local t = {}
   local n = h:len()
   for i=1,n,2 do t[#t+1] = string.pack("B",HHTON[h:sub(i,i+1)]) end
   return table.concat(t)
end

function M.bintoh(bin)
-- converts a binary string to an hexstr
   local t = { }
   local n = bin:len()
   for i=1,n do local j = string.unpack("B",bin:sub(i,i)) t[#t+1] = NTOHH[j] end
   return table.concat(t)
end

function M.btobin(b)
-- converts a bitstr to a binary string
   return M(htobin(M.btoh(b)))
end

function M.bintob(bin)
-- converts a binary string to a bitstr
   return M.htob(M.bintoh(bin))
end

-------------------------------------------------------------------------------
-- Bitstr to/from hexstr conversions                                          
-------------------------------------------------------------------------------

function M.htob(h, n)
-- returns a bitstr composed of the first n octets of the hexstr h
   local len = h:len()
   local n = n and n*2 or len
   local t = {}
   local h = n < len and h:sub(1,n) or h
   for i=1,n do
      t[#t+1] = HTOBBBB[h:sub(i,i)]
   end
   return table.concat(t)
end


function M.btoh(b, n) 
-- returns a hexstr composed of the first n octets of the bitstr b
   local b, n = b, n or 0
   local len = b:len()
   local npad = len%8
   if npad > 0 then b = b .. string.rep('0',npad) end -- align to octet
   local t = {}
   for i=1,len+npad,4 do
      t[#t+1] = BBBBTOH[b:sub(i,i+3)]
   end
   b = table.concat(t)
   return M.hpad(b,n)
end   

-------------------------------------------------------------------------------
-- Bits and octets extraction                                              
-------------------------------------------------------------------------------

function M.bits(b, n1, n2) 
-- returns a bitstr composed of the bits n1..n2 (inclusive) of the bitstr b
-- (note: bits are counted starting from zero)
   return b:sub(fixindex(n1, n2))
end

function M.hbits(h, n1, n2) 
-- returns a bitstr composed of the bits n1..n2 (inclusive) of the hexstr h
-- (note: bits are counted starting from zero)
   return M.htob(h):sub(fixindex(n1,n2))
end

function M.octets(b, n1, n2)
-- returns a hexstr composed of the octets n1..n2 (inclusive) of the bitstr b
   local n1, n2 =  n1, n2 or n1
   return M.btoh(b:sub(fixindex(n1*8, n2*8+7)))
end

function M.hoctets(h, n1, n2)
-- returns a hexstr composed of the octets n1..n2 (inclusive) of the hexstr h
   local n1, n2 =  n1, n2 or n1
   return h:sub(fixindex(n1*2,n2*2+1))
end

-------------------------------------------------------------------------------
-- Padding                                                                 
-------------------------------------------------------------------------------

function M.bpad(b, n, pad) -- (b,n,[pad='0'])
-- returns a bitstr obtained by padding b to n octets with the pad bit
   local b = b or ""
   local pad = pad or '0'
   assertf(2, not b or type(b) == "string", "invalid bitstr") 
   assertf(2, type(n) == "number", "invalid n") 
   assertf(2, pad=='0' or pad=='1', "invalid pad") 
   local npad = n*8 - b:len()
   if npad <= 0 then return b end
   return b .. string.rep(pad, npad)
end


function M.hpad(h, n, pad) -- (h,n,[pad='00'])
-- returns an hexstr obtained by padding h to n octets with the pad (hexstr) byte
   local h = h or ""
   local pad = pad or '00'
   assertf(2, HHTON[pad], "invalid pad")
   assertf(2, type(n) == "number", "invalid n") 
   local len = h:len()
   assertf(2, len%2==0, "invalid hexstr")
   local halflen = len/2
   if halflen >= n then return h end
   local npad = n - halflen
   return h .. string.rep(pad,npad)
end


-------------------------------------------------------------------------------
-- Ascii to/from hexstr conversions                                        
-------------------------------------------------------------------------------

function M.atoh(s,n)
   local s, t = s, {}
   local len, n = s:len(), n or 0
   for i = 1,len do
      t[i] = string.format('%.2x',string.byte(s:sub(i))) 
   end
   local h = table.concat(t)
   if n > len then return M.hpad(h,n) end
   return h
end

function M.htoa(h)
   local h, t = h, {}
   local n = h:len()/2
   for i=1,n do
      t[i] = string.char(M.htouc(h:sub(2*i-1,2*i)))
   end
   for i,v in ipairs(t) do print(i,t[i]) end
   return table.concat(t)
end

-------------------------------------------------------------------------------
-- Integral types to/from hexstr conversions                                        
-------------------------------------------------------------------------------

-- uc: unsigned char ----------------------------------------------------------

M.UCMIN = 0
M.UCMAX = 255

function M.uctoh(x)
   local x = x
   assertf(2, type(x)=="number" and x>=0 and x<256, "invalid argument")
   return string.format('%.2x',x)
end

function M.htouc(h)
   assertf(2,type(h)=="string" and h:len()==2, "invalid argument")
   local x = HHTON[h]
   assertf(2,x,"invalid argument")
   return x
end


-- c: signed char -------------------------------------------------------------

M.CMIN = -128
M.CMAX = 127

function M.ctoh(x)
   local x = x
   assertf(2,type(x)=="number" and x>=-128 and x<=127, "invalid argument")
   if x<0 then x=x+256 end
   return string.format('%.2x',x)
end

function M.htoc(h)
   assertf(2,type(h)=="string" and h:len()>=2, "invalid argument")
   local x = HHTON[h]
   assertf(2,x,"invalid argument")
   return x > 127 and x-256 or x
end


-- us: unsigned short (16 bits) -----------------------------------------------

M.USMIN = 0
M.USMAX = 65535

function M.ustoh(x, endian)
   local endian = endian or 'B'
   assertf(2, type(x)=="number" and x>=0 and x<=65535,"invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local h = string.format('%.4x',x)
   if endian == 'L' then
      return h:sub(3,4)..h:sub(1,2)
   end
   return h 
end

function M.htous(h,endian)
   local endian = endian or 'B'
   assertf(2,type(h)=="string" and h:len()>=4, "invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local h1,h2,h3,h4
   if endian == 'B' then
      h1,h2,h3,h4 = h:sub(1,1), h:sub(2,2), h:sub(3,3), h:sub(4,4)
   else
      h3,h4,h1,h2 = h:sub(1,1), h:sub(2,2), h:sub(3,3), h:sub(4,4)
   end
   assertf(2, h1 and h2 and h3 and h4, "invalid argument")
   return HHTON[h1..h2]*256 + HHTON[h3..h4]
end

-- s: signed short (16 bits) --------------------------------------------------

M.SMIN = -32768
M.SMAX = 32767

function M.stoh(x,endian)
   local endian = endian or 'B'
   assertf(2, type(x)=="number" and x>=-32768 and x<=32767,"invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local x = x < 0 and x+65536 or x
   return M.ustoh(x,endian)
end

function M.htos(h,endian)
   local x = M.htous(h,endian)
   return x > 32767 and x-65536 or x
end

-- ui: unsigned int (32 bits) ------------------------------------------------

M.UIMIN = 0
M.UIMAX = 4294967295

function M.uitoh(x, endian)
   local endian = endian or 'B'
   assertf(2, type(x)=="number" and x>=0 and x<=4294967295,"invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local h = string.format('%.8x',x)
   if endian == 'L' then
      return h:sub(7,8)..h:sub(5,6)..h:sub(3,4)..h:sub(1,2)
   end
   return h 
end


function M.htoui(h,endian)
   local endian = endian or 'B'
   assertf(2,type(h)=="string" and h:len()>=8, "invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local h1,h2,h3,h4
   if endian == 'B' then
      h1,h2,h3,h4,h5,h6,h7,h8 = 
         h:sub(1,1), h:sub(2,2), h:sub(3,3), h:sub(4,4), 
         h:sub(5,5), h:sub(6,6), h:sub(7,7), h:sub(8,8)
   else
      h7,h8,h5,h6,h3,h4,h1,h2 = 
         h:sub(1,1), h:sub(2,2), h:sub(3,3), h:sub(4,4), 
         h:sub(5,5), h:sub(6,6), h:sub(7,7), h:sub(8,8)
   end
   assertf(2, h1 and h2 and h3 and h4 and h5 and h6 and h7 and h8, "invalid argument")
   return HHTON[h1..h2]*0x1000000 + HHTON[h3..h4]*0x10000 + HHTON[h5..h6]*0x100 + HHTON[h7..h8]
end


-- i: signed int (32 bits) ---------------------------------------------------

M.IMIN = -2147483648 -- -2^31
M.IMAX = 2147483647 -- 2^31 - 1

function M.itoh(x,endian)
   local endian = endian or 'B'
   assertf(2, type(x)=="number" and x>=-2147483648 and x<=2147483647,"invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local x = x<0 and x+4294967296 or x
   return M.uitoh(x,endian)
end

function M.htoi(h,endian)
   local x = M.htoui(h,endian)
   return x > 2147483647 and x-4294967296 or x
end

--[[@@ mettere a posto
--
-- ul: unsigned long (64 bits) -----------------------------------------------

M.ULMIN = 0
M.ULMAX = 4294967295

function M.ultoh(x, endian)
   local endian = endian or 'B'
   assertf(2, type(x)=="number" and x>=0 and x<=4294967295,"invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local h = string.format('%.8x',x)
   if endian == 'L' then
      return h:sub(7,8)..h:sub(5,6)..h:sub(3,4)..h:sub(1,2)
   end
   return h 
end


function M.htoul(h,endian)
   local endian = endian or 'B'
   assertf(2,type(h)=="string" and h:len()>=8, "invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local h1,h2,h3,h4
   if endian == 'B' then
      h1,h2,h3,h4,h5,h6,h7,h8 = 
         h:sub(1,1), h:sub(2,2), h:sub(3,3), h:sub(4,4), 
         h:sub(5,5), h:sub(6,6), h:sub(7,7), h:sub(8,8)
   else
      h7,h8,h5,h6,h3,h4,h1,h2 = 
         h:sub(1,1), h:sub(2,2), h:sub(3,3), h:sub(4,4), 
         h:sub(5,5), h:sub(6,6), h:sub(7,7), h:sub(8,8)
   end
   assertf(2, h1 and h2 and h3 and h4 and h5 and h6 and h7 and h8, "invalid argument")
   return HHTON[h1..h2]*0x1000000 + HHTON[h3..h4]*0x10000 + HHTON[h5..h6]*0x100 + HHTON[h7..h8]
end


-- l: signed long (64 bits) --------------------------------------------------

M.LMIN = -2147483648 -- -2^31
M.LMAX = 2147483647 -- 2^31 - 1

function M.ltoh(x,endian)
   local endian = endian or 'B'
   assertf(2, type(x)=="number" and x>=-2147483648 and x<=2147483647,"invalid argument")
   assertf(2, type(endian)=="string" and (endian=='B' or endian=='L'), "invalid endian")
   local x = x<0 and x+4294967296 or x
   return M.uitoh(x,endian)
end

function M.htol(h,endian)
   local x = M.htoui(h,endian)
   return x > 2147483647 and x-4294967296 or x
end

--]]

-------------------------------------------------------------------------------
-- Miscellanea                                                             
-------------------------------------------------------------------------------

function M.tok(s, sep) -- (s,[sep=':'])
-- returns a list of strings obtained by tokenizing the string b, interpreting the
-- character sep as separator
   local sep = sep or ':'
   local pattern = string.format('[^%s]+',sep)
   local t,i,j = {},1,0
   while true do
      i,j = string.find(s,pattern,j+1)
      if not i then break end
      t[#t+1] = string.sub(s,i,j)
   end
   return table.unpack(t)
end


--[[

local function swapnibble(h,n) -- n = no of nibbles (shall be multiple of 8)
   assertf(2, h:len() >= n and n%8==0,"invalid hexstr length")
   local t = {}
   local k = 1
   for j = 1,n,8 do
      for i = 3,0,-1 do
         t[k] = h:sub(j+2*i,j+2*i+1)
         k = k+1
      end
   end
   return table.concat(t)
end


@@ these are based on POSIX inet_pton()
function M.htoip(h,endian)
   local h, endian = h , endian or 'B'
   if(endian == 'L') then
      h = swapnibble(h,8)
   end
   return M.htoip1(h)
end

function M.iptoh(ip,endian)
   local ip, endian = ip , endian or 'B'
   local h = M.iptoh1(ip)
   if(endian == 'L') then
      h = swapnibble(h,8)
   end
   return h
end

function M.htoip6(h,endian)
   local h, endian = h , endian or 'B'
   if(endian == 'L') then
      h = swapnibble(h,32)
   end
   return M.htoip61(h)
end

function M.ip6toh(ip,endian)
   local ip, endian = ip , endian or 'B'
   local h = M.ip6toh1(ip)
   if(endian == 'L') then
      h = swapnibble(h,32)
   end
   return h
end
--]]

return M
