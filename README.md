## LuaMoulds: A C-like typedef for Lua

_*** This project is discontinued, superseded by
[MoonTypes](https://github.com/stetre/moontypes). ***_


LuaMoulds is a Lua module that provides means to define **C-like structured types** in Lua.

It has been designed for the definition of formats of messages (in particular, of signals
exchanged by [LunaSDL](https://github.com/stetre/lunasdl) agents),
but it is a standalone module that can be used as a general purpose type system as well.

LuaMoulds requires [Lua](http://www.lua.org/) (>=5.2), and 
[LPeg](http://www.inf.puc-rio.br/~roberto/lpeg/).
It is is written in plain Lua, so no compiling is needed.

_Authored by:_ _[Stefano Trettel](https://www.linkedin.com/in/stetre)_

[![Lua logo](./doc/powered-by-lua.gif)](http://www.lua.org/)

#### License

MIT/X11 license (same as Lua). See [LICENSE](./LICENSE).

## Intro

The LuaMoulds module is loaded with _require( )_, as shown in the example below.
It returns a function that, each time it is called, creates a new database of type definitions
(which we will call a '**mould set**').

```lua
moulds = require("moulds")

ms = moulds() -- creates a new mould set (i.e. a database of types)
```

A mould set, when created, has only a few **primitive types** defined in it, namely:
* Lua's native [basic types](http://www.lua.org/manual/5.3/manual.html#2.1)
(with the exception of _nil_),
* the _void_ type (a type having no values),
* a few constrained numeric types (_bit_, _char_, _short_, _int_, etc.), and
* the _bitstr_ and _hexstr_ types (strings representing binary and hexadecimal data).

Additional custom primitive types may be defined with the **primitive**( ) method.

**Derived types** can be defined on top of primitive types, and added to the mould set
using the **typedef**( ) and the **ftypedef**( ) methods.
Derived types are defined with a C-like _typedef_ syntax that supports the _struct_
and _union_ constructs, as well as arrays and nesting.
For example:

```lua

-- defines a few types in the mould set ms:
ms:typedef([[

-- this is a comment (C and C++ style comments are also supported)
#include <stdio.h> /* and lines starting with '#' are ignored */

-- some aliases of primitive types:
typedef boolean myboolean
typedef string mystring
typedef void myvoid

-- a derived structured type:
typedef struct {
   boolean b      
   string  s;  -- optional separators ';' or ',' can be used
   number  arr[5]
} mytype

]])
```


An **instance** of a LuaMoulds type is a 
Lua [sequence](http://www.lua.org/manual/5.3/manual.html#3.4.7)
whose first array-element is a string denoting the type, and the elements that follow are
the values of the terminal fields. The special value '?' is assigned to uninitialized (or absent)
terminal fields. This special value is configurable.

To instantiate a type, one can use the **new**( ) method. For example:

```lua
x = ms:new('mytype') -- create an uninitialized instance of mytype
--> x = { 'mytype', '?', '?', '?', '?', '?', '?', '?' }
```

The **set**( ) and **get**( ) methods can then be used to set or get the values of the fields.
With these methods, fields may be accessed at any level of nesting, including the outermost:

```lua
-- set some values:
ms:set(x, 'b', true)          -- set the x.b field
ms:set(x, 's', 'this is s')   -- set the x.s field
ms:set(x, 'arr', 12, 25, 41)  -- set (part of) the x.arr field
--> x = { 'mytype', true, 'this is s', 12, 25, 41, '?', '?' }

-- get some values:
print(ms:get(x, '*'))     --> true  'this is s'  12  25  41  ?  ?  (the whole type)
print(ms:get(x, 'b'))     --> true
print(ms:get(x, 's'))     --> 'this is s'
print(ms:get(x, 'arr'))   --> 12  25  41  ?  ?  (the whole array)
print(ms:get(x, 'arr.2')  --> 25  (only the 2nd element)

-- copy a whole array from an instance to another:
y = ms:new('mytype')
ms:set(y, 'arr', ms:get(x, 'arr'))  
--> y = { 'mytype', '?', '?', 12, 25, 41, '?', '?' }

```

Fields are referred to with a **dot notation** similar to the notation used in C, but with
a couple of differences:  elements of arrays are named with their index, starting from 1
(e.g., the elements of the '_arr_' array in the above example are named 
'_arr.1_', '_arr.2_', ..., '_arr.5_'), and the special name '_*_' is used to denote the
whole type.

Besides having a structure encoded in the type name, LuaMoulds instances are
still regular Lua tables, and as such, their fields are **dynamically typed**. 
That is, any valid Lua value may be assigned to a terminal field, and values of different
types may be assigned to it at different times. 

The **set**( ) and **get**( ) methods don't do type-checking on the values they write in
or read from type instances.

For **safe access**, LuaMoulds provides the two additional methods **tset**( ) and **tget**( ),
where the 't' stands for 'terminal' as these methods can only be used to access terminal fields.
It provides also two methods, **pset**( ) and **pget**( ), to conveniently
access instances of primitive types without the need to specify a field name.

#### Documentation

For more details, see the [Reference Manual](https://stetre.github.io/luamoulds/doc/index.html).

#### Getting and installing

See [here](https://stetre.github.io/luamoulds/doc/index.html#_getting_and_installing).

#### Examples

See [here](https://stetre.github.io/luamoulds/doc/index.html#_examples).

#### What's in a name?

Lua tables are ductile like clay, and type definitions are like moulds that give them shapes. Just that.
