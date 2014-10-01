module Compiler.Core.Primitives where

import Compiler.Core.Syntax

-----------------------------------
--- Definition of some language primitives
-----------------------------------

-- primitive type declarations

tInt :: Ty
tInt = TCon "Int"

tChar :: Ty
tChar = TCon "Char"

tFloat :: Ty
tFloat = TCon "Float"

tDouble :: Ty
tDouble = TCon "Double"
