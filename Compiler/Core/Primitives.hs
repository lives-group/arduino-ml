module Compiler.Core.Primitives where

import Prelude hiding (LT, EQ)
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

-- these types aren't primitive, but for now they will be.
-- In a future, we may use pragmas to attach user
-- defined types that are isomorphic to these types.

tBool :: Ty
tBool = TCon "Bool"

tUnit :: Ty
tUnit = TCon "()"

-- arrow type constructor

tArrow :: Ty -> Ty -> Ty
tArrow l r = TApp (TApp (TCon "->") l) r

(->>) :: Ty -> Ty -> Ty
l ->> r = tArrow l r

-- primitive type operations

tAdd :: Ty
tAdd = tInt ->> tInt ->> tInt

tMult :: Ty
tMult = tInt ->> tInt ->> tInt

tMinus :: Ty
tMinus = tInt ->> tInt ->> tInt

tLT :: Ty
tLT = tInt ->> tInt ->> tBool

tEQ :: Ty
tEQ = tInt ->> tInt ->> tBool

-- initial environment for DeBruijn translation

initialEnv :: (Int, [Name])
initialEnv = (length ops, ops)
             where
                ops = map show [ADD .. EQ]
