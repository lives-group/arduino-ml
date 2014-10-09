module Compiler.Core.Syntax(module Compiler.Core.Syntax, BOP(..)) where

import Prelude hiding (LT, EQ)
import Data.Char (chr)
import Text.PrettyPrint.HughesPJ

import Compiler.Utils.Env

-----------------------------------
--- Definition of the core language syntax
-----------------------------------

data Literal = ILit Int | CLit Char | DLit Double | Null
               deriving (Eq, Ord, Show)

data Term = Var Name                 -- variables
          | Const Literal            -- literals
          | Op BOP Term Term         -- binary operators
          | Lam Name Term            -- abstractions
          | App Term Term            -- applications
          | If Term Term Term        -- conditionals
          | Let Name Term Term       -- let
          deriving (Eq, Ord, Show)

-- types

data Ty = Free Name     -- free type variables. Used only by type inference engine
        | Bound Int     -- bound type variables.
        | TCon Name     -- type constructors
        | TApp Ty Ty    -- type application
        deriving (Eq, Ord, Show)

data Type = Forall [Int] Ty  -- type scheme
          | Simple Ty        -- simple type
          deriving (Eq, Ord, Show)

-- pretty printer

class Pretty a where
   pprint :: a -> Doc

instance Pretty BOP where
   pprint ADD  = text "+"
   pprint MULT = text "*"
   pprint MINUS = text "-"
   pprint LT = text "<"
   pprint EQ = text "="

instance Pretty Ty where
   pprint (Free n)   = text n
   pprint (Bound n)  = text (binders n)
   pprint (TCon n)   = text n
   pprint (TApp l r) = empty

instance Pretty Literal where
   pprint (ILit i) = int i
   pprint (CLit c) = char c
   pprint (DLit d) = double d
   pprint Null     = int 0

binders :: Int -> String
binders n = [ chr ((n `mod` 26) + 97) ]
