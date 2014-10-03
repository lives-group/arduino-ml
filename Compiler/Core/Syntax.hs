module Compiler.Core.Syntax where

import Prelude hiding (LT, EQ)
import Data.Char (chr)
import Text.PrettyPrint.HughesPJ

-----------------------------------
--- Definition of the core language syntax
-----------------------------------

data Literal = ILit Int | CLit Char | FLit Float | DLit Double
               deriving (Eq, Ord, Show)

type Name = String

data Term = Var Name                 -- variables
          | Const Literal            -- literals
          | Op BOP Term Term         -- binary operators
          | Lam Name Term            -- abstractions
          | App Term Term            -- applications
          | If Term Term Term        -- conditionals
          | Let Name Term Term       -- let
          deriving (Eq, Ord, Show)

-- operators

data BOP = ADD | MULT | MINUS | LT | EQ deriving (Eq, Ord, Enum)

instance Show BOP where
   show ADD = "+"
   show MULT = "*"
   show MINUS = "-"
   show LT = "<"
   show EQ = "=="

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

instance Pretty Ty where
   pprint (Free n)   = text n
   pprint (Bound n)  = text (binders n)
   pprint (TCon n)   = text n
   pprint (TApp l r) = empty

binders :: Int -> String
binders n = [ chr ((n `mod` 26) + 97) ]
