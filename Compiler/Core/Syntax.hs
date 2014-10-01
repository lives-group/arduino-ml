module Compiler.Core.Syntax where


-----------------------------------
--- Definition of the core language syntax
-----------------------------------

data Literal = ILit Int | CLit Char | FLit Float | DLit Double
               deriving (Eq, Ord, Show)

data BOP = ADD | MULT | EQ deriving (Eq, Ord, Show)

type Name = String

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
        | Bound Name    -- bound type variables.
        | TCon Name     -- type constructors
        | TApp Ty Ty    -- type application
        deriving (Eq, Ord, Show)

data Type = Forall [Name] Ty -- type scheme
          | Simple Ty        -- simple type
          deriving (Eq, Ord, Show)
