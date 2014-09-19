module Compiler.Core.Syntax where


-----------------------------------
--- Definition of the core language syntax
-----------------------------------

type Name = String

data Term = Var Name
          | IntConst Int
          | BoolConst Bool
          | Lam Name Term
          | App Term Term
          | If Term Term Term
          | Let Name Term Term
          | LetRec Name Term Term
          deriving (Eq, Ord, Show)
