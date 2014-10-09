module Compiler.Utils.Env where

import Prelude hiding (LT,EQ)

-----------------------------------
--- Simple environment used by compiler's monad
-----------------------------------

type Name = String

type Env = (Int,[Name])

-- operators

data BOP = ADD | MULT | MINUS | LT | EQ deriving (Eq, Ord, Enum)

instance Show BOP where
   show ADD = "+"
   show MULT = "*"
   show MINUS = "-"
   show LT = "<"
   show EQ = "=="


-- initial environment for DeBruijn translation

initialEnv :: Env
initialEnv = (length ops, ops)
             where
                ops = map show [ADD .. EQ]
