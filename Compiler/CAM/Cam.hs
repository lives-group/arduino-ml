module Compiler.CAM.Cam where


import Control.
import Text.PrettyPrint.HughesPJ

--------------------
-- simple implementation of the categorical abstract machine
-------------------

-- value definition

data Value = IntValue Int
           | BoolValue Bool
           | NullValue deriving (Eq, Ord, Show)

-- Operators

data OP = Plus | Minus | EQ deriving (Eq, Ord, Show)

-- Commands for the categorical abstract machine

data Com = Quote Value
         | Op OP
         | Car
         | Cdr
         | Cons
         | Push
         | Swap
         | App
         | Rplac
         | Cur [Com]
         | Branch [Com] [Com]
         deriving (Eq, Ord, Show)

-- a full CAM program

newtype Cam = Cam { unCam :: [Com] }

-- here will be the code generator.
