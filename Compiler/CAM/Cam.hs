module Compiler.CAM.Cam where

import Text.PrettyPrint.HughesPJ

import Compiler.Core.Syntax (Literal(..),BOP(..), Pretty(..))
--------------------
-- simple implementation of the categorical abstract machine
-------------------

-- Commands for the categorical abstract machine

data Com = Quote Literal
         | Op BOP
         | Car
         | Cdr
         | Cons
         | Push
         | Swap
         | App
         | Rplac
         | Cur [Com]
         | Branch [Com] [Com]
         | Print
         | Read
         deriving (Eq, Ord, Show)

-- a full CAM program

newtype Cam = Cam { unCam :: [Com] }

-- a pretty printer for cam programs
