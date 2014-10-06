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

instance Pretty Com where
  pprint (Quote l) = pprint l
  pprint (Op b)    = pprint b
  pprint Car       = text "car"
  pprint Cdr       = text "cdr"
  pprint Cons      = text "cons"
  pprint Push      = text "push"
  pprint Swap      = text "swap"
  pprint App       = text "app"
  pprint Rplac     = text "rplac"
  pprint (Cur cs)  = text "cur" <>
                     parens (hcat $ punctuate semi (map pprint cs))
  pprint (Branch cs cs')
        = text "branch" <> parens ((hcat $ punctuate semi (map pprint cs)) <>
                                   (hcat $ punctuate semi (map pprint cs')))
  pprint Print = text "print"
  pprint Read  = text "read"

instance Pretty Cam where
  pprint = hcat . punctuate semi . map pprint . unCam

instance Show Cam where
  show = show . pprint
