module Compiler.Core.DeBruijn where

import Data.Map(Map)
import qualified Data.Map as Map

import Compiler.Core.Syntax

-----------------------------------
--- Converting a term to its DeBruijn representation.
-----------------------------------


-- describing syntax using DeBruijn rep.
-- this is needed to get a direct translation to CAM in the code generator.

type Index = Int

data NTerm = NVar Index             -- DeBruijn variable
           | NConst Literal         -- Constant
           | NCon Index             -- Constructors are also replaced by indexes
           | NIf NTerm NTerm NTerm  -- conditionals
           | NLam NTerm             -- lambda terms
           | NApp NTerm NTerm       -- application
           | NLet NTerm NTerm       -- let expression
           deriving (Eq, Ord, Show)
