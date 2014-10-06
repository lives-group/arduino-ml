module Compiler.Core.CamGen (generateCamCode) where

import Compiler.CAM.Cam
import Compiler.Core.DeBruijn
import Compiler.Core.Syntax(Literal(..))

-----------------------------------
--- Generating CAM instructions from a nameless term
-----------------------------------

generateCamCode :: NTerm -> Cam
generateCamCode = Cam . genCode

genCode :: NTerm -> [Com]
genCode (NVar n) = varCode n
genCode (NConst l) = [Quote l]
genCode (NIf e e' e'') = Push : genCode e ++ [Branch (genCode e') (genCode e'')]
genCode (NLam e) = [Cur (genCode e)]
genCode (NApp l r) = Push : (genCode l) ++ Swap : (genCode r) ++ [Cons]
genCode (NLet e e')
    = concat [[Push, Quote Null, Cons, Push], (genCode e), [Swap, Rplac], (genCode e')]

varCode :: Int -> [Com]
varCode 0 = [Cdr]
varCode n = Car : varCode (n - 1)
