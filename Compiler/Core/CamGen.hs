module Compiler.Core.CamGen (generateCamCode) where

import Compiler.CAM.Cam
import Compiler.Core.DeBruijn
import Compiler.Core.Syntax(Literal(..))
import Compiler.Utils.Monad

-----------------------------------
--- Generating CAM instructions from a nameless term
-----------------------------------

generateCamCode :: NTerm -> PhaseM Cam
generateCamCode t = genCode t >>= return . Cam

genCode :: NTerm -> PhaseM [Com]
genCode (NVar n)
         = varCode n
genCode (NConst l)
         = return [Quote l]
genCode (NIf e e' e'')
         =  do
             c <- genCode e
             c' <- genCode e'
             c'' <- genCode e''
             return (Push : c ++ [Branch c' c''])
genCode (NLam e)
         = do
             c <- genCode e
             return [Cur c]
genCode (NApp l r)
         = do
             c <- genCode l
             c' <- genCode r
             return (Push : c ++ Swap : c' ++ [Cons])
genCode (NLet e e')
         = do
             c <- genCode e
             c' <- genCode e'
             return (concat [[Push, Quote Null, Cons, Push], c, [Swap, Rplac], c'])

varCode :: Int -> PhaseM [Com]
varCode 0 = return [Cdr]
varCode n = do
              cs <- varCode (n - 1)
              return (Car : cs)
