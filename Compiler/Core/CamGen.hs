module Compiler.Core.CamGen (codeGenerator) where

import Control.Monad.Identity
import Control.Monad.State

import Compiler.CAM.Cam
import Compiler.Core.DeBruijn
import Compiler.Core.Syntax(Literal(..))

import Text.PrettyPrint.HughesPJ

-----------------------------------
--- Generating CAM instructions from a nameless term
-----------------------------------

codeGenerator :: NTerm -> Doc
codeGenerator t = header <> braces (nl <> generateCode (generateCamCode t) <> nl)

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


-- generate C code

type GenM a = (StateT Int Identity) a

runGenM :: GenM a -> a
runGenM m = fst $ runIdentity (runStateT m 0)

generateCode :: Cam -> Doc
generateCode c = runGenM (codeGen (unCam c))

codeGen :: [Com] -> GenM Doc
codeGen cs
   = do
       cs' <- mapM gen cs
       return (hcat $ punctuate (semi <> char '\n') cs')

gen :: Com -> GenM Doc
gen (Quote Null)
  = do
      v <- fresh
      return (text "quote_val((*void)" <> int v <> char ')')
gen (Quote l)
  = return (quote (text $ show l))
gen (Op op)
  = undefined
gen Car
  = return car
gen Cdr
  = return cdr
gen Cons
  = return cons
gen Swap
  = return swap
gen Rplac
  = return rplac
gen (Cur ds)
  = mapM gen ds >>= funGen
gen (Branch cs cs')
  = do
      cs1 <- mapM gen cs
      cs2 <- mapM gen cs'
      return (branch cs1 cs2)


fresh :: GenM Int
fresh = do
         v <- get
         put (v + 1)
         return v

-- auxiliar functions

quote :: Doc -> Doc
quote d = text "quote_int" <> parens d

car :: Doc
car = text "car"

cdr :: Doc
cdr = text "cdr"

cons :: Doc
cons = text "cons"

swap :: Doc
swap = text "swap"

app :: Doc
app = text "app"

rplac :: Doc
rplac = text "rplac"

cur :: [Doc] -> GenM Doc
cur ds = parens (hcat ds')
         where
           ds' = punctuate semi ds

branch :: [Doc] -> [Doc] -> Doc
branch ds ds'
     = text "branch" <> parens (ds1 <> comma <> ds2)
       where
          ds1 = hcat $ punctuate semi ds
          ds2 = hcat $ punctuate semi ds'

header :: Doc
header = map text ["#include <stdio.h>\n",
                   "#include \"../cam.h\"\n",
                   "int main(int argc, char** argv)"]

nl :: Doc
nl = char '\n'
