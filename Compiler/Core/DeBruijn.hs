module Compiler.Core.DeBruijn where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List (elemIndex)
import Prelude hiding (LT,EQ)

import Compiler.Core.Syntax
import Compiler.Core.Primitives
import Compiler.Utils.Monad

-----------------------------------
--- Converting a term to its DeBruijn representation.
-----------------------------------

convert :: Term -> PhaseM NTerm
convert t = convert' t

convert' :: Term -> ConvertM NTerm
convert' (Var n)
     = do
         env <- gets snd
         maybe (panicMessage n env) (return . NVar) (elemIndex n env)
convert' (Const l)
     = return (NConst l)
convert' (Op op l r)
     = do
         l' <- convert' l
         r' <- convert' r
         op' <- convertOp op
         return (NApp (NApp op' l') r')
convert' (Lam n t)
     = do
         fromName n
         liftM NLam (convert' t)
convert' (App l r)
     = liftM2 NApp (convert' l) (convert' r)
convert' (If e e' e'')
     = do
         t <- convert' e
         t' <- convert' e'
         t'' <- convert' e''
         return (NIf t t' t'')
convert' (Let n e e')
     = do
         fromName n
         t <- convert' e
         t' <- convert' e'
         return (NLet t t')

convertOp :: BOP -> ConvertM NTerm
convertOp op
          = do
              env <- gets snd
              maybe (panicMessage (show op) env)
                    (return . NVar)
                    (elemIndex (show op) env)


-- monad definitions

type ConvertM a = PhaseM a

fromName :: Name -> ConvertM ()
fromName n
     = do
         (i,env) <- get
         put (i + 1 , n : env)


-- describing syntax using DeBruijn rep.
-- this is needed to get a direct translation to CAM in the code generator.

type Index = Int

data NTerm = NVar Index             -- DeBruijn variable
           | NConst Literal         -- Constant
           | NIf NTerm NTerm NTerm  -- conditionals
           | NLam NTerm             -- lambda terms
           | NApp NTerm NTerm       -- application
           | NLet NTerm NTerm       -- let expression
           deriving (Eq, Ord)

-- auxiliar functions and error messages

panicMessage :: Name -> [Name] -> a
panicMessage n ns = error ("Panic! Undefined variable: " ++ n ++ "\n" ++ concat ns)
