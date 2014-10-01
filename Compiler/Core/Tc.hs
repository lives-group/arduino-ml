module Compiler.Core.Tc (typeCheck) where

import Control.Applicative((<$>))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import Compiler.Core.Syntax
import Compiler.Core.Primitives


-----------------------------------
--- Definition of the core language type inference engine
-----------------------------------

typeCheck :: Ctx -> Term -> Either String Ty
typeCheck ctx t = either Left (Right . fst . fst) (runTcM (tc ctx t))

tc :: Ctx -> Term -> TcM (Ty, Subst)
tc ctx (Var n)
    = do
        let v = lookup n (unCtx ctx)
            f t = inst t >>= \t' -> return (t', sid)
        maybe (undefVarErr n) f v
tc ctx (Const l)
    = liftM (\t -> (t,sid)) (tcLiteral l)
tc ctx (Op b l r)
    = do
         (t,s)   <- tc ctx l
         (t',s') <- tc ctx r
         undefined
tc ctx (Lam n t) = undefined
tc ctx (App l r) = undefined
tc ctx (If t t' t'') = undefined
tc ctx (Let n t t') = undefined

tcLiteral :: Literal -> TcM Ty
tcLiteral (ILit _) = return tInt
tcLiteral (CLit _) = return tChar
tcLiteral (FLit _) = return tFloat
tcLiteral (DLit _) = return tDouble

-- type instantiation

inst :: Type -> TcM Ty
inst (Forall vs t)
    = do
        s <- liftM (zip vs) (mapM (\_ -> freshVar) vs)
        let
          inst' s v@(Bound n) = maybe v id (lookup n s)
          inst' s (TApp l r)  = TApp (inst' s l) (inst' s r)
          inst' s c           = c
        return (inst' s t)
inst (Simple t) = return t

-- context definition

newtype Ctx = Ctx { unCtx :: [(Name,Type)] }

-- substitution and its operations over types and contexts

type Subst = [(Name,Ty)]

sid :: Subst
sid = []

class Types a where
   apply :: Subst -> a -> a
   tv    :: a -> [Name]

instance Types a => Types [a] where
   apply s = map (apply s)
   tv      = concatMap tv

instance Types Ty where
   apply s v@(Free n) = maybe v id (lookup n s)
   apply s (TApp l r) = TApp (apply s l) (apply s r)
   apply s c          = c

   tv (Free n)   = [n]
   tv (TApp l r) = tv l ++ tv r
   tv _          = []

instance Types Type where
   apply s (Simple t) = Simple (apply s t)
   apply _ t          = t

   tv (Simple t)      = tv t
   tv (Forall _ t)    = tv t

instance Types Ctx where
   apply s = Ctx . map (\(n, t) -> (n, apply s t)) . unCtx
   tv      = concatMap (tv . snd) . unCtx

-- type checking monad

type TcM a = (StateT Int (ExceptT String Identity)) a

runTcM :: TcM a -> Either String (a, Int)
runTcM m = runIdentity (runExceptT (runStateT m 0))

-- fresh variable supply

fresh :: TcM Int
fresh
   = do
       v <- get
       put (v + 1)
       return v

freshVar :: TcM Ty
freshVar = (Free . toName) <$> fresh

-- error reporting functions

undefVarErr :: Name -> TcM a
undefVarErr n = throwError ("The variable:\n" ++ n ++ "\nisn't defined.")

-- some auxiliar functions

toName :: Int -> String
toName = ("x_" ++) . show
