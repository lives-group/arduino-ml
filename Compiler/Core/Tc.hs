module Compiler.Core.Tc (typeCheck, Ctx(..)) where

import Control.Applicative((<$>))
import Control.Monad(liftM)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import Data.List ((\\))

import Compiler.Core.Syntax hiding (BOP (..))
import qualified Compiler.Core.Syntax as B
import Compiler.Core.Primitives
import Compiler.Utils.Monad


-----------------------------------
--- Definition of the core language type inference engine
-----------------------------------

typeCheck :: Ctx -> Term -> TcM Ty
typeCheck ctx t = liftM fst (tc ctx t)

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
         top <- tcOp b
         v <- freshVar
         s'' <- unify (t ->> t' ->> v) top
         return (apply s'' v, s'' @@ s' @@ s)
tc ctx (Lam n t)
     = do
          v <- freshVar
          (t', s) <- tc (ins ctx n (Simple v)) t
          return (apply s (v ->> t'), s)
tc ctx (App l r)
     = do
         (t,s) <- tc ctx l
         v <- freshVar
         (t',s') <- tc ctx r
         s'' <- unify (apply s t) ((apply s t') ->> v)
         let sr = s'' @@ s' @@ s
         return (apply sr v, sr)
tc ctx (If e e' e'')
      = do
         (t,s) <- tc ctx e
         (t',s') <- tc ctx e'
         (t'',s'') <- tc ctx e''
         unless (t == tBool) (expectedBoolError e t)
         unless (t' == t'') (typeMatchError e' t' e'' t'')
         let sr = s'' @@ s' @@ s
         return (apply sr t' , sr)
tc ctx (Let n e e')
      = do
         v <- freshVar
         (t,s) <- tc (ins ctx n (Simple v)) e
         let sig = gen (apply s ctx) (apply s t)
         tc (ins ctx n sig) e'

tcLiteral :: Literal -> TcM Ty
tcLiteral (ILit _) = return tInt
tcLiteral (CLit _) = return tChar
tcLiteral (DLit _) = return tDouble

tcOp :: B.BOP -> TcM Ty
tcOp B.ADD   = return tAdd
tcOp B.MULT  = return tMult
tcOp B.MINUS = return tMinus
tcOp B.LT    = return tLT
tcOp B.EQ    = return tEQ

-- type generalization

gen :: Ctx -> Ty -> Type
gen ctx t = Forall ns' t'
            where
              vs = tv t
              ns = vs \\ (tv ctx)
              ns' = [0.. length vs]
              s  = zipWith (\v n -> (v,Bound n)) ns [0..]
              t' = apply s t

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

ins :: Ctx -> Name -> Type -> Ctx
ins ctx n t = Ctx ((n,t) : unCtx ctx)

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

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(v, apply s1 t) | (v,t) <- s2] ++ s1

(+->) :: Name -> Ty -> Subst
v +-> t = [(v,t)]

-- unification algorithm

varBind :: Name -> Ty -> TcM Subst
varBind v t
      | v `notElem` tv t = return [(v,t)]
      | otherwise     = occursCheckError v t

unify :: Ty -> Ty -> TcM Subst
unify (Free v) t = varBind v t
unify t (Free v) = varBind v t
unify (TCon n) (TCon n')
      | n == n'   = return sid
      | otherwise = differentConstructorsError n n'
unify (TApp l r) (TApp l' r')
      = do
          s  <- unify l l'
          s' <- unify (apply s r) (apply s r')
          return (s' @@ s)
unify t t' = differentShapesError t t'

-- type checking monad

type TcM a = PhaseM a

freshVar :: TcM Ty
freshVar = (Free . toName) <$> fresh

-- error reporting functions

undefVarErr :: Name -> TcM a
undefVarErr n
    = throwError ("The variable:\n" ++ n ++ "\nisn't defined.")

occursCheckError :: Name -> Ty -> TcM a
occursCheckError n t
    = throwError ("The variable:\n" ++ n ++ "\noccurs in:\n" ++ show (pprint t))

differentConstructorsError :: Name -> Name -> TcM a
differentConstructorsError n n'
    = throwError ("The constructors:\n" ++ n ++ "\nand\n" ++ n' ++ "\nare different")

differentShapesError :: Ty -> Ty -> TcM a
differentShapesError t t'
    = throwError ("The types:\n" ++ show (pprint t) ++ "\nand\n" ++ show (pprint t') ++
                  "aren't unifiable")

expectedBoolError :: Term -> Ty -> TcM a
expectedBoolError e t
    = throwError ("Expected boolean but found:" ++ show (pprint t))

typeMatchError :: Term -> Ty -> Term -> Ty -> TcM a
typeMatchError e t e' t'
    = throwError ("Types do not match.\nExpected:\n" ++ show (pprint t) ++ "\nbut, found:" ++
                  show (pprint t'))

-- some auxiliar functions

toName :: Int -> String
toName = ("x_" ++) . show
