module Compiler.Core.Kc where

-----------------------------------
--- Definition of the core language kind inference engine
-----------------------------------

import Compiler.Core.Syntax hiding (Subst, Types(..))
import Compiler.Utils.Monad

-- kind definition

data Kind = Star             -- kind of types
          | KFun Kind Kind   -- kind of type operators
          | KVar Int         -- kind variables
            deriving (Eq, Ord)

instance Pretty Kind where
   pprint Star = text "*"
   pprint (KVar n) = text "k_" <> int n
   pprint (KFun k k')
      | isFun k = parens (pprint k) <> text "->" <> pprint k'
      | otherwise = pprint k <> text "->" <> pprint k'

-- kind substitution

type Subst = [(Int, Kind)]

-- Kind environment

type KEnv = [(Name, Kind)]

-- application of a substitution

apply :: Subst -> Kind -> Kind
apply s (KFun k k') = KFun (apply s k) (apply s k')
apply s v@(KVar n) = maybe v id (lookup n s)
apply s k = k

sid :: Subst
sid = []

-- kind checking monad

type KcM a = PhaseM a

-- kind checking algorithm

kc :: KEnv -> Ty -> KcM (Kind , Subst)
kc env (KVar n) = undefined

-- unification

unify :: Kind -> Kind -> KcM Subst
unify Star Star = return sid
unify (KVar n) k = varBind n k
unify k (KVar n) = varBind n k
unify (KFun k k1) (KFun k' k1')
    = do
        s <- unify k k'
        s' <- unify (apply s k1) (apply s k1')
        return (s' @@ s)
unify t t' = differentShapesError t t'

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(v, apply s1 t) | (v,t) <- s2] ++ s1

-- binding variables

varBind :: Int -> Kind -> KcM Subst
varBind n k
    | n `notElem` kv k = n +-> k
    | otherwise = occursCheckError n k

(+->) :: Int -> Kind -> KcM Subst
n +-> k = return [(n,k)]

-- getting kind variables

kv :: Kind -> [Int]
kv (KVar n) = [n]
kv (KFun k k') = kv k ++ kv k'
kv _ = []

-- auxiliar functions

occursCheckError :: Int -> Kind -> KcM a
occursCheckError n t
    = throwError ("The variable:\nk_" ++ show n ++ "\noccurs in:\n" ++ show (pprint t))

differentShapesError :: Kind -> Kind -> KcM a
differentShapesError t t'
    = throwError ("The types:\n" ++ show (pprint t) ++ "\nand\n" ++ show (pprint t') ++
                  "aren't unifiable")

kindInferenceError :: KcM a
kindInferenceError = throwError "Ill formed kind error."

isFun :: Kind -> Bool
isFun (KFun _ _) = True
isFun _ = False
