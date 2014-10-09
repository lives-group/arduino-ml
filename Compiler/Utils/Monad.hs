module Compiler.Utils.Monad(PhaseM, run, fresh, throwError, Env, BOP(..), Name, initialEnv) where

import Control.Monad.Except
import Control.Monad.State
import Compiler.Utils.Env

-----------------------------------
--- Compiler's main monad
-----------------------------------

-- I will structure the compiler as a series of phases, each phase will use a monad.

type PhaseM a = StateT Env (ExceptT String IO) a

-- running a phase

run :: PhaseM a -> Env -> IO (Either String a)
run m n = runExceptT (runStateT m n) >>= return . either Left (Right . fst)

-- getting some fresh value

fresh :: PhaseM Int
fresh
   = do
       (n,e) <- get
       put ((n + 1),e)
       return n
