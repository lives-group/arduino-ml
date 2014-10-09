module Main where

import Control.Monad.Except
import Control.Monad.Trans
import System.Environment (getArgs)

import Compiler.CAM.Cam
import Compiler.Core.CamGen
import Compiler.Core.Syntax
import Compiler.Core.Parser
import Compiler.Core.Tc
import Compiler.Core.DeBruijn
import Compiler.Core.Primitives

import Compiler.Utils.Monad

main :: IO ()
main = getArgs >>= readFile . head >>= compiler

compiler :: String -> IO ()
compiler s
       = do
          r <- run (compile s) initialEnv
          either print (print . pprint) r

compile :: String -> PhaseM Cam
compile s
    = do
        t <- parser s
        ty <- typeCheck (Ctx []) t
        t' <- convert t
        generateCamCode t'
