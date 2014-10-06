module Main where

import System.Environment (getArgs)

import Compiler.CAM.Cam
import Compiler.Core.CamGen
import Compiler.Core.Syntax
import Compiler.Core.Parser
import Compiler.Core.Tc
import Compiler.Core.DeBruijn

type CompilerM a = Either String a

main :: IO ()
main = getArgs >>= readFile . head >>= compiler


compiler :: String -> IO ()
compiler = either putStrLn (print . pprint) . compile

compile :: String -> CompilerM Cam
compile s
    = do
         r <- parser s
         t <- typeCheck (Ctx []) r
         return (generateCamCode (fst $ convert r))
