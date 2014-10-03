module Tests.Cases.DeBruijnConverter where


import Test.HUnit
import Compiler.Core.DeBruijn
import Compiler.Core.Syntax


-- test cases for DeBruijn conversion

testdebruijnconverter1 = testconverter t1 t1'
testdebruijnconverter2 = testconverter t2 t2'

testconverter :: Term -> NTerm -> Assertion
testconverter t nt
      = assertEqual "De Bruijn Conversion Result:" nt (convert t)

-- values for testing

t1 = Lam "x" (Var "x")

t1' = NLam (NVar 0)

t2 = App (Lam "y" (Lam "x" (App (Var "x") (Var "y")))) t1

t2' = NApp (NLam (NLam (NApp (NVar 0) (NVar 1)))) t1'
