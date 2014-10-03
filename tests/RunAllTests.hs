module Tests.RunAllTests where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Tests.Cases.DeBruijnConverter

main = defaultMain tests

tests = [testGroup "DeBruijn Converter Algorithm Tests"
               [testCase "testconvert1" testdebruijnconverter1,
                testCase "testconvert2" testdebruijnconverter2]]
