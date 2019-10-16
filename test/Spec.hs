
module Main where

import Test.Tasty
import SExpr_Unittests
import Parse_Unittests
import Print_Unittests
import SchemeR5RS_Unittests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [sexpTestTree, parseTestTree, printTestTree, r5rsTestTree]
