
module Main where

import Test.Tasty
import SExpr_Unittests
import Combinators_Unittests
import Print_Unittests
import SchemeR5RS_Unittests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [sexpTestTree, combinatorTestTree, printTestTree, r5rsTestTree]
