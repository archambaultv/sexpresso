
module Main where

import Test.Tasty
import SExpr_Unittests
import Parse.Char_Unittests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [sexpTestTree, charTestTree]
