{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SExpr_Unittests (
  sexpTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.SExpresso.SExpr

sexpTestTree :: TestTree
sexpTestTree = testGroup "SExprr.hs unit tests"

  [testGroup "isList" [
      testCase "Empty SList" $ isList (SList [] :: SExpr Int) @?= True,
      testCase "SList" $ isList (SList [SAtom 1, SAtom 2] :: SExpr Int) @?= True,
      testCase "SAtom" $ isList (SAtom 1 :: SExpr Int) @?= False],

   testGroup "sList" [
      testCase "Empty SList" $ sList (SList [] :: SExpr Int) @?= Just [],
      testCase "SList" $ sList (SList [SAtom 1, SAtom 2] :: SExpr Int) @?= Just [SAtom 1, SAtom 2],
      testCase "SAtom" $ sList (SAtom 1 :: SExpr Int) @?= Nothing],
    
   testGroup "isAtom" [
      testCase "Empty SList" $ isAtom (SList [] :: SExpr Int) @?= False,
      testCase "SList" $ isAtom (SList [SAtom 1, SAtom 2] :: SExpr Int) @?= False,
      testCase "SAtom" $ isAtom (SAtom 1 :: SExpr Int) @?= True],

   testGroup "sAtom" [
      testCase "Empty SList" $ sAtom (SList [] :: SExpr Int) @?= Nothing,
      testCase "SList" $ sAtom (SList [SAtom 1, SAtom 2] :: SExpr Int) @?= Nothing,
      testCase "SAtom" $ sAtom (SAtom 1 :: SExpr Int) @?= Just 1],

   -- testGroup "Pattern synonyms" [
   --    testCase "L - empty list (1/2)" $
   --      (case (SList []) of
   --         L [] -> True
   --         _ -> False) @?= True,
   --    testCase "L - empty list (2/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int]) of
   --         L [] -> True
   --         _ -> False) @?= False,
   --    testCase "L - singleton list (1/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int]) of
   --         L [_] -> True
   --         _ -> False) @?= True,
   --    testCase "L - singleton list (2/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int]) of
   --         L [] -> True
   --         _ -> False) @?= False,
   --    testCase "L - atom" $
   --      (case (SAtom 1 :: SExpr Int) of
   --         L _ -> True
   --         _ -> False) @?= False,
   --    testCase "A - atom (1/2)" $
   --      (case (SAtom 1 :: SExpr Int) of
   --         A 1 -> True
   --         _ -> False) @?= True,
   --    testCase "A - atom (2/2)" $
   --      (case (A 1 :: SExpr Int) of
   --         SAtom 1 -> True
   --         _ -> False) @?= True,
   --    testCase "A - singleton list" $
   --      (case (SList [SAtom 1 :: SExpr Int]) of
   --         A 1 -> True
   --         _ -> False) @?= False,
   --    testCase "::: (1/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int, SAtom 2]) of
   --         (A 1 ::: A 2 ::: L []) -> True
   --         _ -> False) @?= True,
   --    testCase "::: (2/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int, SAtom 2, SAtom 3]) of
   --         (A 1 ::: L xs) -> xs == [SAtom 2, SAtom 3]
   --         _ -> False) @?= True,
   --    testCase "Nil (1/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int, SAtom 2]) of
   --         (A 1 ::: A 2 ::: Nil) -> True
   --         _ -> False) @?= True,
   --    testCase "Nil (2/2)" $
   --      (case (SList [SAtom 1 :: SExpr Int, SAtom 2, SAtom 3]) of
   --         (A 1 ::: A 2 ::: Nil) -> True
   --         _ -> False) @?= False
   --    ],

    testGroup "Functor" [
      testCase "Empty SList" $ fmap (\x -> x + 1) (SList [] :: SExpr Int) @?= (SList []),
      testCase "Singleton SList" $ fmap (\x -> x + 1) (SList [SAtom 5] :: SExpr Int) @?= (SList [SAtom 6])]
  ]
