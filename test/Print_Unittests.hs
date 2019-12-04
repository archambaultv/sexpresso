{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Print_Unittests (
  printTestTree
  )where

import Data.Void
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.SExpr
import Data.SExpresso.Print
import Data.SExpresso.Parse

type Parser = Parsec Void String

instance (Serial m a) => Serial m (SExpr a) where
  series = cons1 SAtom \/ cons1 SList

printer :: SExprPrinter String (SExpr Integer)
printer = simplePrinter (sexprTranslator "(" ")" show) " "

pDigit :: Parser Integer
pDigit = do
  sign <- optional (char '-')
  n <- fmap read (some digitChar)
  case sign of
    Nothing -> return n
    Just _ -> return (-1 * n)

sexpParser :: Parser (SExpr Integer)
sexpParser = sexpr (char '(') (const $ char ')') pDigit space1 sepIsMandatory

printTestTree :: TestTree
printTestTree = testGroup "Print.hs unit tests" $
  [testGroup "flatPrint" [
      testCase "Empty SList" $ flatPrint printer (SList [] :: SExpr Integer) @?= "()",
      testCase "Singleton SList" $ flatPrint printer (SList [SAtom 1] :: SExpr Integer) @?= "(1)",
      testCase "SList 1/3" $ flatPrint printer (SList [SAtom 1, SAtom 2, SAtom 3] :: SExpr Integer) @?= "(1 2 3)",
      testCase "SList 2/3" $ flatPrint printer (SList [SAtom 1, SList [SAtom 2], SAtom 3] :: SExpr Integer) @?= "(1 (2) 3)",
      testCase "SList 3/3" $ flatPrint printer (SList [SList [SAtom 1], SAtom 2, SList [SAtom 3]] :: SExpr Integer) @?= "((1) 2 (3))",
      testCase "SAtom" $ flatPrint printer (SAtom 3 :: SExpr Integer) @?= "3",
      SC.testProperty "decodeOne inverse of flatPrint" $
      \s -> parse (sexpParser) "" (flatPrint printer (s :: SExpr Integer)) == Right s
      ]
  ]
