module Combinators_Unittests (
  combinatorTestTree
  )where

import Data.Void
import Data.Either
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.SExpresso.SExpr
import Data.SExpresso.Parse.Combinators

type Parser = Parsec Void String

asciiLetter :: Parser Char
asciiLetter = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])

pAtom :: Parser String
pAtom = some asciiLetter <|> some digitChar

pSExpr :: Parser (SExpr String)
pSExpr = sexpr (char '(') (const $ char ')') pAtom space1 sepIsMandatory

pOptionalSpace :: Parser (SExpr String)
pOptionalSpace = sexpr (char '(') (const $ char ')') pAtom space1 sepIsOptional

combinatorTestTree :: TestTree
combinatorTestTree = testGroup "Parse/Combinator.hs unit tests" $
  let tparse :: Parser a -> String -> Either String a
      tparse p s = first M.errorBundlePretty $ M.parse p "" s

  in
    [
      testGroup "sexpr" 
      [
        let s = "()" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [])),
        let s = "(   )" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [])),
        let s = "foo" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SAtom "foo")),
        let s = "1234" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SAtom "1234")),
        let s = "(foo)" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [SAtom "foo"])),
        let s = "( foo)" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [SAtom "foo"])),
        let s = "(foo )" in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [SAtom "foo"])),
        let s = "(foo bar baz)"
        in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "(foo bar baz )"
        in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "( foo bar baz )"
        in testCase (show s) $ tparse pSExpr s @?= (Right $ (SList [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "(foo (bar baz))"
        in testCase (show s) $ tparse pSExpr s @?=
           (Right $ (SList [SAtom "foo", SList [SAtom "bar", SAtom "baz"]])),
        let s = "(foo(bar baz))"
        in testCase (show s) $ tparse pSExpr s @?=
           (Right $ (SList [SAtom "foo", SList [SAtom "bar", SAtom "baz"]])),
        let s = "((foo bar)baz)"
        in testCase (show s) $ tparse pSExpr s @?=
           (Right $ (SList [SList [SAtom "foo", SAtom "bar"], SAtom "baz"])),
        let s = "(foo1234)"
        in testCase (show s) $ (isLeft $ tparse pSExpr "(foo1234)") @? "Parsing must fail. foo and 1234 are not separated by whitespace",

        let s = " foo"
        in testCase (show s) $ (isLeft $ tparse pSExpr s) @? "Parsing must fail. sexpr should not parse whitespace",

        let s = "()()" in testCase (show s) $ tparse (some pSExpr) s @?= Right [SList [], SList []],
        let s = "()() " in testCase (show s) $ tparse (some pSExpr) s @?= Right [SList [], SList []],
        let s = "(foo)(1234)" in testCase (show s) $ tparse (some pSExpr) s @?= Right [SList [SAtom "foo"], SList [SAtom "1234"]],
        let s = "(foo)(1234) " in testCase (show s) $ tparse (some pSExpr) s @?= Right [SList [SAtom "foo"], SList [SAtom "1234"]],
        --let s = "(foo) 1234" in testCase (show s) $ tparse (some pSExpr) s @?= Right [SList [SAtom "foo"], SAtom "1234"],
        --let s = "foo 1234" in testCase (show s) $ tparse (some pSExpr) s @?= Right [SAtom "foo", SAtom "1234"],
        let s = "foo(1234)" in testCase (show s) $ tparse (some pSExpr) s @?= Right [SAtom "foo", SList [SAtom "1234"]],
        let s = "(foo)1234" in testCase (show s) $ tparse (some pSExpr) s @?= Right [SList [SAtom "foo"], SAtom "1234"]
      ],

     testGroup "spaceIsOptional" [
        let s = "(foo1234)"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList [SAtom "foo", SAtom "1234"])),
        let s = "(foo 1234)"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList [SAtom "foo", SAtom "1234"])),
        let s = "(foo1234 bar)"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList [SAtom "foo", SAtom "1234", SAtom "bar"])),
        let s = "( foo1234 )"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList [SAtom "foo", SAtom "1234"]))
        ]
    ]
