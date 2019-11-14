module Parse_Unittests (
  parseTestTree
  )where

import Data.Void
import Data.Either
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.SExpresso.SExpr
import Data.SExpresso.Parse

type Parser = Parsec Void String

asciiLetter :: Parser Char
asciiLetter = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])

pAtom :: Parser String
pAtom = some asciiLetter <|> some digitChar

pSExpr :: Parser (SExpr String)
pSExpr = simpleSExpr pAtom

pManySExpr1 :: Parser [SExpr String]
pManySExpr1 = simpleManySExpr1 pAtom

pOptionalSpace :: Parser (SExpr String)
pOptionalSpace = sexpr (char '(') (char ')') pAtom space1 sepIsOptional

parseTestTree :: TestTree
parseTestTree = testGroup "Parse/Generic.hs & Parse/Char.hs unit tests" $
  let tparse :: Parser a -> String -> Either String a
      tparse p s = first M.errorBundlePretty $ M.parse p "" s

      sExprTests :: (Eq a, Show a) => Parser a -> (SExpr String -> a) -> [TestTree]
      sExprTests p f = [
        let s = "()" in testCase (show s) $ tparse p s @?= (Right $ f (SList [])),
        let s = "(   )" in testCase (show s) $ tparse p s @?= (Right $ f (SList [])),
        let s = "foo" in testCase (show s) $ tparse p s @?= (Right $ f (SAtom "foo")),
        let s = "1234" in testCase (show s) $ tparse p s @?= (Right $ f (SAtom "1234")),
        let s = "(foo)" in testCase (show s) $ tparse p s @?= (Right $ f (SList [SAtom "foo"])),
        let s = "( foo)" in testCase (show s) $ tparse p s @?= (Right $ f (SList [SAtom "foo"])),
        let s = "(foo )" in testCase (show s) $ tparse p s @?= (Right $ f (SList [SAtom "foo"])),
        let s = "(foo bar baz)"
        in testCase (show s) $ tparse p s @?= (Right $ f (SList [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "(foo bar baz )"
        in testCase (show s) $ tparse p s @?= (Right $ f (SList [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "( foo bar baz )"
        in testCase (show s) $ tparse p s @?= (Right $ f (SList [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "(foo (bar baz))"
        in testCase (show s) $ tparse p s @?=
           (Right $ f (SList [SAtom "foo", SList [SAtom "bar", SAtom "baz"]])),
        let s = "(foo(bar baz))"
        in testCase (show s) $ tparse p s @?=
           (Right $ f (SList [SAtom "foo", SList [SAtom "bar", SAtom "baz"]])),
        let s = "((foo bar)baz)"
        in testCase (show s) $ tparse p s @?=
           (Right $ f (SList [SList [SAtom "foo", SAtom "bar"], SAtom "baz"])),
        let s = "(foo1234)"
        in testCase (show s) $ (isLeft $ tparse p "(foo1234)") @? "Parsing must fail. foo and 1234 are not separated by whitespace"
        ]
  in
    [
      testGroup "parseSExpr" $ sExprTests pSExpr id ++
      [
        let s = " foo"
        in testCase (show s) $ (isLeft $ tparse pSExpr s) @? "Parsing must fail. sexpr should not parse whitespace"
      ],
     testGroup "manySExpr1" $ sExprTests pManySExpr1 (\x -> [x]) ++
      [
        let s = "()()" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [], SList []],
        let s = "() ()" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [], SList []],
        let s = "()() " in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [], SList []],
        let s = "(foo)(1234)" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [SAtom "foo"], SList [SAtom "1234"]],
        let s = "(foo) (1234)" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [SAtom "foo"], SList [SAtom "1234"]],
        let s = "(foo)(1234) " in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [SAtom "foo"], SList [SAtom "1234"]],
        let s = "(foo) 1234" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [SAtom "foo"], SAtom "1234"],
        let s = "foo 1234" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SAtom "foo", SAtom "1234"],
        let s = "foo(1234)" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SAtom "foo", SList [SAtom "1234"]],
        let s = "(foo)1234" in testCase (show s) $ tparse pManySExpr1 s @?= Right [SList [SAtom "foo"], SAtom "1234"],
        let s = "bar1234"
        in testCase (show s) $ (isLeft $ tparse pManySExpr1 s) @? "Parsing must fail. bar and 1234 are not separated by whitespace",
        let s = " foo"
        in testCase (show s) $ (isLeft $ tparse pSExpr s) @? "Parsing must fail. manySExpr1 should not parse whitespace"
      ],
     testGroup "spaceIsOptional" $ [
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
