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

pIdent :: Parser String
pIdent = some asciiLetter

pDigit :: Parser String
pDigit = some digitChar

sexpParser :: SExprParser Parser () String
sexpParser = plainSExprParser (pIdent <|> pDigit)

pSExpr :: Parser (Sexp String)
pSExpr = parseSExpr sexpParser

pDecodeOne :: Parser (Sexp String)
pDecodeOne = decodeOne sexpParser

pDecode :: Parser [Sexp String]
pDecode = decode sexpParser

pOptionalSpace :: Parser (Sexp String)
pOptionalSpace = decodeOne $ setSpacingRule spaceIsOptional sexpParser

parseTestTree :: TestTree
parseTestTree = testGroup "Parse/Generic.hs & Parse/Char.hs unit tests" $
  let tparse :: Parser a -> String -> Either String a
      tparse p s = first M.errorBundlePretty $ M.parse p "" s

      sExprTests :: (Eq a, Show a) => Parser a -> (Sexp String -> a) -> [TestTree]
      sExprTests p f = [
        let s = "()" in testCase (show s) $ tparse p s @?= (Right $ f (SList () [])),
        let s = "(   )" in testCase (show s) $ tparse p s @?= (Right $ f (SList () [])),
        let s = "foo" in testCase (show s) $ tparse p s @?= (Right $ f (SAtom "foo")),
        let s = "1234" in testCase (show s) $ tparse p s @?= (Right $ f (SAtom "1234")),
        let s = "(foo)" in testCase (show s) $ tparse p s @?= (Right $ f (SList () [SAtom "foo"])),
        let s = "( foo)" in testCase (show s) $ tparse p s @?= (Right $ f (SList () [SAtom "foo"])),
        let s = "(foo )" in testCase (show s) $ tparse p s @?= (Right $ f (SList () [SAtom "foo"])),
        let s = "(foo bar baz)"
        in testCase (show s) $ tparse p s @?= (Right $ f (SList () [SAtom "foo", SAtom "bar", SAtom "baz"])),
        let s = "(foo (bar baz))"
        in testCase (show s) $ tparse p s @?=
           (Right $ f (SList () [SAtom "foo", SList () [SAtom "bar", SAtom "baz"]])),
        let s = "(foo(bar baz))"
        in testCase (show s) $ tparse p s @?=
           (Right $ f (SList () [SAtom "foo", SList () [SAtom "bar", SAtom "baz"]])),
        let s = "((foo bar)baz)"
        in testCase (show s) $ tparse p s @?=
           (Right $ f (SList () [SList () [SAtom "foo", SAtom "bar"], SAtom "baz"])),
        let s = "(foo1234)"
        in testCase (show s) $ (isLeft $ tparse p "(foo1234)") @? "Parsing must fail. foo and 1234 are not separated by whitespace"
        ]
                         
      decodeCommon :: (Eq a, Show a) => Parser a -> (Sexp String -> a) -> [TestTree]
      decodeCommon p f = [
        let s = " () " in testCase (show s) $ tparse p s @?= (Right $ f (SList () [])),
        let s = " ()" in testCase (show s) $ tparse p s @?= (Right $ f (SList () [])),
        let s = "() " in testCase (show s) $ tparse p s @?= (Right $ f (SList () [])),
        let s = "   ()   " in testCase (show s) $ tparse p s @?= (Right $ f (SList () []))
        ]
  in
    [
      testGroup "parseSExpr" $ sExprTests pSExpr id ++
      [
        let s = " foo"
        in testCase (show s) $ (isLeft $ tparse pSExpr s) @? "Parsing must fail. parseSExpr should not parse whitespace"
      ],
      testGroup "decondeOne" $ sExprTests pDecodeOne id ++
      decodeCommon pDecodeOne id ++
      [
        let s = "() err" in testCase (show s) $ (isLeft $ tparse pDecodeOne s) @? "Parsing must fail. 2 SExpr",
        let s = "err ()" in testCase (show s) $ (isLeft $ tparse pDecodeOne s) @? "Parsing must fail. 2 SExpr",
        let s = "()err" in testCase (show s) $ (isLeft $ tparse pDecodeOne s) @? "Parsing must fail. 2 SExpr",
        let s = "err()" in testCase (show s) $ (isLeft $ tparse pDecodeOne s) @? "Parsing must fail. 2 SExpr"
      ],
     testGroup "decode" $ sExprTests pDecode (\x -> [x]) ++
     decodeCommon pDecode (\x -> [x]) ++
      [
        let s = "()()" in testCase (show s) $ tparse pDecode s @?= Right [SList () [], SList () []],
        let s = " ()()" in testCase (show s) $ tparse pDecode s @?= Right [SList () [], SList () []],
        let s = "() ()" in testCase (show s) $ tparse pDecode s @?= Right [SList () [], SList () []],
        let s = "()() " in testCase (show s) $ tparse pDecode s @?= Right [SList () [], SList () []],
        let s = "  ()  ()  " in testCase (show s) $ tparse pDecode s @?= Right [SList () [], SList () []],
        let s = "(foo)(1234)" in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SList () [SAtom "1234"]],
        let s = " (foo)(1234)" in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SList () [SAtom "1234"]],
        let s = "(foo) (1234)" in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SList () [SAtom "1234"]],
        let s = "(foo)(1234) " in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SList () [SAtom "1234"]],
        let s = "  (foo)  (1234)  " in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SList () [SAtom "1234"]],
        let s = "(foo) 1234" in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SAtom "1234"],
        let s = "foo 1234" in testCase (show s) $ tparse pDecode s @?= Right [SAtom "foo", SAtom "1234"],
        let s = "foo(1234)" in testCase (show s) $ tparse pDecode s @?= Right [SAtom "foo", SList () [SAtom "1234"]],
        let s = "(foo)1234" in testCase (show s) $ tparse pDecode s @?= Right [SList () [SAtom "foo"], SAtom "1234"],
        let s = "bar1234"
        in testCase (show s) $ (isLeft $ tparse pDecode s) @? "Parsing must fail. bar and 1234 are not separated by whitespace"
      ],
     testGroup "spaceIsOptional" $ [
        let s = "(foo1234)"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList () [SAtom "foo", SAtom "1234"])),
        let s = "(foo 1234)"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList () [SAtom "foo", SAtom "1234"])),
        let s = "(foo1234 bar)"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList () [SAtom "foo", SAtom "1234", SAtom "bar"])),
        let s = "( foo1234 )"
        in testCase (show s) $ tparse pOptionalSpace s @?= (Right (SList () [SAtom "foo", SAtom "1234"]))
        ]
  ]
