{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SchemeR5RS_Unittests (
  r5rsTestTree
  )where

import Data.Void
import qualified Data.Text as T
import Data.Either
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
--import Text.Megaparsec.Char
import Data.SExpresso.SExpr
import Data.SExpresso.Parse
import Data.SExpresso.Language.SchemeR5RS as R5

type Parser = Parsec Void T.Text

pSExpr :: Parser [SExpr R5.SExprType R5.SchemeToken]
pSExpr = decode R5.sexpr

-- tparse parses the whole input
tparse :: Parser a -> T.Text -> Either String a
tparse p s = first errorBundlePretty $ parse (p <* eof) "" s

-- Runs a test that must succeed. Accepts the parser and
-- the string to parse and the expected value as tuple
okTest :: (Eq a, Show a) => Parser a -> (T.Text, a) -> TestTree
okTest p (str, expected) = testCase ("Should parse " ++ show str)
                         $ tparse p str @?= Right expected

okDatum :: (T.Text, [Datum]) -> TestTree
okDatum (t, d) = okTest (sexpr2Datum <$> pSExpr) (t, Right d)

-- Runs a test that must fail. Accepts the parser
-- and the string to parse
koTest :: Parser a -> T.Text -> TestTree
koTest p str = testCase ("Should not parse " ++ show str)
             $ (isLeft $ tparse p str) @?
               ("Parsing must fail on " ++ show str)

koDatum :: T.Text -> TestTree
koDatum str = testCase ("Should not parse " ++ show str)
             $ (isLeft $ (tparse pSExpr str >>= sexpr2Datum)) @?
               ("Parsing must fail on " ++ show str)

-- Tests about whitespace that must succeed (see okTest)
okWhitespace :: [(T.Text, ())]
okWhitespace = map (,()) [" ", "\t", "\n", "\r\n"]

-- Tests about comment that must succeed (see okTest)
okComment :: [(T.Text, ())]
okComment = map (,()) [";", ";hello world", ";hello\n", ";abcdef\r\n"]

okInterTokenSpace1 :: [(T.Text, ())]
okInterTokenSpace1 = okWhitespace
                  ++ okComment
                  ++ map (,()) [" ;comment\n    ",
                     "\t\n;comment   \n   ",
                     "\n\n\n\n\n",
                     "\r\n;Hello World",
                     "  ;comment\n   ;comment"]

okInterTokenSpace :: [(T.Text, ())]
okInterTokenSpace = okInterTokenSpace1 ++ map (,()) [""]

okChar :: [(T.Text, Char)]
okChar = [("#\\t", 't'),
          ("#\\a", 'a'),
          ("#\\space", ' '),
          ("#\\newline", '\n'),
          ("#\\\n", '\n'),
          ("#\\ ", ' '),
          ("#\\\t", '\t')]

okBool :: [(T.Text, Bool)]
okBool = [("#t", True),
          ("#f", False)]

okIdentifier :: [(T.Text, T.Text)]
okIdentifier = map (\x -> (x,x)) ["foo", "x2", "!hot!", "+", "-", "...",
                                  "helloWorld"]

okString :: [(T.Text, T.Text)]
okString = [("\"abc def ghi\"", "abc def ghi"),
            ("\"\"", ""),
            ("\"\n\"", "\n"),
            ("\" \"", " "),
            ("\"\t\"", "\t"),
            (T.pack ['"','\\','\\','"'], "\\"),
            (T.pack ['"','\\','"','"'], "\"")]

okNumber :: [(T.Text, SchemeNumber)]
okNumber =  [("-1", SchemeNumber Exact $
                    CReal (SInteger Minus (UInteger 1))),
    ("-0", SchemeNumber Exact $
                    CReal (SInteger Minus (UInteger 0 ))),
    ("0", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 0))),
    ("1", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),


    ("#e1", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),

    ("#i1", SchemeNumber Inexact $
                    CReal (SInteger Plus (UInteger 1))),

    ("#b1", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),
    ("#o1", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),
    ("#d1", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),
    ("#x1", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),
    ("#xa", SchemeNumber Exact $ CReal (SInteger Plus (UInteger 10))),
    ("#xb", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 11))),
    ("#xc", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 12))),
    ("#xd", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 13))),
    ("#xe", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 14))),
    ("#xf", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 15))),
    ("-0001", SchemeNumber Exact $
                    CReal (SInteger Minus (UInteger 1))),
    ("-0000", SchemeNumber Exact $
                    CReal (SInteger Minus (UInteger 0))),
    ("0000", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 0))),
    ("0001", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 1))),

    ("-1#", SchemeNumber Inexact $
                    CReal (SInteger Minus (UIntPounds 1 1))),
    ("-0#", SchemeNumber Inexact $
                    CReal (SInteger Minus (UIntPounds 0 1))),
    ("0#", SchemeNumber Inexact $
                    CReal (SInteger Plus (UIntPounds 0 1))),
    ("1#", SchemeNumber Inexact $
                    CReal (SInteger Plus (UIntPounds 1 1))),

    ("-1###", SchemeNumber Inexact $
                    CReal (SInteger Minus (UIntPounds 1 3))),
    ("-0###", SchemeNumber Inexact $
                    CReal (SInteger Minus (UIntPounds 0 3))),
    ("0###", SchemeNumber Inexact $
                    CReal (SInteger Plus (UIntPounds 0 3))),
    ("1###", SchemeNumber Inexact $
                    CReal (SInteger Plus (UIntPounds 1 3))),

    ("-12345", SchemeNumber Exact $
                    CReal (SInteger Minus (UInteger 12345))),
    ("12345", SchemeNumber Exact $
                    CReal (SInteger Plus (UInteger 12345))),

    ("-12345/5", SchemeNumber Exact $
                    CReal (SRational Minus (UInteger 12345) (UInteger 5))),
    ("12345/5", SchemeNumber Exact $
                    CReal (SRational Plus (UInteger 12345) (UInteger 5))),
    ("-12345#/5", SchemeNumber Inexact $
                    CReal (SRational Minus (UIntPounds 12345 1) (UInteger 5))),
    ("12345/5##", SchemeNumber Inexact $
                    CReal (SRational Plus (UInteger 12345) (UIntPounds 5 2))),
    ("-12345##/5", SchemeNumber Inexact $
                    CReal (SRational Minus (UIntPounds 12345 2) (UInteger 5))),
    ("12345####/5#", SchemeNumber Inexact $
                    CReal (SRational Plus (UIntPounds 12345 4) (UIntPounds 5 1))),


    ("-12345.0", SchemeNumber Inexact $
                    CReal (SDecimal Minus (UInteger 12345) (UInteger 0) Nothing)),
    (".0", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 0) (UInteger 0) Nothing)),
    ("0.", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 0) (UInteger 0) Nothing)),

    ("0.###", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 0) (UPounds 3) Nothing)),
    ("-.569", SchemeNumber Inexact $
                    CReal (SDecimal Minus (UInteger 0) (UInteger 569) Nothing)),
    ("-245#.", SchemeNumber Inexact $
                    CReal (SDecimal Minus (UIntPounds 245 1) (UPounds 0) Nothing)),
    ("#e-.569", SchemeNumber Exact $
                    CReal (SDecimal Minus (UInteger 0) (UInteger 569) Nothing)),
    ("1e10", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 1)
                                                       (UInteger 0)
                                                       (Just $ Suffix PDefault Plus 10))),
    ("1e-10", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 1)
                                                       (UInteger 0)
                                                       (Just $ Suffix PDefault Minus 10))),
    ("1s10", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 1)
                                                       (UInteger 0)
                                                       (Just $ Suffix PShort Plus 10))),
    ("1f10", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 1)
                                                       (UInteger 0)
                                                       (Just $ Suffix PSingle Plus 10))),
    ("1d10", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 1)
                                                       (UInteger 0)
                                                       (Just $ Suffix PDouble Plus 10))),
    ("1l10", SchemeNumber Inexact $
                    CReal (SDecimal Plus (UInteger 1)
                                                       (UInteger 0)
                                                       (Just $ Suffix PLong Plus 10))),

    ("1+i", SchemeNumber Exact $
                    CAbsolute (SInteger Plus (UInteger 1)) (SInteger Plus (UInteger 1))),

    ("1-i", SchemeNumber Exact $
                    CAbsolute (SInteger Plus (UInteger 1)) (SInteger Minus (UInteger 1))),

    ("0.5+i", SchemeNumber Inexact $
                    CAbsolute (SDecimal Plus (UInteger 0)
                                                  (UInteger 5)
                                                  Nothing) (SInteger Plus (UInteger 1))),

    ("-8i", SchemeNumber Exact $
                    CAbsolute (SInteger Plus (UInteger 0)) (SInteger Minus (UInteger 8))),


    ("-8.25i", SchemeNumber Inexact $
                    CAbsolute (SInteger Plus (UInteger 0)) (SDecimal Minus (UInteger 8)
                                                                                    (UInteger 25)
                                                                                    Nothing)),
    ("0@25", SchemeNumber Exact $
                    CAngle (SInteger Plus (UInteger 0)) (SInteger Plus (UInteger 25))),

    ("1/4@-25", SchemeNumber Exact $
                    CAngle (SRational Plus (UInteger 1) (UInteger 4)) (SInteger Minus (UInteger 25))),

    ("1#/4@-25##", SchemeNumber Inexact $
                    CAngle (SRational Plus (UIntPounds 1 1) (UInteger 4)) (SInteger Minus (UIntPounds 25 2)))]

-- Returns all the "ok..." series of tests except the one provided
-- as input. Since R5RS grammar is non ambiguous, a token parser should not be able
-- to parse the valid input of other token parser.
mkKoTest :: [T.Text] -> [T.Text]
mkKoTest goodStr =
  let allTests = map fst okInterTokenSpace ++
                 map fst okChar ++
                 map fst okBool ++
                 map fst okIdentifier ++
                 map fst okString ++
                 map fst okNumber
  in filter (not . (`elem` goodStr)) (alwaysBad ++ allTests)

-- Always bad string input
alwaysBad :: [T.Text]
alwaysBad = ["#", "#T", "#F", "#true", "#false"] ++
          -- Ill formated number
            ["#b3", "#o9", "#da", "#xA", "#b1.1", "#o1.1", "#x1.1", "#b.1",
             "#o.1", "#x.1", "123##.12"]

r5rsTestTree :: TestTree
r5rsTestTree = testGroup "Language/R5RS.hs" $ [
  testGroup "whitespace" $
      map (okTest R5.whitespace) okWhitespace ++
      map (koTest R5.whitespace) (mkKoTest $ map fst okWhitespace),
  testGroup "comment" $
      map (okTest R5.comment) okComment ++
      map (koTest R5.comment) (mkKoTest $ map fst okComment),
  testGroup "interTokenSpace" $
      map (okTest R5.interTokenSpace) okInterTokenSpace ++
      map (koTest R5.interTokenSpace) (mkKoTest $ map fst okInterTokenSpace),
  testGroup "interTokenSpace1" $
      map (okTest R5.interTokenSpace1) okInterTokenSpace1 ++
      map (koTest R5.interTokenSpace1) (mkKoTest $ map fst okInterTokenSpace1),
  testGroup "character" $
      map (okTest R5.character) okChar ++
      map (koTest R5.character) (mkKoTest $ map fst okChar),
  testGroup "boolean" $
      map (okTest R5.boolean) okBool ++
      map (koTest R5.boolean) (mkKoTest $ map fst okBool),
  testGroup "identifier" $
        map (okTest R5.identifier) okIdentifier ++
        map (koTest R5.identifier) (mkKoTest $ map fst okIdentifier),
  testGroup "string" $
        map (okTest R5.stringParser) okString ++
        map (koTest R5.stringParser) (mkKoTest $ map fst okString),
  testGroup "number" $
        map (okTest R5.number) okNumber ++
        map (koTest R5.number) (mkKoTest $ map fst okNumber),

    testGroup "datum" $
      map okDatum (map (fmap ( (:[]) . DChar)) okChar) ++
      map okDatum (map (fmap ( (:[]) . DBoolean)) okBool) ++
      map okDatum (map (fmap ( (:[]) . DIdentifier)) okIdentifier) ++
      map okDatum (map (fmap ( (:[]) . DString)) okString) ++
      map okDatum (map (fmap ( (:[]) . DNumber)) okNumber) ++
      map okDatum
      [("(foo #\\a)", [DList [DIdentifier "foo", DChar 'a']]),
       ("(foo #\\a) \"hello\"", [DList [DIdentifier "foo", DChar 'a'], DString "hello"]),
       ("'foo", [DQuote (DIdentifier "foo")]),
       ("`foo", [DQuasiquote (DIdentifier "foo")]),
       ("`(foo ,a)", [DQuasiquote (DList [DIdentifier "foo", DComma (DIdentifier "a")])]),
       ("`(foo , a)", [DQuasiquote (DList [DIdentifier "foo", DComma (DIdentifier "a")])]),
       ("`(foo, a)", [DQuasiquote (DList [DIdentifier "foo", DComma (DIdentifier "a")])]),
       ("`(foo ,@a)", [DQuasiquote (DList [DIdentifier "foo", DCommaAt (DIdentifier "a")])]),
       ("`(foo ,@ a)", [DQuasiquote (DList [DIdentifier "foo", DCommaAt (DIdentifier "a")])]),
       ("`(foo,@ a)", [DQuasiquote (DList [DIdentifier "foo", DCommaAt (DIdentifier "a")])]),
       ("(foo . a)", [DDotList [DIdentifier "foo"] (DIdentifier "a")]),
       ("(foo a b c . d)",
        [DDotList [DIdentifier "foo", DIdentifier "a", DIdentifier "b", DIdentifier "c"] (DIdentifier "d")])]
      ++
      map koDatum ["(foo .)", "(foo ')", "(foo `)", "(foo ,)", "(foo ,@)", "(foo a b . c d)"]
  ]
