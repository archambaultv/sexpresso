{-# LANGUAGE OverloadedStrings #-}

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

pSExpr :: Parser (SExpr R5.SExprType R5.SchemeToken)
pSExpr = parseSExpr R5.sexpr

pDecodeOne :: Parser (SExpr R5.SExprType R5.SchemeToken)
pDecodeOne = decodeOne R5.sexpr

pDecode :: Parser [SExpr R5.SExprType R5.SchemeToken]
pDecode = decode R5.sexpr

-- tparse parses the whole input
tparse :: Parser a -> T.Text -> Either String a
tparse p s = first errorBundlePretty $ parse (p <* eof) "" s
  
r5rsTestTree :: TestTree
r5rsTestTree = testGroup "Language/R5RS.hs" $ [
  testGroup "whitespace" $ [
      let s = " " in testCase (show s) $ tparse R5.whitespace s @?= Right (),
      let s = "\t" in testCase (show s) $ tparse R5.whitespace s @?= Right (),
      let s = "\n" in testCase (show s) $ tparse R5.whitespace s @?= Right (),
      let s = "\r\n" in testCase (show s) $ tparse R5.whitespace s @?= Right (),
      let s = "" in testCase (show s) $ (isLeft $ tparse R5.whitespace s) @? "Parsing must fail on empty input",
      let s = "a" in testCase (show s) $ (isLeft $ tparse R5.whitespace s) @? "Parsing must fail on a",
      let s = ";" in testCase (show s) $ (isLeft $ tparse R5.whitespace s) @? "Parsing must fail on ;"
      ],
  testGroup "comment" $ [
      let s = ";" in testCase (show s) $ tparse R5.comment s @?= Right (),
      let s = ";hello world" in testCase (show s) $ tparse R5.comment s @?= Right (),
      let s = ";hello\n" in testCase (show s) $ tparse R5.comment s @?= Right (),
      let s = ";abcdef\r\n" in testCase (show s) $ tparse R5.comment s @?= Right (),
      let s = "" in testCase (show s) $ (isLeft $ tparse R5.comment s) @? "Parsing must fail on empty input",
      let s = "a" in testCase (show s) $ (isLeft $ tparse R5.comment s) @? "Parsing must fail on a",
      let s = "#t" in testCase (show s) $ (isLeft $ tparse R5.comment s) @? "Parsing must fail on #t"
      ],
  testGroup "interTokenSpace" $ [
      let s = ";" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = ";hello world" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = ";hello\n" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = ";abcdef\r\n" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = " " in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "\t" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "\n" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "\r\n" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = " ;comment\n    " in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "\t\n;comment   \n   " in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "\n\n\n\n\n" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "\r\n;Hello World" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right (),
      let s = "" in testCase (show s) $ tparse R5.interTokenSpace s @?= Right ()
      ],
  testGroup "interTokenSpace1" $ [
      let s = ";" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = ";hello world" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = ";hello\n" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = ";abcdef\r\n" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = " " in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "\t" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "\n" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "\r\n" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = " ;comment\n    " in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "\t\n;comment   \n   " in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "\n\n\n\n\n" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "\r\n;Hello World" in testCase (show s) $ tparse R5.interTokenSpace1 s @?= Right (),
      let s = "" in testCase (show s) $ (isLeft $ tparse R5.interTokenSpace1 s) @? "Parsing must fail on empty input",
      let s = "1234" in testCase (show s) $ (isLeft $ tparse R5.interTokenSpace1 s) @? "Parsing must fail on 1234",
      let s = "a" in testCase (show s) $ (isLeft $ tparse R5.interTokenSpace1 s) @? "Parsing must fail on a",
      let s = "#t" in testCase (show s) $ (isLeft $ tparse R5.interTokenSpace1 s) @? "Parsing must fail on #t"
      ],
  testGroup "character" $ [
      let s = "#\\t" in testCase (show s) $ tparse R5.character s @?= Right 't',
      let s = "#\\a" in testCase (show s) $ tparse R5.character s @?= Right 'a',
      let s = "#\\space" in testCase (show s) $ tparse R5.character s @?= Right ' ',
      let s = "#\\newline" in testCase (show s) $ tparse R5.character s @?= Right '\n',
      let s = "#\\\n" in testCase (show s) $ tparse R5.character s @?= Right '\n',
      let s = "#\\ " in testCase (show s) $ tparse R5.character s @?= Right ' ',
      let s = "#\\\t" in testCase (show s) $ tparse R5.character s @?= Right '\t',
      let s = "" in testCase (show s) $ (isLeft $ tparse R5.character s) @? "Parsing must fail on empty input",
      let s = "#t" in testCase (show s) $ (isLeft $ tparse R5.character s) @? "Parsing must fail on #t",
      let s = "#f" in testCase (show s) $ (isLeft $ tparse R5.character s) @? "Parsing must fail on #f"
      ],
  testGroup "boolean" $ [
      let s = "#t" in testCase (show s) $ tparse R5.boolean s @?= Right True,
      let s = "#f" in testCase (show s) $ tparse R5.boolean s @?= Right False,
      let s = "" in testCase (show s) $ (isLeft $ tparse R5.boolean s) @? "Parsing must fail on empty input",
      let s = "t" in testCase (show s) $ (isLeft $ tparse R5.boolean s) @? "Parsing must fail on t",
      let s = "f" in testCase (show s) $ (isLeft $ tparse R5.boolean s) @? "Parsing must fail on f"
      ],
  testGroup "identifier" $ [
      let s = "foo" in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "x2" in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "!hot!" in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "+" in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "-" in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "..." in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "helloWorld" in testCase (show s) $ tparse R5.identifier s @?= Right s,
      let s = "" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on empty input",
      let s = "#t" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on #t",
      let s = "#f" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on #f",
      let s = "123" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on 123",
      let s = "+123" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on +123",
      let s = "-123" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on -123",
      let s = "+i" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on +i",
      let s = "-i" in testCase (show s) $ (isLeft $ tparse R5.identifier s) @? "Parsing must fail on -i"
      ],
    testGroup "string" $ [
      let s = "\"abc def ghi\"" in testCase (show s) $ tparse R5.stringParser s @?= Right "abc def ghi",
      let s = "\"\"" in testCase (show s) $ tparse R5.stringParser s @?= Right "",
      let s = "\"\n\"" in testCase (show s) $ tparse R5.stringParser s @?= Right "\n",
      let s = "\" \"" in testCase (show s) $ tparse R5.stringParser s @?= Right " ",
      let s = "\"\t\"" in testCase (show s) $ tparse R5.stringParser s @?= Right "\t",
      let s = T.pack ['"','\\','\\','"'] in testCase (show s) $ tparse R5.stringParser s @?= Right "\\",
      let s = T.pack ['"','\\','"','"'] in testCase (show s) $ tparse R5.stringParser s @?= Right "\"",
      let s = "#t" in testCase (show s) $ (isLeft $ tparse R5.stringParser s) @? "Parsing must fail on #t",
      let s = "#f" in testCase (show s) $ (isLeft $ tparse R5.stringParser s) @? "Parsing must fail on #f"
      ],
    testGroup "number" $ [
      let s = "-1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 1 0))),
      let s = "-0" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 0 0))),
      let s = "0" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 0 0))),
      let s = "1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),

      
      let s = "#e1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber (Just Exact) $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),
        
      let s = "#i1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber (Just Inexact) $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),
        
      let s = "#b1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),
      let s = "#o1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),
      let s = "#d1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),
      let s = "#x1" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),
      let s = "#xa" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                                                                   ComplexReal (SReal Plus (UInt $ UInteger 10 0))),
      let s = "#xb" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 11 0))),
      let s = "#xc" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 12 0))),
      let s = "#xd" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 13 0))),
      let s = "#xe" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 14 0))),
      let s = "#xf" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 15 0))),
      let s = "-0001" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 1 0))),
      let s = "-0000" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 0 0))),
      let s = "0000" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 0 0))),
      let s = "0001" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 0))),

      let s = "-1#" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 1 1))),
      let s = "-0#" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 0 1))),
      let s = "0#" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 0 1))),
      let s = "1#" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 1))),

      let s = "-1###" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 1 3))),
      let s = "-0###" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 0 3))),
      let s = "0###" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 0 3))),
      let s = "1###" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 1 3))),

      let s = "-12345" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UInt $ UInteger 12345 0))),
      let s = "12345" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UInt $ UInteger 12345 0))),

      let s = "-12345/5" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (URational (UInteger 12345 0) (UInteger 5 0)))),
      let s = "12345/5" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (URational (UInteger 12345 0) (UInteger 5 0)))),
      let s = "-12345#/5" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (URational (UInteger 12345 1) (UInteger 5 0)))),
      let s = "12345/5##" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (URational (UInteger 12345 0) (UInteger 5 2)))),
      let s = "-12345##/5" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (URational (UInteger 12345 2) (UInteger 5 0)))),
      let s = "12345####/5#" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (URational (UInteger 12345 4) (UInteger 5 1)))),


      let s = "-12345.0" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UDecimal (UInteger 12345 0)
                                                         (Left (UInteger 0 0))
                                                         Nothing))),
      let s = ".0" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 0 0)
                                                         (Left (UInteger 0 0))
                                                         Nothing))),
      let s = "0." in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 0 0)
                                                         (Left (UInteger 0 0))
                                                         Nothing))),
        
      let s = "0.###" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 0 0)
                                                         (Left (UInteger 0 3))
                                                         Nothing))),
      let s = "-.569" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Minus (UDecimal (UInteger 0 0)
                                                         (Left (UInteger 569 0))
                                                         Nothing))),
      let s = "1e10" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 1 0)
                                                         (Left (UInteger 0 0))
                                                         (Just $ Suffix PDefault Plus 10)))),
      let s = "1e-10" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 1 0)
                                                         (Left (UInteger 0 0))
                                                         (Just $ Suffix PDefault Minus 10)))),
      let s = "1s10" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 1 0)
                                                         (Left (UInteger 0 0))
                                                         (Just $ Suffix PShort Plus 10)))),
      let s = "1f10" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 1 0)
                                                         (Left (UInteger 0 0))
                                                         (Just $ Suffix PSingle Plus 10)))),
      let s = "1d10" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 1 0)
                                                         (Left (UInteger 0 0))
                                                         (Just $ Suffix PDouble Plus 10)))),
      let s = "1l10" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexReal (SReal Plus (UDecimal (UInteger 1 0)
                                                         (Left (UInteger 0 0))
                                                         (Just $ Suffix PLong Plus 10)))),
        
      let s = "1+i" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAbsolute (SReal Plus (UInt $ UInteger 1 0)) (SReal Plus (UInt $ UInteger 1 0))),
        
      let s = "1-i" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAbsolute (SReal Plus (UInt $ UInteger 1 0)) (SReal Minus (UInt $ UInteger 1 0))),

      let s = "0.5+i" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAbsolute (SReal Plus (UDecimal (UInteger 0 0)
                                                    (Left (UInteger 5 0))
                                                    Nothing)) (SReal Plus (UInt $ UInteger 1 0))),
                                                                     
      let s = "-8i" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) (SReal Minus (UInt $ UInteger 8 0))),

      
      let s = "-8.25i" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) (SReal Minus (UDecimal (UInteger 8 0)
                                                                                      (Left (UInteger 25 0))
                                                                                      Nothing))),
      let s = "0@25" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAngle (SReal Plus (UInt $ UInteger 0 0)) (SReal Plus (UInt $ UInteger 25 0))),

      let s = "1/4@-25" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAngle (SReal Plus (URational (UInteger 1 0) (UInteger 4 0))) (SReal Minus (UInt $ UInteger 25 0))),


      let s = "1#/4@-25##" in testCase (show s) $ tparse R5.number s @?= (Right $ SchemeNumber Nothing $
                      ComplexAngle (SReal Plus (URational (UInteger 1 1) (UInteger 4 0))) (SReal Minus (UInt $ UInteger 25 2))),

      let s = "#b3" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #b3",
      let s = "#o9" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #o9",
      let s = "#da" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #da",
      let s = "#xA" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #xA",
      
      let s = "#b1.1" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #b1.1",
      let s = "#o1.1" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #o1.1",
      let s = "#x1.1" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #x1.1",
      let s = "#b.1" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #b.1",
      let s = "#o.1" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #o.1",
      let s = "#x.1" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #x.1",
      
      let s = "123##.12" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on 123##.12",
      let s = "#t" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #t",
      let s = "#f" in testCase (show s) $ (isLeft $ tparse R5.number s) @? "Parsing must fail on #f"
      ]
  ]
