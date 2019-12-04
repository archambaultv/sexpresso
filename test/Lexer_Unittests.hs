module Lexer_Unittests (
  lexerTestTree
  )where

import Data.Void
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Combinators
import qualified Data.SExpresso.Parse.Lexer as L

type Parser = Parsec Void String

asciiLetter :: Parser Char
asciiLetter = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])

pAtom :: Parser String
pAtom = some asciiLetter <|> some digitChar

lexer :: Parser [L.SExprTokenPos Char Char String]
lexer = L.sexprLexer (char '(') (char ')') pAtom space1 sepIsMandatory <* eof

stream :: Parser (L.SExprStream String Char Char String)
stream = L.sexprStream (char '(') (char ')') pAtom space1 sepIsMandatory

str :: String
str = "(foo 1 2) (p 42 bar)\n(baz 25)\nx"

lexerTestTree :: TestTree
lexerTestTree = testGroup "Parse/Combinator.hs unit tests" $
  let tparse :: Parser a -> String -> Either String a
      tparse p s = first M.errorBundlePretty $ M.parse p "" s
  in
    [
      testGroup "lexer" 
      [
        
        let s = "" in testCase (show s) $ tparse lexer s @?= (Right $ []),
        let s = "       " in testCase (show s) $ tparse lexer s @?= (Right $ []),
        let s = "()" in testCase (show s) $ tparse lexer s @?= (Right $ [((0,1), L.OpenDelimiter '('),
                                                                          ((1,1), L.CloseDelimiter ')')]),
        let s = "() ()" in testCase (show s) $ tparse lexer s @?= (Right $ [((0,1), L.OpenDelimiter '('),
                                                                            ((1,1), L.CloseDelimiter ')'),
                                                                            ((3,1), L.OpenDelimiter '('),
                                                                            ((4,1), L.CloseDelimiter ')')]),
        let s = " ( ) " in testCase (show s) $ tparse lexer s @?= (Right $ [((1,1), L.OpenDelimiter '('),
                                                                            ((3,1), L.CloseDelimiter ')')]),
        let s = " ) ( " in testCase (show s) $ tparse lexer s @?= (Right $ [((1,1), L.CloseDelimiter ')'),
                                                                            ((3,1), L.OpenDelimiter '(')]),
        let s = "(foo 1 2)" in testCase (show s) $ tparse lexer s @?= (Right $ [((0,1), L.OpenDelimiter '('),
                                                                                ((1,3), L.AtomToken "foo"),
                                                                                ((5,1), L.AtomToken "1"),
                                                                                ((7,1), L.AtomToken "2"),
                                                                                ((8,1), L.CloseDelimiter ')')]),
        let s = " foo ( 12 " in testCase (show s) $ tparse lexer s @?= (Right $ [((1,3), L.AtomToken "foo"),
                                                                                ((5,1), L.OpenDelimiter '('),
                                                                                ((7,2), L.AtomToken "12")])                                                        
      ],
      testGroup "stream"
      --str = "(foo 1 2) (p 42 bar)\n(baz 25)\nx"
      [
        testCase "take1_" $ ((fmap fst . take1_) <$> tparse stream str) @?= (Right $ Just $ L.OpenDelimiter '('),
        testCase "takeN_" $ ((fmap fst . takeN_ 3) <$> tparse stream str) @?= (Right $ Just $ [L.OpenDelimiter '(',
                                                                                               L.AtomToken "foo",
                                                                                               L.AtomToken "1"]),
        testCase "inputOffset - 1" $ (L.inputOffset <$> tparse stream str) @?= (Right $ 0),
        testCase "inputOffset - 2" $ (fmap (L.inputOffset . snd) . take1_ <$> tparse stream str) @?= (Right $ Just 1),
        testCase "inputOffset - 3" $ (fmap (L.inputOffset . snd) . takeN_ 2 <$> tparse stream str) @?= (Right $ Just 5),
        testCase "inputOffset - 4" $ (L.inputOffset . snd . takeWhile_ (const True) <$> tparse stream str) @?= (Right $ length str)
      ]
    ]
