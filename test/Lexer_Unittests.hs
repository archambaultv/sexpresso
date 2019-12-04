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
lexer = L.sexprLexer (char '(') (char ')') pAtom space1 sepIsMandatory

lexerTestTree :: TestTree
lexerTestTree = testGroup "Parse/Combinator.hs unit tests" $
  let tparse :: Parser a -> String -> Either String a
      tparse p s = first M.errorBundlePretty $ M.parse p "" s

  in
    [
      testGroup "lexer" 
      [
        let s = "()" in testCase (show s) $ tparse lexer s @?= (Right $ [((0,1), L.OpenDelimiter '('),
                                                                          ((1,1), L.CloseDelimiter ')')]),
        let s = " ( ) " in testCase (show s) $ tparse lexer s @?= (Right $ [((1,1), L.OpenDelimiter '('),
                                                                          ((3,1), L.CloseDelimiter ')')])
      ]
    ]
