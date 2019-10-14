{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Parsing library for some parts of the Scheme R5RS language
-- as defined in section 7 of the report
-- The library does parse tab and \r\n and whitespace
module Data.SExpresso.Language.SchemeR5RS (
  schemeR5RSTokenParser,
  token2Datum,
  whitespace,
  comment,
  interTokenSpace,
  interTokenSpace1,
  identifier,
  boolean,
  character,
  stringParser,
  Radix(..),
  Exactness(..),
  Sign(..),
  Pounds,
  UInteger(..),
  Precision(..),
  Suffix(..),
  UReal(..),
  SReal(..),
  Complex(..),
  SchemeNumber(..),
  number,
  SchemeToken(..),
  tokenParser,
  Datum(..)
  ) where

import Data.Void
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.SExpresso.SExpr
import Data.SExpresso.Parse

type Parser = Parsec Void T.Text

data SchemeToken = TBoolean Bool
                 | TNumber SchemeNumber
                 | TChar Char
                 | TString T.Text
                 | TIdentifier T.Text
                 | TQuote
                 | TQuasiquote
                 | TComma
                 | TCommaAt
                 | TDot

tokenParser :: Parser SchemeToken
tokenParser = (boolean >>= return . TBoolean) <|>
              (number >>= return . TNumber) <|>
              (character >>= return . TChar) <|>
              (stringParser >>= return . TString) <|>
              (identifier >>= return . TIdentifier) <|>
              (quote >> return TQuote) <|>
              (quasiquote >> return TQuasiquote) <|>
              (comma >> return TComma) <|>
              (commaAt >> return TCommaAt) <|>
              (dot >> return TDot)


r5RSspacingRule :: SchemeToken -> SpacingRule
r5RSspacingRule (TString _) = SOptional
r5RSspacingRule TQuote = SOptional
r5RSspacingRule TQuasiquote  = SOptional
r5RSspacingRule TComma = SOptional
r5RSspacingRule TCommaAt = SOptional
r5RSspacingRule _ = SMandatory

data SExprType = STList | STVector

schemeR5RSTokenParser :: SExprParser Parser SExprType SExprType SchemeToken
schemeR5RSTokenParser =
  let sTag = (single '(' >> return STList) <|> (chunk "#(" >> return STVector)
      eTag = \t -> single ')' >> return t
  in SExprParser sTag eTag tokenParser interTokenSpace1 (mkSpacingRule r5RSspacingRule)
          
data Datum = DBoolean Bool
           | DNumber SchemeNumber
           | DChar Char
           | DString T.Text
           | DIdentifier T.Text
           | DList [Datum]
           | DDotList [Datum] Datum
           | DQuote Datum
           | DQuasiquote Datum
           | DComma Datum
           | DCommaAt Datum
           | DVector [Datum]

token2Datum :: [SExpr SExprType SchemeToken] -> Either String [Datum]
token2Datum [] = Right []
token2Datum ((A (TBoolean x)) : xs) = (:) <$> pure (DBoolean x) <*> token2Datum xs
token2Datum ((A (TNumber x)) : xs) = (:) <$> pure (DNumber x) <*> token2Datum xs
token2Datum ((A (TChar x)) : xs) = (:) <$> pure (DChar x) <*> token2Datum xs
token2Datum ((A (TString x)) : xs) = (:) <$> pure (DString x) <*> token2Datum xs
token2Datum ((A (TIdentifier x)) : xs) = (:) <$> pure (DIdentifier x) <*> token2Datum xs
token2Datum ((A TQuote) : xs) = do
  xs' <- token2Datum xs
  if null xs'
  then Left "Expecting a datum after the quote."
  else return $ DQuote (head xs') : tail xs'
token2Datum ((A TQuasiquote) : xs) = do
  xs' <- token2Datum xs
  if null xs'
  then Left "Expecting a datum after the quasiquote."
  else return $ DQuasiquote (head xs') : tail xs'
token2Datum ((A TComma) : xs) = do
  xs' <- token2Datum xs
  if null xs'
  then Left "Expecting a datum after the comma."
  else return $ DQuote (head xs') : tail xs'
token2Datum ((A TCommaAt) : xs) = do
  xs' <- token2Datum xs
  if null xs'
  then Left "Expecting a datum after the quote."
  else return $ DCommaAt (head xs') : tail xs'
token2Datum ((A TDot) : _) = Left "Unexpected dot"
token2Datum ((SList STVector vs) : xs) = (:) <$> (token2Datum vs >>= return . DVector) <*> token2Datum xs
token2Datum ((SList STList ls) : xs) = (:) <$> (listToken2Datum ls) <*> token2Datum xs
  where listToken2Datum ys =
          let l = length ys
          in if l < 3
             then token2Datum ys >>= return . DList
             else let penultimate = head $ drop (l - 2) ys
                  in case penultimate of
                       (A TDot) ->
                         let last' = head $ drop (l - 1) ys
                             tokens' = take (l - 2) ys
                         in do
                           lastD <- token2Datum [last']
                           tokensD <- token2Datum tokens'
                           return $ DDotList tokensD (head lastD) 
                       _ -> token2Datum ys >>= return . DList


------------------------- Whitespace and comments -------------------------
whitespace :: Parser ()
whitespace = (char ' ' >> return ()) <|>
             (char '\t' >> return ()) <|>
             (eol >> return ())

comment :: Parser ()
comment = char ';' >> takeWhileP Nothing (\c -> c == '\n' || c == '\r') >> eol >> return ()

atmosphere :: Parser ()
atmosphere = whitespace <|> comment

interTokenSpace :: Parser ()
interTokenSpace = many atmosphere >> return ()

interTokenSpace1 :: Parser ()
interTokenSpace1 = some atmosphere >> return ()

------------------------- Identifier -------------------------

-- FixMe : Must not parse +i, -i, etc ... (they are numbers)
identifier :: Parser T.Text
identifier = standardIdentifier <|> peculiarIdentifier
  where standardIdentifier = do
          i <- oneOf initialList
          is <- takeWhileP Nothing (\c -> c `elem` subsequentList)
          return $ T.cons i is

initialList :: String
initialList = ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*/:<=>?^_~"

subsequentList :: String
subsequentList = initialList ++ ['0'..'9'] ++ "+-.@"

peculiarIdentifier :: Parser T.Text
peculiarIdentifier = string "+" <|> string "-" <|> string "..."

------------------------- Booleans -------------------------
boolean :: Parser Bool
boolean = (string "#t" >> return True) <|>
          (string "#f" >> return False)


------------------------- Character -------------------------
character :: Parser Char
character = do
  _ <- string "#\\"
  (string "newline" >> return '\n') <|> (string "space" >> return ' ') <|> anySingle

------------------------- String -------------------------
stringParser :: Parser T.Text
stringParser = do
  _ <- char '"'
  xs <- consume
  _ <- char '"'
  return $ L.toStrict $ B.toLazyText xs

  where consume :: Parser B.Builder
        consume = do
          x <- takeWhileP Nothing (\c -> c /= '\\' || c /= '"')
          c <- char '\\' <|> char '"'
          case c of
            '"' -> return $ B.fromText x
            _ -> do
               c1 <- char '\\' <|> char '"'
               case c1 of
                 '\\' -> do
                   x2 <- consume
                   return $ B.fromText x <> B.fromText "\\" <> x2
                 _ -> do
                   x2 <- consume
                   return $ B.fromText x <> B.fromText "\"" <> x2


------------------------- Numbers -------------------------
data Radix = R2 | R8 | R10 | R16
           deriving (Eq, Show)

data Exactness = Exact | Inexact

data Sign = Plus | Minus

-- The second integer indicates the number of # sign in the number
type Pounds = Integer

data UInteger = UInteger Integer Pounds

data Precision = PDefault | PShort | PSingle | PDouble | PLong

data Suffix = Suffix Precision Sign Integer

data UReal = UDecimal UInteger (Either UInteger Pounds) Suffix
           | URational UInteger UInteger
           | UInt UInteger

data SReal = SReal Sign UReal

data Complex = ComplexReal SReal
             | ComplexAngle SReal SReal
             | ComplexAbsolute SReal SReal

data SchemeNumber = SchemeNumber (Maybe Radix) (Maybe Exactness) Complex

number :: Parser SchemeNumber
number = do
  (r, e) <- prefix
  c <- complex (fromMaybe R10 r)
  return $ SchemeNumber r e c
 
complex :: Radix -> Parser Complex
complex r = (string "+i" >> (return $ ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) (SReal Plus (UInt $ UInteger 1 0)))) <|>
            (string "-i" >> (return $ ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) (SReal Minus (UInt $ UInteger 1 0)))) <|>
            withRealPart <|> noRealPart (SReal Plus (UInt $ UInteger 0 0))
  where withRealPart = do
          n1 <- real r
          c <- optional (char '@' <|> char '+' <|> char '-')
          case c of
            Nothing -> return $ ComplexReal n1
            Just '@' -> do
              n2 <- real r
              return $ ComplexAngle n1 n2
            Just _ -> noRealPart n1
          
        noRealPart n1 = do
          s <- (char '-' >> return Minus) <|> (char '+' >> return Plus)
          u <- optional (ureal r)
          _ <- char 'i'
          case u of
            Nothing -> return $ ComplexAbsolute n1 (SReal s (UInt $ UInteger 1 0))
            Just n2 -> return $ ComplexAbsolute n1 (SReal s n2)

real :: Radix -> Parser SReal
real r = do
  s <- option Plus sign
  u <- ureal r
  return $ SReal s u

ureal :: Radix -> Parser UReal
ureal r =
  case r of
    R10 -> dotN <|> (do
      u1 <- uinteger r
      case u1 of
        UInteger _ nbPounds | nbPounds > 0 -> do
            mc <- optional (char '/' <|> char '.')
            case mc of
              Nothing -> return $ UInt u1
              Just '/' -> rational u1
              Just _ -> do
                pounds <- takeWhileP Nothing (== '#')
                s <- suffix
                return $ UDecimal u1 (Right $ toInteger $ T.length pounds) s
        UInteger _ _ -> do
          mc <- optional (char '/' <|> char '.')
          case mc of
            Nothing -> return $ UInt u1
            Just '/' -> rational u1
            Just _ -> do
              n <- option 0 (udigit R10)
              pounds <- takeWhileP Nothing (== '#')
              s <- suffix
              return $ UDecimal u1 (Left $ UInteger n (toInteger $ T.length pounds)) s 
          )
                
    _ -> intOrRational

  where
        intOrRational :: Parser UReal
        intOrRational = do
          u1 <- uinteger r
          mc <- optional (char '/')
          case mc of
            Nothing -> return $ UInt u1
            Just _ -> rational u1

        rational :: UInteger -> Parser UReal
        rational u1 = do
          u2 <- uinteger r
          return $ URational u1 u2

        dotN :: Parser UReal
        dotN = do
          _ <- char '.'
          n <- udigit R10
          pounds <- takeWhileP Nothing (== '#')
          s <- suffix
          return $ UDecimal (UInteger 0 0) (Left $ UInteger n (toInteger $ T.length pounds)) s

uinteger :: Radix -> Parser UInteger
uinteger r = do
  n <- udigit r
  pounds <- takeWhileP Nothing (== '#')
  return $ UInteger n (toInteger $ T.length pounds)
  

prefix :: Parser (Maybe Radix, Maybe Exactness)
prefix = do
  _ <- char '#'
  c <- char 'i' <|> char 'e' <|> char 'b' <|>
       char 'o' <|> char 'd' <|> char 'x'
  case c of
    'i' -> optional radix >>= \r -> return (r, Just Inexact)
    'e' -> optional radix >>= \r -> return (r, Just Exact)
    'b' -> optional exactness >>= \e -> return (Just R2, e)
    'o' -> optional exactness >>= \e -> return (Just R8, e)
    'd' -> optional exactness >>= \e -> return (Just R10, e)
    _ -> optional exactness >>= \e -> return (Just R16, e)

exactness :: Parser Exactness
exactness = (string "#e" >> return Exact) <|>
            (string "#i" >> return Inexact)
  
radix :: Parser Radix
radix =
  (string "#b" >> return R2) <|>
  (string "#o" >> return R8) <|>
  (string "#d" >> return R10) <|>
  (string "#x" >> return R16)
  
udigit :: Radix -> Parser Integer
udigit r = do
  case r of
    R2 -> L.binary
    R8 -> L.octal
    R10 -> L.decimal
    R16 -> L.hexadecimal

sign :: Parser Sign
sign = (char '-' >> return Minus) <|> (char '+' >> return Plus)

suffix :: Parser Suffix
suffix = do
  p <- (char 'e' >> return PDefault)  <|>
       (char 's' >> return PShort)  <|>
       (char 'f' >> return PSingle) <|>
       (char 'd' >> return PDouble) <|>
       (char 'l' >> return PLong)
  s <- option Plus sign
  n <- udigit R10
  return $ Suffix p s n

------------------------- Other tokens -------------------------
quote :: Parser Char
quote = char '\''

quasiquote :: Parser Char
quasiquote = char '`'

comma :: Parser Char
comma = char ','

commaAt :: Parser T.Text
commaAt = string ",@"

dot :: Parser Char
dot = char '.'
