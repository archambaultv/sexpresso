{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Parsing library for some parts of the Scheme R5RS language
-- as defined in section 7 of the report
-- The library does parse tab and \r\n and whitespace
module Data.SExpresso.Language.SchemeR5RS (
  SchemeToken(..),
  tokenParser,

  SExprType(..),
  sexpr,
  
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

  Datum(..),
  sexpr2Datum,

  ) where

import Data.Maybe
import Data.Proxy
import Data.List
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as ML
import Data.SExpresso.SExpr
import Data.SExpresso.Parse

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
                 deriving (Eq, Show)

tokenParser :: (MonadParsec e s m, Token s ~ Char) => m SchemeToken
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


spacingRule :: SchemeToken -> SpacingRule
spacingRule (TString _) = SOptional
spacingRule TQuote = SOptional
spacingRule TQuasiquote  = SOptional
spacingRule TComma = SOptional
spacingRule TCommaAt = SOptional
spacingRule _ = SMandatory

data SExprType = STList | STVector
               deriving (Eq, Show)

sexpr :: forall e s m . (MonadParsec e s m, Token s ~ Char) => SExprParser m SExprType SExprType SchemeToken
sexpr =
  let sTag = (single '(' >> return STList) <|> (chunk (tokensToChunk (Proxy :: Proxy s) "#(") >> return STVector)
      eTag = \t -> single ')' >> return t
  in SExprParser sTag eTag tokenParser interTokenSpace1 (mkSpacingRule spacingRule)
          
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
           deriving (Eq, Show)

sexpr2Datum :: [SExpr SExprType SchemeToken] -> Either String [Datum]
sexpr2Datum [] = Right []
sexpr2Datum ((A (TBoolean x)) : xs) = (:) <$> pure (DBoolean x) <*> sexpr2Datum xs
sexpr2Datum ((A (TNumber x)) : xs) = (:) <$> pure (DNumber x) <*> sexpr2Datum xs
sexpr2Datum ((A (TChar x)) : xs) = (:) <$> pure (DChar x) <*> sexpr2Datum xs
sexpr2Datum ((A (TString x)) : xs) = (:) <$> pure (DString x) <*> sexpr2Datum xs
sexpr2Datum ((A (TIdentifier x)) : xs) = (:) <$> pure (DIdentifier x) <*> sexpr2Datum xs
sexpr2Datum ((A TQuote) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the quote."
  else return $ DQuote (head xs') : tail xs'
sexpr2Datum ((A TQuasiquote) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the quasiquote."
  else return $ DQuasiquote (head xs') : tail xs'
sexpr2Datum ((A TComma) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the comma."
  else return $ DQuote (head xs') : tail xs'
sexpr2Datum ((A TCommaAt) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the quote."
  else return $ DCommaAt (head xs') : tail xs'
sexpr2Datum ((A TDot) : _) = Left "Unexpected dot"
sexpr2Datum ((SList STVector vs) : xs) = (:) <$> (sexpr2Datum vs >>= return . DVector) <*> sexpr2Datum xs
sexpr2Datum ((SList STList ls) : xs) = (:) <$> (listToken2Datum ls) <*> sexpr2Datum xs
  where listToken2Datum ys =
          let l = length ys
          in if l < 3
             then sexpr2Datum ys >>= return . DList
             else let penultimate = head $ drop (l - 2) ys
                  in case penultimate of
                       (A TDot) ->
                         let last' = head $ drop (l - 1) ys
                             tokens' = take (l - 2) ys
                         in do
                           lastD <- sexpr2Datum [last']
                           tokensD <- sexpr2Datum tokens'
                           return $ DDotList tokensD (head lastD) 
                       _ -> sexpr2Datum ys >>= return . DList


------------------------- Whitespace and comments -------------------------
whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = (char ' ' >> return ()) <|>
             (char '\t' >> return ()) <|>
             (eol >> return ())

comment :: (MonadParsec e s m, Token s ~ Char) => m ()
comment = char ';' >>
          takeWhileP Nothing (\c -> c /= '\n' && c /= '\r') >>
          ((eol >> return ()) <|> eof)

atmosphere :: (MonadParsec e s m, Token s ~ Char) => m ()
atmosphere = whitespace <|> comment

interTokenSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
interTokenSpace = many atmosphere >> return ()

interTokenSpace1 :: (MonadParsec e s m, Token s ~ Char) => m ()
interTokenSpace1 = some atmosphere >> return ()

------------------------- Identifier -------------------------

-- FixMe : Must not parse +i, -i, etc ... (they are numbers)
identifier :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m T.Text
identifier = standardIdentifier <|> peculiarIdentifier
  where standardIdentifier = do
          i <- oneOf initialList
          is <- takeWhileP Nothing (\c -> c `elem` subsequentList)
          return $ T.pack $ (i : chunkToTokens (Proxy :: Proxy s) is)

initialList :: String
initialList = ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*/:<=>?^_~"

subsequentList :: String
subsequentList = initialList ++ ['0'..'9'] ++ "+-.@"

peculiarIdentifier :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m T.Text
peculiarIdentifier = (single '+' >> return "+") <|>
                     (single '-' >> return "-") <|>
                     (chunk (tokensToChunk (Proxy :: Proxy s) "...") >> return "...")

------------------------- Booleans -------------------------
boolean :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m Bool
boolean = (chunk (tokensToChunk (Proxy :: Proxy s) "#t") >> return True) <|>
          (chunk (tokensToChunk (Proxy :: Proxy s) "#f") >> return False)


------------------------- Character -------------------------
character :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m Char
character = do
  _ <- chunk (tokensToChunk (Proxy :: Proxy s) "#\\")
  (chunk (tokensToChunk (Proxy :: Proxy s) "newline") >> return '\n') <|>
   (chunk (tokensToChunk (Proxy :: Proxy s) "space") >> return ' ') <|>
   anySingle

------------------------- String -------------------------
stringParser :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m T.Text
stringParser = do
  _ <- char '"'
  xs <- consume
  return $ L.toStrict $ B.toLazyText xs

  where consume :: (MonadParsec e s m, Token s ~ Char) => m B.Builder
        consume = do
          x <- takeWhileP Nothing (\c -> c /= '\\' && c /= '"')
          c <- char '\\' <|> char '"'
          let xB = B.fromString $ chunkToTokens (Proxy :: Proxy s) x
          case c of
            '"' -> return $ xB
            _ -> do
               c1 <- char '\\' <|> char '"'
               x2 <- consume
               return $ xB <> B.fromString [c1] <> x2


------------------------- Numbers -------------------------
data Radix = R2 | R8 | R10 | R16
           deriving (Eq, Show)

data Exactness = Exact | Inexact
               deriving (Eq, Show)

data Sign = Plus | Minus
          deriving (Eq, Show)

-- The second integer indicates the number of # sign in the number
type Pounds = Integer

data UInteger = UInteger Integer Pounds
              deriving (Eq, Show)

hasPounds :: UInteger -> Bool
hasPounds (UInteger _ 0) = False
hasPounds _ = True

data Precision = PDefault | PShort | PSingle | PDouble | PLong
               deriving (Eq, Show)

data Suffix = Suffix Precision Sign Integer
            deriving (Eq, Show)

data UReal = UDecimal UInteger (Either UInteger Pounds) (Maybe Suffix)
           | URational UInteger UInteger
           | UInt UInteger
           deriving (Eq, Show)

data SReal = SReal Sign UReal
           deriving (Eq, Show)

data Complex = ComplexReal SReal
             | ComplexAngle SReal SReal
             | ComplexAbsolute SReal SReal
             deriving (Eq, Show)

data SchemeNumber = SchemeNumber (Maybe Radix) (Maybe Exactness) Complex
                  deriving (Eq, Show)

number :: (MonadParsec e s m, Token s ~ Char) => m SchemeNumber
number = do
  (r, e) <- prefix
  c <- complex (fromMaybe R10 r)
  return $ SchemeNumber r e c
 
complex :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> m Complex
complex r = (chunk (tokensToChunk (Proxy :: Proxy s) "+i") >>
             (return $ ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) (SReal Plus (UInt $ UInteger 1 0)))) <|>
            (chunk (tokensToChunk (Proxy :: Proxy s) "-i") >>
             (return $ ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) (SReal Minus (UInt $ UInteger 1 0)))) <|>
            withRealPart <|> noRealPart (SReal Plus (UInt $ UInteger 0 0))
  where withRealPart = do
          n1 <- real r
          c <- optional (char '@' <|> char '+' <|> char '-' <|> char 'i')
          case c of
            Nothing -> return $ ComplexReal n1
            Just '@' -> do
              n2 <- real r
              return $ ComplexAngle n1 n2
            Just 'i' -> return $ ComplexAbsolute (SReal Plus (UInt $ UInteger 0 0)) n1
            Just '+' -> noRealPart' Plus n1
            Just _ -> noRealPart' Minus n1
          
        noRealPart n1 = do
          s <- (char '-' >> return Minus) <|> (char '+' >> return Plus)
          noRealPart' s n1

        noRealPart' s n1 = do
          u <- optional (ureal r)
          _ <- char 'i'
          case u of
            Nothing -> return $ ComplexAbsolute n1 (SReal s (UInt $ UInteger 1 0))
            Just n2 -> return $ ComplexAbsolute n1 (SReal s n2)

real :: (MonadParsec e s m, Token s ~ Char) => Radix -> m SReal
real r = do
  s <- option Plus sign
  u <- ureal r
  return $ SReal s u

ureal :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> m UReal
ureal r =
  case r of
    R10 -> dotN <|> (do
      u1 <- uinteger r
      mc <- optional (char '/' <|> char '.')
      case mc of
        -- Integer, with or without suffix
        Nothing -> do
          s <- optional suffix
          case s of
            Just _ -> return $ UDecimal u1 (Left $ UInteger 0 0) s
            Nothing -> return $ UInt u1

        -- Rational
        Just '/' -> rational u1

        -- Decimal
        Just _ ->
          if hasPounds u1
          then do
            pounds <- takeWhileP Nothing (== '#')
            s <- optional suffix
            return $ UDecimal u1 (Right $ toInteger $ chunkLength (Proxy :: Proxy s) pounds) s
          else do
            n <- option 0 (udigit R10)
            pounds <- takeWhileP Nothing (== '#')
            s <- optional suffix
            return $ UDecimal u1 (Left $ UInteger n (toInteger $ chunkLength (Proxy :: Proxy s) pounds)) s 
                    )

    _ -> intOrRationalOnly

  where        
        intOrRationalOnly = do
          u1 <- uinteger r
          mc <- optional (char '/')
          case mc of
            Nothing -> return $ UInt u1
            Just _ -> rational u1
        
        rational u1 = do
          u2 <- uinteger r
          return $ URational u1 u2

        dotN = do
          _ <- char '.'
          n <- udigit R10
          pounds <- takeWhileP Nothing (== '#')
          s <- optional suffix
          return $ UDecimal (UInteger 0 0) (Left $ UInteger n (toInteger $ chunkLength (Proxy :: Proxy s) pounds)) s

uinteger :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> m UInteger
uinteger r = do
  n <- udigit r
  pounds <- takeWhileP Nothing (== '#')
  return $ UInteger n (toInteger $ chunkLength (Proxy :: Proxy s) pounds)
  

prefix :: (MonadParsec e s m, Token s ~ Char) => m (Maybe Radix, Maybe Exactness)
prefix = do
  x <- optional $ char '#'
  case x of
    Nothing -> return (Nothing, Nothing)
    _ -> do
      c <- char 'i' <|> char 'e' <|> char 'b' <|>
           char 'o' <|> char 'd' <|> char 'x'
      case c of
        'i' -> optional radix >>= \r -> return (r, Just Inexact)
        'e' -> optional radix >>= \r -> return (r, Just Exact)
        'b' -> optional exactness >>= \e -> return (Just R2, e)
        'o' -> optional exactness >>= \e -> return (Just R8, e)
        'd' -> optional exactness >>= \e -> return (Just R10, e)
        _ -> optional exactness >>= \e -> return (Just R16, e)

exactness :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m Exactness
exactness = (chunk (tokensToChunk (Proxy :: Proxy s) "#e") >> return Exact) <|>
            (chunk (tokensToChunk (Proxy :: Proxy s) "#i") >> return Inexact)
  
radix :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m  Radix
radix =
  (chunk (tokensToChunk (Proxy :: Proxy s) "#b") >> return R2) <|>
  (chunk (tokensToChunk (Proxy :: Proxy s) "#o") >> return R8) <|>
  (chunk (tokensToChunk (Proxy :: Proxy s) "#d") >> return R10) <|>
  (chunk (tokensToChunk (Proxy :: Proxy s) "#x") >> return R16)
  
udigit :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> m Integer
udigit r = do
  case r of
    R2 -> ML.binary
    R8 -> ML.octal
    R10 -> ML.decimal
    R16 -> hexadecimal
  where hexadecimal = mkNum
                      <$> takeWhile1P Nothing (\c -> c `elem` ("0123456789abcdef" :: String))
                      <?> "hexadecimal integer"
                      
        mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
        step a c = a * 16 + fromIntegral (C.digitToInt c)

sign :: (MonadParsec e s m, Token s ~ Char) => m  Sign
sign = (char '-' >> return Minus) <|> (char '+' >> return Plus)

suffix :: (MonadParsec e s m, Token s ~ Char) => m Suffix
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
quote :: (MonadParsec e s m, Token s ~ Char) => m Char
quote = char '\''

quasiquote :: (MonadParsec e s m, Token s ~ Char) => m Char
quasiquote = char '`'

comma :: (MonadParsec e s m, Token s ~ Char) => m Char
comma = char ','

commaAt :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m String
commaAt = chunk (tokensToChunk (Proxy :: Proxy s) ",@") >> return ",@"

dot :: (MonadParsec e s m, Token s ~ Char) => m Char
dot = char '.'
