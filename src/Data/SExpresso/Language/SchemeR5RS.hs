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

  Exactness(..),
  Sign(..),
  Pounds,
  UInteger(..),
  Precision(..),
  Suffix(..),
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

-- Indicates the number of # sign in the number
type Pounds = Integer

data UInteger = UInteger Integer -- Only digit
              | UIntPounds Integer Pounds -- Digit with some #
              | UPounds Pounds -- Only #
              deriving (Eq, Show)

hasPounds :: UInteger -> Bool
hasPounds (UInteger _) = False
hasPounds _ = True

isInexactI :: UInteger -> Bool
isInexactI = hasPounds

data Precision = PDefault | PShort | PSingle | PDouble | PLong
               deriving (Eq, Show)

data Suffix = Suffix Precision Sign Integer
            deriving (Eq, Show)

data SReal = SInteger Sign UInteger
           | SRational Sign UInteger UInteger
           | SDecimal Sign UInteger UInteger (Maybe Suffix)
           deriving (Eq, Show)

isInexactR :: SReal -> Bool
isInexactR (SInteger _ i) = isInexactI i
isInexactR (SRational _ i1 i2) = isInexactI i1 || isInexactI i2
isInexactR (SDecimal _ _ _ _) = True

data Complex = CReal SReal
             | CAngle SReal SReal
             | CAbsolute SReal SReal
             deriving (Eq, Show)

isInexact :: Complex -> Bool
isInexact (CReal s) = isInexactR s
isInexact (CAngle s1 s2) = isInexactR s1 || isInexactR s2
isInexact (CAbsolute s1 s2) = isInexactR s1 || isInexactR s2

-- From R5RS 6.4.2 A numerical constant may be specified to be either
-- exact orinexact by a prefix.  The prefixes are#efor exact, and#ifor
-- inexact.  An exactness prefix may appear before or afterany radix
-- prefix that is used.  If the written representation of a number has
-- no exactness prefix, the constant may be either inexact or exact.
-- It is inexact if it contains a decimal point, an exponent, or a “#”
-- character in the place of a digit, otherwise it is exact.

data SchemeNumber = SchemeNumber Exactness Complex
                  deriving (Eq, Show)

number :: (MonadParsec e s m, Token s ~ Char) => m SchemeNumber
number = do
  (r, e) <- prefix
  c <- complex (fromMaybe R10 r)
  let e' = fromMaybe (if isInexact c then Inexact else Exact) e
  return $ SchemeNumber e'  c
 
complex :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> m Complex
complex r = do
  ms <- optional sign
  case ms of
    Nothing -> complex' Plus
    Just s -> i s <|> complex' s

  where
    -- Parser for +i and -i
    i s = char 'i' >> (return $ CAbsolute (SInteger Plus (UInteger 0)) (SInteger s (UInteger 1)))

    -- Parser for complex except +i and -i
    complex' sr = do
      -- First parse a number
      n1 <- ureal r sr
      -- Check if the number is followed by any of these characters
      c <- optional (char '@' <|> char '+' <|> char '-' <|> char 'i')
      case c of
          -- Plain real number
          Nothing -> return $ CReal n1
          -- Complex angular number
          Just '@' -> do
            n2 <- real r
            return $ CAngle n1 n2
          -- Pure imaginary number
          Just 'i' -> return $ CAbsolute (SInteger Plus (UInteger 0)) n1
          -- Real +/- Imaginary number
          Just '+' -> imaginaryPart n1 Plus
          Just _ -> imaginaryPart n1 Minus
  
    imaginaryPart realN si = do
      u <- optional (ureal r si)
      _ <- char 'i'
      case u of
        Nothing -> return $ CAbsolute realN (SInteger si (UInteger 1))
        Just n2 -> return $ CAbsolute realN n2

real :: (MonadParsec e s m, Token s ~ Char) => Radix -> m SReal
real r = do
  s <- option Plus sign
  ureal r s

ureal :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> Sign -> m SReal
ureal r s =
  case r of
    R10 -> dotN <|> (do
      u1 <- uinteger r
      mc <- optional (char '/' <|> char '.')
      case mc of
        -- Integer, with or without suffix
        Nothing -> do
          sf <- optional suffix
          case sf of
            Just _ -> return $ SDecimal s u1 (UInteger 0) sf
            Nothing -> return $ SInteger s u1

        -- Rational
        Just '/' -> rational u1

        -- Decimal
        Just _ ->
          if hasPounds u1
          then do
            -- The number before the dot has #, so only # are allowed
            pounds <- takeWhileP Nothing (== '#')
            sf <- optional suffix
            return $ SDecimal s u1 (UPounds $ toInteger $ chunkLength (Proxy :: Proxy s) pounds) sf
          else do
            -- The number before the dot does not have #, so both digit, # are allowed
            n <- optional (udigit R10)
            pounds <- takeWhileP Nothing (== '#')
            sf <- optional suffix
            let nbPounds = toInteger $ chunkLength (Proxy :: Proxy s) pounds
            if nbPounds <= 0
            then return $ SDecimal s u1 (UInteger (fromMaybe 0 n)) sf
            else case n of
                   Nothing -> return $ SDecimal s u1 (UPounds nbPounds) sf
                   Just u2 -> return $ SDecimal s u1 (UIntPounds u2 nbPounds) sf
                    )

    _ -> intOrRationalOnly

  where        
        intOrRationalOnly = do
          u1 <- uinteger r
          mc <- optional (char '/')
          case mc of
            Nothing -> return $ SInteger s u1
            Just _ -> rational u1
        
        rational u1 = do
          u2 <- uinteger r
          return $ SRational s u1 u2

        dotN = do
          _ <- char '.'
          n <- uinteger R10
          sf <- optional suffix
          return $ SDecimal s (UInteger 0) n sf

uinteger :: forall e s m . (MonadParsec e s m, Token s ~ Char) => Radix -> m UInteger
uinteger r = do
  n <- udigit r
  pounds <- takeWhileP Nothing (== '#')
  let nbPounds = toInteger $ chunkLength (Proxy :: Proxy s) pounds
  if nbPounds <= 0
  then return $ UInteger n
  else return $ UIntPounds n nbPounds
  

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
  
udigit :: forall e s m a . (MonadParsec e s m, Token s ~ Char, Integral a) => Radix -> m a
udigit r = do
  case r of
    R2 -> ML.binary
    R8 -> ML.octal
    R10 -> ML.decimal
    R16 -> hexadecimal -- ML.hexadecimal also parses uppercase "ABCDEF"
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
