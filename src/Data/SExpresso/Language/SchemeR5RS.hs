-- |
-- Module      :  Data.SExpresso.SExpr
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- Module for parsing the Scheme R5RS language.
--
-- Scheme R5RS s-expressions are parsed as @'SExpr' 'SExprType'
-- 'SchemeToken'@.  Such s-expressions can be converted into a Scheme
-- R5RS datum (see 'Datum') by the function 'sexpr2Datum'.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Parsing library for some parts of the Scheme R5RS language
-- as defined in section 7 of the report
-- The library does parse tab and \r\n and whitespace
module Data.SExpresso.Language.SchemeR5RS (
  -- * SchemeToken and Datum related data types and functions 
  SExprType(..),
  SchemeToken(..),
  tokenParser,
  sexpr,

  Datum(..),
  sexpr2Datum,

  -- * Scheme R5RS whitespace parsers
  whitespace,
  comment,
  interTokenSpace,
  interTokenSpace1,

  -- * Individual parser for each of the constructors of SchemeToken
  identifier,
  boolean,
  character,
  stringParser,
  quote,
  quasiquote,
  comma,
  commaAt,
  dot,

  -- ** Scheme Number
  --
  -- | Scheme R5RS numbers are quite exotic. They can have exactness
  -- prefix, radix prefix and the pound sign (#) can replace a
  -- digit. On top of that, you can define integer, rational, decimal
  -- and complex numbers of arbitrary precision. Decimal numbers can
  -- also have a suffix indicating the machine precision.
  --
  -- Since Haskell does not have native types to express this
  -- complexity, this module defines the 'SchemeNumber' data type to
  -- encode the parsed number. User of this module can then convert a
  -- 'SchemeNumber' object to a more appropriate data type according
  -- to their needs.
  SchemeNumber(..),
  Exactness(..),
  Complex(..),
  SReal(..),
  Sign(..),
  UInteger(..),
  Pounds,
  Precision(..),
  Suffix(..),
  number,


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

-- | The 'SchemeToken' data type defines the atoms of an Scheme R5RS
-- s-expression. An @'SExpr' 'SExprType' 'SchemeToken'@ object
-- containning the atoms 'TQuote', 'TQuasiquote', 'TComma', 'TCommaAt'
-- and 'TDot' need futher processing in order to get what the R5RS
-- report calls a datum. See also 'Datum'.
data SchemeToken =
  -- | A boolean.
  TBoolean Bool
  -- | A number. See 'SchemeNumber'.
  | TNumber SchemeNumber
  -- | A unicode character.
  | TChar Char
  -- | A string.
  | TString T.Text
  -- | A valid R5RS identifier.
  | TIdentifier T.Text
  -- | The quote (') symbol.
  | TQuote
  -- | The quasiquote (`) symbol.
  | TQuasiquote
  -- | The comma (,) symbol.
  | TComma
  -- | The comma at (,\@) symbol.
  | TCommaAt
  -- | The dot (.) symbol.
  | TDot
  deriving (Eq, Show)

-- | The 'tokenParser' parses a 'SchemeToken'
tokenParser :: (MonadParsec e s m, Token s ~ Char) => m SchemeToken
tokenParser = (boolean >>= return . TBoolean) <|>
              -- character must come before number
              (character >>= return . TChar) <|>
              (stringParser >>= return . TString) <|>
              (identifier >>= return . TIdentifier) <|>
              (quote >> return TQuote) <|>
              (quasiquote >> return TQuasiquote) <|>
              -- commaAt must come before comma
              (commaAt >> return TCommaAt) <|> 
              (comma >> return TComma) <|>
              -- We must try number because it can conflict with the dot ex : .2 and (a . b)
              (try number >>= return . TNumber) <|>
              (dot >> return TDot)
              


spacingRule :: SchemeToken -> SpacingRule
spacingRule (TString _) = SOptional
spacingRule TQuote = SOptional
spacingRule TQuasiquote  = SOptional
spacingRule TComma = SOptional
spacingRule TCommaAt = SOptional
spacingRule _ = SMandatory

-- | Scheme R5RS defines two types of s-expressions. Standard list
-- beginning with '(' and vector beginning with '#('. The 'SExprType'
-- data type indicates which one was parsed.
data SExprType =
  -- | A standard list
  STList
  -- | A vector
  | STVector
  deriving (Eq, Show)

-- | The 'sexpr' defines a 'SExprParser' to parse a Scheme R5RS
-- s-expression as an @'SExpr' 'SExprType' 'SchemeToken'@. If you also
-- want source position see the 'withLocation' function.
--
-- Space is optional before and after the following tokens:
--
-- * 'TString'
-- * 'TQuote'
-- * 'TQuasiquote'
-- * 'TComma'
-- * 'TCommaAt'
sexpr :: forall e s m . (MonadParsec e s m, Token s ~ Char) => SExprParser m SExprType SchemeToken
sexpr =
  let sTag = (single '(' >> return STList) <|> (chunk (tokensToChunk (Proxy :: Proxy s) "#(") >> return STVector)
      eTag = \t -> single ')' >> return t
  in SExprParser sTag eTag tokenParser interTokenSpace1 (mkSpacingRule spacingRule)

-- | The 'Datum' data type implements the Scheme R5RS definition of a Datum. See also 'sexpr2Datum'.
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

-- | The 'sexpr2Datum' function takes a list of 'SchemeToken' and
-- returns a list of 'Datum'. In case of failure it will report an
-- error, hence the 'Either' data type in the signature.
--
-- As defined in the Scheme R5RS report, the 'TQuote', 'TQuasiquote',
-- 'TComma', 'TCommaAt' and 'TDot' tokens must be followed by another
-- token.
sexpr2Datum :: [SExpr SExprType SchemeToken] -> Either String [Datum]
sexpr2Datum [] = Right []
sexpr2Datum ((SAtom (TBoolean x)) : xs) = (:) <$> pure (DBoolean x) <*> sexpr2Datum xs
sexpr2Datum ((SAtom (TNumber x)) : xs) = (:) <$> pure (DNumber x) <*> sexpr2Datum xs
sexpr2Datum ((SAtom (TChar x)) : xs) = (:) <$> pure (DChar x) <*> sexpr2Datum xs
sexpr2Datum ((SAtom (TString x)) : xs) = (:) <$> pure (DString x) <*> sexpr2Datum xs
sexpr2Datum ((SAtom (TIdentifier x)) : xs) = (:) <$> pure (DIdentifier x) <*> sexpr2Datum xs
sexpr2Datum ((SAtom TQuote) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the quote."
  else return $ DQuote (head xs') : tail xs'
sexpr2Datum ((SAtom TQuasiquote) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the quasiquote."
  else return $ DQuasiquote (head xs') : tail xs'
sexpr2Datum ((SAtom TComma) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the comma."
  else return $ DComma (head xs') : tail xs'
sexpr2Datum ((SAtom TCommaAt) : xs) = do
  xs' <- sexpr2Datum xs
  if null xs'
  then Left "Expecting a datum after the quote."
  else return $ DCommaAt (head xs') : tail xs'
sexpr2Datum ((SAtom TDot) : _) = Left "Unexpected dot"
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
-- | The 'whitespace' parser  parses one space, tab or end of line (\\n and \\r\\n).
whitespace :: (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = (char ' ' >> return ()) <|>
             (char '\t' >> return ()) <|>
             (eol >> return ())

-- | The 'comment' parser parses a semi-colon (;) character and
-- everything until the end of line included.
comment :: (MonadParsec e s m, Token s ~ Char) => m ()
comment = char ';' >>
          takeWhileP Nothing (\c -> c /= '\n' && c /= '\r') >>
          ((eol >> return ()) <|> eof)

atmosphere :: (MonadParsec e s m, Token s ~ Char) => m ()
atmosphere = whitespace <|> comment

-- | The 'interTokenSpace' parser parses zero or more whitespace or comment.
interTokenSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
interTokenSpace = many atmosphere >> return ()

-- | The 'interTokenSpace1' parser parses one or more whitespace or comment.
interTokenSpace1 :: (MonadParsec e s m, Token s ~ Char) => m ()
interTokenSpace1 = some atmosphere >> return ()

------------------------- Identifier -------------------------

-- | The 'identifier' parser parses a Scheme R5RS identifier.
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
-- | The 'boolean' parser parses a Scheme R5RS boolean (\#t or \#f).
boolean :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m Bool
boolean = (chunk (tokensToChunk (Proxy :: Proxy s) "#t") >> return True) <|>
          (chunk (tokensToChunk (Proxy :: Proxy s) "#f") >> return False)


------------------------- Character -------------------------
-- | The 'character' parser parses a Scheme R5RS character.
character :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m Char
character = do
  _ <- chunk (tokensToChunk (Proxy :: Proxy s) "#\\")
  (chunk (tokensToChunk (Proxy :: Proxy s) "newline") >> return '\n') <|>
   (chunk (tokensToChunk (Proxy :: Proxy s) "space") >> return ' ') <|>
   anySingle

------------------------- String -------------------------
-- | The 'stringParser' parser parses a Scheme R5RS character.
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

-- | A Scheme R5RS number is either exact or inexact. The paragraph
-- 6.4.2 from the R5RS report should clarify the meaning of exact and
-- inexact :
--
-- \"\"\"A numerical constant may be specified to be either
-- exact or inexact by a prefix.  The prefixes are \#e for exact, and \#i
-- for inexact.  An exactness prefix may appear before or after any
-- radix prefix that is used.  If the written representation of a
-- number has no exactness prefix, the constant may be either inexact
-- or exact.  It is inexact if it contains a decimal point, an
-- exponent, or a \“#\” character in the place of a digit, otherwise it
-- is exact.\"\"\"
data Exactness = Exact | Inexact
               deriving (Eq, Show)

-- | The 'Sign' datatype indicates if a number is positive ('Plus') or negative ('Minus')
data Sign = Plus | Minus
          deriving (Eq, Show)

-- | A Scheme R5RS number can have many # signs at the end. This type alias
-- indicates the number of # signs parsed.
type Pounds = Integer

-- | A Scheme R5RS unsigned integer can be written in three ways.
--
-- * With digits only
-- * With digits and # signs
-- * With only # signs in some special context.
data UInteger =
  -- | Integer made only of digits
  UInteger Integer
  -- | Integer made of digits and #. The first argument is the number
  -- that was parsed and the second the number of # signs. For
  -- example, 123## is represented as @UIntPounds 123 2@. Do not take
  -- the first argument as a good approximation of the number. It
  -- needs to be shifted by the number of pounds.
  | UIntPounds Integer Pounds
  -- | Integer made only of #. It can only appear as the third argument in numbers of the form @'SDecimal' _ _ _ _@.
  | UPounds Pounds
  deriving (Eq, Show)

hasPounds :: UInteger -> Bool
hasPounds (UInteger _) = False
hasPounds _ = True

isInexactI :: UInteger -> Bool
isInexactI = hasPounds

-- | Scheme R5RS defines 5 types of machine precision for a decimal
-- number. The machine precision is specified in the suffix (see
-- 'Suffix').
data Precision =
  -- | Suffix starting with e.
  PDefault |
  -- | Suffix starting with s.
  PShort |
  -- | Suffix starting with f.
  PSingle |
  -- | Suffix starting with d.
  PDouble |
  -- | Suffix starting with l.
  PLong
  deriving (Eq, Show)

-- | The 'Suffix' data type represents the suffix for a Scheme R5RS
-- decimal number. It is a based 10 exponent.
data Suffix = Suffix Precision Sign Integer
            deriving (Eq, Show)

-- | The 'SReal' data type represents a Scheme R5RS real number.
data SReal =
  -- | A signed integer.
  SInteger Sign UInteger
  -- | A signed rational. The first number is the numerator and the
  -- second one the denominator.
  | SRational Sign UInteger UInteger
  -- | A signed decimal number. The first number appears before the
  -- dot, the second one after the dot.
  | SDecimal Sign UInteger UInteger (Maybe Suffix)
  deriving (Eq, Show)

isInexactR :: SReal -> Bool
isInexactR (SInteger _ i) = isInexactI i
isInexactR (SRational _ i1 i2) = isInexactI i1 || isInexactI i2
isInexactR (SDecimal _ _ _ _) = True

-- | The 'Complex' data type represents a Scheme R5RS complex number.
data Complex =
  -- | A real number.
  CReal SReal
  -- | A complex number in angular notation.
  | CAngle SReal SReal
  -- | A complex number in absolute notation.
  | CAbsolute SReal SReal
  deriving (Eq, Show)

isInexact :: Complex -> Bool
isInexact (CReal s) = isInexactR s
isInexact (CAngle s1 s2) = isInexactR s1 || isInexactR s2
isInexact (CAbsolute s1 s2) = isInexactR s1 || isInexactR s2

-- | A Scheme R5RS number is an exact or inexact complex number.
data SchemeNumber = SchemeNumber Exactness Complex
                  deriving (Eq, Show)

-- | The 'number' parser parses a Scheme R5RS number.
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
ureal r s = dotN <|> ureal'

  where dotN =  do
          _ <- char '.'
          if r /= R10
          then fail "Numbers containing decimal point must be in decimal radix"
          else do
             n <- uinteger R10
             sf <- optional suffix
             return $ SDecimal s (UInteger 0) n sf

        ureal' = do
          -- First parse an integer
          u1 <- uinteger r
          -- Check if the integer is followed by these characters
          mc <- optional (char '/' <|> char '.')
          case mc of
            -- Integer with or without suffix
            Nothing -> plainInteger u1
            -- Rational
            Just '/' -> rational u1
            -- Decimal
            Just _ -> decimal u1

        plainInteger u1 = do
            sf <- optional suffix
            case sf of
              Just _ -> return $ SDecimal s u1 (UInteger 0) sf
              Nothing -> return $ SInteger s u1
        
        rational u1 = do
          u2 <- uinteger r
          return $ SRational s u1 u2

        decimal u1 = do
          if r /= R10
          then fail "Numbers containing decimal point must be in decimal radix"
          else do
             -- If u1 has # character, only other # are
             -- allowed. Otherwise a number may be present
             n <- if hasPounds u1 then return Nothing else optional (udigit R10) :: m (Maybe Integer)
             pounds <- takeWhileP Nothing (== '#')
             sf <- optional suffix
             let nbPounds = toInteger $ chunkLength (Proxy :: Proxy s) pounds
             let u2 = case (hasPounds u1, nbPounds, n) of
                         (True, p, _) -> UPounds p
                         (False, 0, Nothing) -> UInteger 0
                         (False, 0, (Just x)) -> UInteger x
                         (False, p, Nothing) -> UPounds p
                         (False, p, (Just x)) -> UIntPounds x p
             return $ SDecimal s u1 u2 sf

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
-- | The 'quote' parser parses a quote character (').
quote :: (MonadParsec e s m, Token s ~ Char) => m Char
quote = char '\''

-- | The 'quasiquote' parser parses a quasiquote character (`).
quasiquote :: (MonadParsec e s m, Token s ~ Char) => m Char
quasiquote = char '`'

-- | The 'comma' parser parses a comma (,).
comma :: (MonadParsec e s m, Token s ~ Char) => m Char
comma = char ','

-- | The 'commaAt' parser parses a comma followed by \@ (,\@).
commaAt :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m T.Text
commaAt = chunk (tokensToChunk (Proxy :: Proxy s) ",@") >> return ",@"

-- | The 'dot' parser parses a single dot character (.).
dot :: (MonadParsec e s m, Token s ~ Char) => m Char
dot = char '.'
