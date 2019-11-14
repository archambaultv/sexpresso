-- |
-- Module      :  Data.SExpresso.Parse.Generic
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module includes everything you need to write a parser for
-- S-expression ('SExpr'). It is based on the "Text.Megaparsec"
-- library and parsers can be defined for any kind of ('MonadParsec' e
-- s m) instance. This is quite generic, if you are working with
-- streams of 'Char', we suggest you also import
-- "Data.SExpresso.Parse.Char" or simply "Data.SExpresso.Parse" which
-- re-exports everything.
--
-- Since version 2.x.x.x, this module exports the basic parser
-- combinators useful for parsing S-expression that are not included
-- in the library "Text.Megaparsec".

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SExpresso.Parse.Generic
  (
    SListDefinition(..),
    getAtomParser,
    setAtomParser,
    getSepRule,
    setSepRule,
    
    SeparatorRule(..),
    sepIsMandatory,
    sepIsOptional,
    mkSeparatorRule,

    sepByRule,
    sepByRule1,
    sepEndByRule,
    sepEndByRule1,
    sepByList,
    
    parseSExpr,
    decodeOne,
    decode
   )
  where

import Control.Applicative
import Text.Megaparsec
import Data.SExpresso.SExpr

-- | The @'SExprParser' m b a@ datatype defines how to parse an
-- @'SExpr' b a@. Most parsing functions require the underlying monad
-- @m@ to be an instance of ('MonadParsec' e s m).
data SListDefinition m a = forall b c d .  
  -- | The @b@ @c@ and @d@ are there only for conviniences. There values are never used, but it is simpler to use than ().
  SListDefinition 
  (m b) -- ^ The parser for the opening tag. Returns an object of an
        -- arbitrary type @c@ that will be used to create the closing
        -- tag parser.
  (m c) -- ^ A function that takes the object returned by the
             -- opening tag parser and provide a parser for the
             -- closing tag.
  (m a) -- ^ The parser for atoms
  (m d) -- ^ A parser for space tokens which does not accept empty
         -- input (e.g. 'Text.Megaparsec.Char.space1')
  (a -> a -> SeparatorRule) -- ^ A function to tell if two consecutive
  -- atoms must be separated by space or
  -- not. See also 'mkSpacingRule' and
  -- 'setSepRule'

getAtomParser :: SListDefinition m a -> m a
getAtomParser (SListDefinition _ _ a _ _) = a

getSepRule :: SListDefinition m a -> (a -> a -> SeparatorRule)
getSepRule (SListDefinition _ _ _ _ r) = r

setAtomParser :: m b -> (b -> b -> SeparatorRule) -> SListDefinition m a -> SListDefinition m b
setAtomParser a r (SListDefinition s e _ sp _) = SListDefinition s e a sp r

setSepRule :: (a -> a -> SeparatorRule) -> SListDefinition m a -> SListDefinition m a
setSepRule r (SListDefinition s e a sp _) = SListDefinition s e a sp r
  
-- | The 'SeparatorRule' datatype indicates if a separator is optional or mandatory between two consecutive tokens.
data SeparatorRule =
  -- | Separator is mandatory
  SMandatory
  -- | Separator is optional
  | SOptional
   deriving (Show, Eq)

-- | The 'sepIsMandatory' function is a 'SeparatorRule' where the separator is always mandatory.
sepIsMandatory :: a -> a -> SeparatorRule
sepIsMandatory = \_ _ -> SMandatory

-- | The 'sepIsOptional' function is a 'SeparatorRule' where the separator is always optional.
sepIsOptional :: a -> a -> SeparatorRule
sepIsOptional = \_ _ -> SOptional

-- | The 'mkSeparatorRule' function is a helper to create a valid
-- spacing rule function for 'SExprParser' when some atoms have the
-- same 'SeparatorRule' both before and after no matter what the other
-- atom is. It takes as argument a function @f@ that takes a single
-- atom and returns the 'SeparatorRule' that applies both before and
-- after this atom.
--
-- For example, to create a spacing rule where space is optional both
-- before and after the fictitious @MyString@ token:
--
-- > s (MyString _) = SOptional
-- > s _ = Mandatory
-- > separatorRule = mkSeparatorRule s
--
-- The above is equivalent to :
--
-- > separatorRule (MyString _) _ = SOptional
-- > separatorRule _ (MyString _) = SOptional
-- > separatorRule _ _ = SMandatory
mkSeparatorRule :: (a -> SeparatorRule) -> (a -> a -> SeparatorRule)
mkSeparatorRule f = \a1 a2 -> case f a1 of
                              SOptional -> SOptional
                              SMandatory -> f a2


-- | The @'sepByRule' p sep f@ combinator parses /zero/ or more
-- occurrences of @p@, separated by @sep@ according to the
-- 'SeparatorRule' returned by @f@. A @sep@ is expected between two
-- values @p1@ and @p2@ returned by @p@ if @f p1 p2 =
-- SMandatory@. Returns a list of values returned by @p@.
sepByRule :: (MonadParsec e s m) =>
             m a ->
             m sep ->
             (a -> a -> SeparatorRule) ->
             m [a]
sepByRule p sep rule = sepByRule1 p sep rule <|> pure []

-- | The @'sepByRule1' p sep f@ combinator parses /one/ or more
-- occurrences of @p@, separated by @sep@ according to the
-- 'SeparatorRule' returned by @f@. A @sep@ is expected between two
-- values @p1@ and @p2@ returned by @p@ if @f p1 p2 =
-- SMandatory@. Returns a list of values returned by @p@.
sepByRule1 :: (MonadParsec e s m) =>
              m a ->
              m sep ->
              (a -> a -> SeparatorRule) ->
              m [a]
sepByRule1 p sep rule = p >>= go
  where --go :: a -> m [a]
        go =  (\x -> (x :) <$> sepOrP x)

        --sepOrP :: a -> m [a]
        sepOrP x1 = observing sep >>= either (\err -> sepFailed x1 err) (const (p >>= go))
  
        --sepFailed :: a -> ParseError s e -> m [a]
        sepFailed x1 err = do
          st <- getParserState
          optional p >>= maybe (pure []) (checkSepRule x1 err st)

        checkSepRule x1 err st x2 = 
          case rule x1 x2 of
            -- A separator was needed, we restore the parser state and
            -- fail using the failure of sep
            SMandatory -> setParserState st >> selectFailure err 
            SOptional -> go x2

        --selectFailure :: ParseError s e -> m a
        selectFailure (TrivialError _ unexpected2 expected) = failure unexpected2 expected
        selectFailure (FancyError _ expected) = fancyFailure expected

sepEndByRule :: (MonadParsec e s m) =>
                m a ->
                m sep ->
                (a -> a -> SeparatorRule) ->
                m [a]
sepEndByRule p sep f = sepEndByRule1 p sep f <|> pure []

sepEndByRule1 :: (MonadParsec e s m) =>
                 m a ->
                 m sep ->
                 (a -> a -> SeparatorRule) ->
                 m [a]
sepEndByRule1 p sep rule = p >>= go
  where --go :: a -> m [a]
        go =  (\x -> (x :) <$> sepOrP x)

        --sepOrP :: a -> m [a]
        sepOrP x1 = observing sep >>= either (\err -> sepFailed x1 err) (const pEnd)

        pEnd = optional p >>= maybe (pure []) go
        
        --sepFailed :: a -> ParseError s e -> m [a]
        sepFailed x1 err = do
          st <- getParserState
          optional p >>= maybe (pure []) (checkSepRule x1 err st)

        checkSepRule x1 err st x2 = 
          case rule x1 x2 of
            -- A separator was needed, we restore the parser state and
            -- fail using the failure of sep
            SMandatory -> setParserState st >> selectFailure err 
            SOptional -> go x2

        --selectFailure :: ParseError s e -> m a
        selectFailure (TrivialError _ unexpected2 expected) = failure unexpected2 expected
        selectFailure (FancyError _ expected) = fancyFailure expected

-- | The 'parseSExprList' function return a parser for parsing S-expression of the form @'SList' _ _@.
sepByList :: (MonadParsec e s m) =>
              SListDefinition m a -> 
              m [a]
sepByList (SListDefinition pSTag pETag atom sep sepRule)  =
 pSTag *> optional sep *> sepEndByRule atom sep sepRule <* pETag

-- | The 'parseSExpr' function return a parser for parsing
-- S-expression ('SExpr'), that is either an atom (@'SAtom' _@) or a
-- list @'SList' _ _@. See also 'decodeOne' and 'decode'.
parseSExpr :: (MonadParsec e s m) =>
              SListDefinition m a ->
              m (SExpr a)
parseSExpr (SListDefinition s e a sp r) =
    let sepRule' (SList _) _ = SOptional
        sepRule' _ (SList _) = SOptional
        sepRule' (SAtom x1) (SAtom x2) = r x1 x2

        def' = SListDefinition s e p sp sepRule'

        p = (SAtom <$> a) <|> (SList <$> sepByList def')
    in p  

-- | The 'decodeOne' function return a parser for parsing a file
-- containing only one S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decode'.
decodeOne :: (MonadParsec e s m) => SListDefinition m a -> m (SExpr a)
decodeOne def@(SListDefinition _ _ _ ws _) =
  optional ws *> parseSExpr def <* (optional ws >> eof)

-- | The 'decode' function return a parser for parsing a file
-- containing many S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decodeOne'.
decode :: (MonadParsec e s m) => SListDefinition m a -> m [SExpr a]
decode def@(SListDefinition _ _ _ ws spaceRule') =
  let sepRule' (SList _) _ = SOptional
      sepRule' _ (SList _) = SOptional
      sepRule' (SAtom x1) (SAtom x2) = spaceRule' x1 x2
  in optional ws *> sepEndByRule (parseSExpr def) ws sepRule' <* eof
