-- |
-- Module      :  Data.SExpresso.Parse.Combinators
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

module Data.SExpresso.Parse.Combinators
  (
    SeparatorRule(..),
    sepIsMandatory,
    sepIsOptional,
    mkSeparatorRule,

    sepByRule,
    sepByRule1,
    sepEndByRule,
    sepEndByRule1,

    SourceOffset,
    sexprAnn,
    sexpr
   )
  where

import Control.Applicative
import Text.Megaparsec as M
import Data.SExpresso.SExpr

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

type SourceOffset = (Int, Int) -- Offset and length

sexprAnn :: (MonadParsec e s m)  =>
         m b ->
         (b -> m c) ->
         m a ->
         m sp ->
         (a -> a -> SeparatorRule) ->
         m (SExprAnn SourceOffset a)
sexprAnn open close atom sep rule = atomP <|> listP

  where atomP = do
          offset1 <- getOffset
          a <- atom
          offset2 <- getOffset
          return $ CAtom (offset1, offset2 - offset1) a

        listP = do
          offset1 <- getOffset
          o <- open <* optional sep
          xs <- sepEndByRule (atomP <|> listP) sep rule2
          _ <- close o
          offset2 <- getOffset
          return $ CList (offset1, offset2 - offset1) xs

        rule2 (CAtom _ x1) (CAtom _ x2) = rule x1 x2
        rule2 _ _ = SOptional

sexpr :: (MonadParsec e s m) =>
         m b ->
         (b -> m c) ->
         m a ->
         m sp ->
         (a -> a -> SeparatorRule) ->
         m (SExpr a)
sexpr open close atom sep rule = removeAnn <$> sexprAnn open close atom sep rule
