-- |
-- Module      :  Data.SExpresso.Parse.Location
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- The module "Data.SExpresso.Parse" re-exports the functions and
-- datatypes of this module.

{-# LANGUAGE DeriveFunctor #-}

module Data.SExpresso.Parse.Location
  (
    Location(..),
    Located(..),
    located,
    startPosPretty,
    endPosPretty
   )
  where

import Text.Megaparsec

-- Taken from https://www.reddit.com/r/haskell/comments/4x22f9/labelling_ast_nodes_with_locations/d6cmdy9/

-- | The 'Location' datatype represents a source span 
data Location = Span SourcePos SourcePos
              deriving (Eq, Ord, Show)

-- | Pretty prints @S1@ of a @'Span' S1 _@ object with 'sourcePosPretty'
startPosPretty :: Location -> String
startPosPretty (Span s _) = sourcePosPretty s

-- | Pretty prints @S2@ of a @'Span' _ S2@ object with 'sourcePosPretty'
endPosPretty :: Location -> String
endPosPretty (Span _ s) = sourcePosPretty s

-- | The 'Located' datatype adds a source span to the type @a@
data Located a = At Location a
               deriving (Eq, Ord, Show, Functor)

-- | The 'located' function adds a source span to a parser.
located :: (MonadParsec e s m, TraversableStream s) => m a -> m (Located a)
located parser = do
  begin <- getSourcePos
  result <- parser
  end <- getSourcePos
  return $ At (Span begin end) result
