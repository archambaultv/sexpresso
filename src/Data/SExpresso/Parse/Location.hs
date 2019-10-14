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

{-# LANGUAGE OverloadedStrings #-}

module Data.SExpresso.Parse.Location
  (
    Location(..),
    Located(..),
    located
   )
  where

import Text.Megaparsec

-- | The 'Location' datatype represents a source span 
data Location = Span SourcePos SourcePos
              deriving (Eq, Ord, Show)

-- | The 'Located' datatype adds a source span to the type @a@
data Located a = At Location a
               deriving (Eq, Ord, Show)

-- | The 'located' function adds a source span to a parser.
located :: (MonadParsec e s m) => m a -> m (Located a)
located parser = do
  begin <- getSourcePos
  result <- parser
  end <- getSourcePos
  return $ At (Span begin end) result
