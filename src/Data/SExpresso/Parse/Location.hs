{-# LANGUAGE OverloadedStrings #-}

module Data.SExpresso.Parse.Location
  (
    Location(..),
    Located(..),
    located
   )
  where

import Text.Megaparsec

------------------------- Source Position -------------------------
data Location = Span SourcePos SourcePos
              deriving (Eq, Ord, Show)

data Located a = At Location a
               deriving (Eq, Ord, Show)

located :: (MonadParsec e s m) => m a -> m (Located a)
located parser = do
  begin <- getSourcePos
  result <- parser
  end <- getSourcePos
  return $ At (Span begin end) result
