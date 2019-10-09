{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SExpresso.Parse.Char
  (
    plainSExprParser
   )
  where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Generic


plainSExprParser :: (MonadParsec e s m, Token s ~ Char) =>
                    m a -> SExprParser m () () a
plainSExprParser p = mkSExprParser
                     (char '(' >> return ())
                     (\_ -> char ')' >> return ())
                     p
                     space1
                     spaceIsMandatory
