{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SExpresso.Parse.Char
  (
    mkSExprParser
   )
  where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Generic

  
mkSExprParser :: (MonadParsec e s m, Token s ~ Char) =>
                 m a -> SExprParser m () () a
mkSExprParser p = SExprParser {
  pSTag = char '(' >> return (),
  pETag = \_ -> char ')' >> return (),
  pAtom = p,
  pSpace = space1,
  spaceRule = spaceIsMandatory} --spaceOrParen}

-- spaceOrParen :: (MonadParsec e s m, Token s ~ Char) => m ()
-- spaceOrParen = space1 <|>
--                (lookAhead (oneOf "()") >> return ())
  
-- literateParser :: (Ord e, Stream s, Monad m) =>
--                   (Located T.Text -> a) -> ParsecT e s m (Tree a) -> ParsecT e s m [Tree a]
-- literateParser f parser =
--   let restOfLine = fmap (flip Node [] . f)
--                    $ located
--                    $ takeWhileP Nothing (\c -> c /= '\n' || c /= '\r')
--                      <* (eof <|> (eol >> return ()))
--   in many (space >> (parser <|> restOfLine))
