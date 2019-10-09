{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.SExpresso.Parse.Generic
  (
    SExprParser(..),
    mkSExprParser,
    SpacingRule(..),
    setTags,
    setTagsFromList,
    spaceIsMandatory,
    spaceIsOptional,
    setSpace,
    setSpacingRule,
    setAtom,
    mkSpacingRule,
    withLocation,
    parseSExprList,
    parseSExpr,
    decodeOne,
    decode
   )
  where

import Data.Maybe
import Control.Applicative
import Text.Megaparsec
import Data.SExpresso.SExpr
import Data.SExpresso.Parse.Location

------------------------- Generic SExpression parser -------------------------
data SpacingRule = SMandatory | SOptional
   deriving (Show, Eq)

data SExprParser m c b a = SExprParser {
  pSTag :: m c,
  pETag :: c -> m b,
  pAtom :: m a,
  pSpace :: m (), --A parser for space characters which does not accept empty input (e.g. Megaparsec space1)
  pSpacingRule :: a -> a -> SpacingRule -- A function to tell if two atoms must be separated by space or not
  }


mkSExprParser :: m c -> (c -> m b) -> m a -> m () -> (a -> a -> SpacingRule) -> SExprParser m c b a
mkSExprParser = SExprParser

withLocation :: (MonadParsec e s m) => SExprParser m c b a -> SExprParser m (SourcePos, c) (Located b) (Located a)
withLocation p =
  let s = do
        pos <- getSourcePos
        c <- pSTag p
        return (pos, c)
      e = \(pos, c) -> do
        b <- pETag p c
        pos2 <- getSourcePos
        return $ At (Span pos pos2) b
  in setTags s e $ setAtom (located (pAtom p)) (\(At _ a1) (At _ a2) -> pSpacingRule p a1 a2) p
        
setAtom :: m a -> (a -> a -> SpacingRule) -> SExprParser m c b a' -> SExprParser m c b a
setAtom a sr p = mkSExprParser (pSTag p) (pETag p) a (pSpace p) sr

setTags :: m c -> (c -> m b) -> SExprParser m c' b' a -> SExprParser m c b a
setTags s e p = mkSExprParser s e (pAtom p) (pSpace p) (pSpacingRule p)

setTagsFromList ::  (MonadParsec e s m) =>
                    [(Tokens s, Tokens s, b)] -> SExprParser m c' b' a -> SExprParser m (Tokens s, b) b a
setTagsFromList [] _ = error "setTagsFromList does not accept the empty list"
setTagsFromList l p =
  let --choose :: [(Tokens s, Tokens s)] -> m (Tokens s, Tokens s)
      choose [] = empty
      choose ((s, e, b) : ts) = (chunk s >> return (e, b)) <|> choose ts
      
      stag = choose l
      etag = \(e, b) -> chunk e >> return b
  in setTags stag etag p
        
spaceIsMandatory :: a -> a -> SpacingRule
spaceIsMandatory = \_ _ -> SMandatory

spaceIsOptional :: a -> a -> SpacingRule
spaceIsOptional = \_ _ -> SOptional
                                               
setSpace :: m () -> SExprParser m c b a -> SExprParser m c b a
setSpace c p = p{pSpace = c}

setSpacingRule :: (a -> a -> SpacingRule) -> SExprParser m c b a -> SExprParser m c b a
setSpacingRule r p = p{pSpacingRule = r}

mkSpacingRule :: (a -> SpacingRule) -> (a -> a -> SpacingRule)
mkSpacingRule f = \a1 a2 -> case f a1 of
                              SOptional -> SOptional
                              SMandatory -> f a2

-- Tells if the space (or absence of) between two atoms is valid or not 
spaceIsOK :: (a -> a -> SpacingRule) -> (SExpr b a) -> (SExpr b a) -> Bool -> Bool
spaceIsOK pSpacingRule' sexp1 sexp2 spaceInBetween =
  case (sexp1, sexp2, spaceInBetween) of
    (_, _, True) -> True
    (SList _ _, _, _) -> True
    (_, SList _ _, _) -> True
    (SAtom a1, SAtom a2, _) -> pSpacingRule' a1 a2 == SOptional

sepEndBy' :: (MonadParsec e s m) => m (SExpr b a) -> m () -> (a -> a -> SpacingRule) -> m [SExpr b a]
sepEndBy' p sep f = sepEndBy1' p sep f <|> pure []

sepEndBy1' :: (MonadParsec e s m) => m (SExpr b a) -> m () -> (a -> a -> SpacingRule) -> m [SExpr b a]
sepEndBy1' p sep f = do
  x <- p
  xs <- parseContent x
  return $ x : xs

  where parseContent a1 = do
          s <- maybe False (const True) <$> optional sep
          mpos <- if not s then Just <$> getSourcePos else return Nothing 
          mx <- optional p
          case mx of
            Nothing -> return []
            Just a2 ->
              if spaceIsOK f a1 a2 s
              then do
                xs <- parseContent a2
                return $ a2 : xs
              else fail ("The previous two atoms are not separated by space.\n" ++
                         "A space was expected at " ++ sourcePosPretty (fromJust mpos))
          
parseSExprList :: (MonadParsec e s m) =>
                SExprParser m c b a -> m (SExpr b a)
parseSExprList def = do
          c <- pSTag def
          _ <- optional (pSpace def)
          xs <- sepEndBy' (parseSExpr def) (pSpace def) (pSpacingRule def)
          b <- pETag def c
          return $ SList b xs

parseSExpr :: (MonadParsec e s m) =>
              SExprParser m c b a -> m (SExpr b a)
parseSExpr def = (pAtom def >>= return . SAtom) <|> (parseSExprList def)

decodeOne :: (MonadParsec e s m) => SExprParser m c b a -> m (SExpr b a)
decodeOne def =
  let ws = pSpace def
  in optional ws *> parseSExpr def <* (optional ws >> eof)

decode :: (MonadParsec e s m) => SExprParser m c b a -> m [SExpr b a]
decode def =
  let ws = pSpace def
  in optional ws *> sepEndBy' (parseSExpr def) ws (pSpacingRule def) <* eof
