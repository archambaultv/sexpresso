{-# LANGUAGE OverloadedStrings #-}

module Data.SExpresso.Parse.Generic
  (
    SExprParser(..),
    SpaceRule(..),
    spaceIsMandatory,
    spaceIsOptional,
    setSpace,
    setSpaceRule,
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

------------------------- Generic SExpression parser -------------------------
-- data SExprParserT e s m c b a = SExprParserT {
--   pSTag :: ParsecT e s m c,
--   pETag :: c -> ParsecT e s m b,
--   pAtom :: ParsecT e s m a,
--   pInterAtom :: ParsecT e s m ()
--   }

data SpaceRule = SMandatory | SOptional
   deriving (Show, Eq)

-- pBetweenAtom must parse zero or more whitespace
-- In the case of zero whitespace, pBetweenAtom must
-- ensure that the next token is a valid delimiter
data SExprParser m c b a = SExprParser {
  pSTag :: m c,
  pETag :: c -> m b,
  pAtom :: m a,
  pSpace :: m (), --A parser for space characters which does not accept empty input (e.g. Megaparsec space1)
  spaceRule :: a -> a -> SpaceRule -- A function to tell if two atoms must be separated by space or not
  }

spaceIsMandatory :: a -> a -> SpaceRule
spaceIsMandatory = \_ _ -> SMandatory

spaceIsOptional :: a -> a -> SpaceRule
spaceIsOptional = \_ _ -> SOptional
                                               
setSpace :: m () -> SExprParser m c b a -> SExprParser m c b a
setSpace c p = p{pSpace = c}

setSpaceRule :: (a -> a -> SpaceRule) -> SExprParser m c b a -> SExprParser m c b a
setSpaceRule r p = p{spaceRule = r}

-- Tells if the space (or absence of) between two atoms is valid or not 
spaceIsOK :: (a -> a -> SpaceRule) -> (SExpr b a) -> (SExpr b a) -> Bool -> Bool
spaceIsOK spaceRule' sexp1 sexp2 spaceInBetween =
  case (sexp1, sexp2, spaceInBetween) of
    (_, _, True) -> True
    (SList _ _, _, _) -> True
    (_, SList _ _, _) -> True
    (SAtom a1, SAtom a2, _) -> spaceRule' a1 a2 == SOptional

sepEndBy' :: (MonadParsec e s m) => m (SExpr b a) -> m () -> (a -> a -> SpaceRule) -> m [SExpr b a]
sepEndBy' p sep f = sepEndBy1' p sep f <|> pure []

sepEndBy1' :: (MonadParsec e s m) => m (SExpr b a) -> m () -> (a -> a -> SpaceRule) -> m [SExpr b a]
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
          xs <- sepEndBy' (parseSExpr def) (pSpace def) (spaceRule def)
          b <- pETag def c
          return $ SList b xs

          
   --        _ <- optional (pSpace def)
   --        mx <- optional (parseSExpr def)
   --        case mx of
   --          Nothing -> pETag def c >>= \b -> return (SList b [])
   --          Just x -> do
   --            xs <- parseContent x
   --            b <- pETag def c
   --            return (SList b (x : xs))

   -- where --parseContent :: (MonadParsec e s m) => (SExpr b a) -> m [SExpr b a]
   --       parseContent a1 = do
   --         s <- maybe False (const True) <$> optional (pSpace def)
   --         --pos <- getSourcePos
   --         mx <- optional (parseSExpr def)
   --         case mx of
   --           Nothing -> return []
   --           Just a2 ->
   --             if spaceIsOK (spaceRule def) a1 a2 s
   --             then do
   --               xs <- parseContent a2
   --               return $ a2 : xs
   --             else fail "Space is not optional"
          
          -- xs <- (pAtom def >> return . SAtom <* (pSpace def <|> pDelimiter)
          --       (parseSExprList def <* optional (pSpace def)
          
          -- _ <- optional (pSpace def)
          -- xs <- sepEndBy (parseSExpr def) (pSpace def)
          -- b <- pETag def c
          -- return $ SList b xs

parseSExpr :: (MonadParsec e s m) =>
              SExprParser m c b a -> m (SExpr b a)
parseSExpr def = (pAtom def >>= return . SAtom) <|> (parseSExprList def)

decodeOne :: (MonadParsec e s m) =>
              Maybe (m ()) -> SExprParser m c b a -> m (SExpr b a)
decodeOne pBetweenSExpr def =
  let ws = fromMaybe (pSpace def) pBetweenSExpr
  in optional ws *> parseSExpr def <* (optional ws >> eof)

decode :: (MonadParsec e s m) =>
           Maybe (m ()) -> SExprParser m c b a -> m [SExpr b a]
decode pBetweenSExpr def =
  let ws = fromMaybe (pSpace def) pBetweenSExpr
  in optional ws *> sepEndBy' (parseSExpr def) ws (spaceRule def) <* eof
