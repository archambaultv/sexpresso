{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Data.SExpresso.Parse.Lexer
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


module Data.SExpresso.Parse.Lexer
  (
    SExprStream(..),
    sexprStream,
    getInputOffset,

    atom,
    atomToken,
    atomSatisfy,

    openDelimiter,
    openToken,
    openSatisfy,

    closeDelimiter,
    closeToken,
    closeSatisfy,
    
    sexprAnnL,
    sexprL,

    SExprToken(..),
    SourceOffset,
    SExprTokenPos,
    tokenLength,
    tokenOffset,
    sexprLexer,
   )
  where

import Data.Proxy
import Data.List
import qualified Data.Set as Set
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import Data.SExpresso.Parse.Combinators  
import Data.SExpresso.SExpr

data SExprToken b c a
  = OpenDelimiter b
  | AtomToken a
  | CloseDelimiter c
  deriving (Eq, Ord, Show)

type SExprTokenPos b c a = (SourceOffset, SExprToken b c a)

-- instance (Eq b, Eq c, Eq a) => Eq (SExprToken b c a) where
--   (OpenDelimiter b1 _ _) == (OpenDelimiter b2 _ _) = b1 == b2
--   (CloseDelimiter c1 _ _) == (CloseDelimiter c2 _ _) = c1 == c2
--   (AtomToken a1 _ _) == (AtomToken a2 _ _) = a1 == a2
--   _ == _ = False

-- instance (Ord b, Ord c, Ord a) => Ord (SExprToken b c a) where
--   compare (OpenDelimiter b1 _ _) (OpenDelimiter b2 _ _) = compare b1 b2
--   compare OpenDelimiter{} _ = LT
--   compare _ OpenDelimiter{} = GT
--   compare (AtomToken a1 _ _) (AtomToken a2 _ _) = compare a1 a2
--   compare AtomToken{} _ = LT
--   compare _ AtomToken{} = GT
--   compare (CloseDelimiter c1 _ _)  (CloseDelimiter c2 _ _) = compare c1 c2
  

tokenLength :: SExprTokenPos b c a -> Int
tokenLength = snd . fst
-- tokenLength (OpenDelimiter _ l _) = l
-- tokenLength (CloseDelimiter _ l _) = l
-- tokenLength (AtomToken _ l _) = l

tokenOffset :: SExprTokenPos b c a -> Int
tokenOffset = fst . fst
-- tokenOffset (OpenDelimiter _ _ o) = o
-- tokenOffset (CloseDelimiter _ _ o) = o
-- tokenOffset (AtomToken _ _ o) = o

showSExprToken :: (Show b, Show c, Show a) => SExprToken b c a -> String
showSExprToken (OpenDelimiter x) = show x
showSExprToken (CloseDelimiter x) = show x
showSExprToken (AtomToken x) = show x

data SExprStream s b c a = SExprStream {
  inputPosState :: PosState s,
  inputOffset :: Int, -- In sync with the SourceOffset of the
                      -- tokenStream, but also available when the list
                      -- is empty
  tokenStream :: [SExprTokenPos b c a]
  }

instance (Ord b, Ord c, Ord a, Show b, Show c, Show a, Stream s) => Stream (SExprStream s b c a) where
  type Token  (SExprStream s b c a) = SExprToken b c a
  type Tokens (SExprStream s b c a) = [SExprToken b c a]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  take1_ (SExprStream _ _ []) = Nothing
  take1_ (SExprStream p o (t:ts)) = Just (snd t, SExprStream p (o + tokenLength t) ts)

  takeN_ n (SExprStream p o s)
    | n <= 0    = Just ([], SExprStream p o s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (map snd x, SExprStream p (o + sum (map tokenLength x)) s')

  takeWhile_ f (SExprStream p o s) =
    let (x, s') = span (f . snd) s
    in (map snd x, SExprStream p (o + sum (map tokenLength x)) s')

  showTokens Proxy = intercalate " "
    . NE.toList
    . fmap showSExprToken

  reachOffset o p =
    let streamTok = tokenStream $ pstateInput p
        (preTokens, postTokens) = splitAt (o - pstateOffset p) streamTok
        strOffset = sum $ map tokenLength preTokens
        (pos, line, strPosState) = reachOffset strOffset (inputPosState $ pstateInput p)
    in (pos,
        line,
        PosState
         {pstateInput = SExprStream
                         { inputPosState = strPosState,
                           inputOffset = strOffset,
                           tokenStream = postTokens
                         }
        , pstateOffset = max (pstateOffset p) o
        , pstateSourcePos = pos
        , pstateTabWidth = pstateTabWidth p
        , pstateLinePrefix = pstateLinePrefix strPosState
        })


sexprRule :: (a -> a -> SeparatorRule) -> SExprTokenPos b c a -> SExprTokenPos b c a -> SeparatorRule
sexprRule f (_, AtomToken x1) (_, AtomToken x2) = f x1 x2
sexprRule _ _ _ = SOptional

-- | To get a parser for SExpr a, use 'removeAnnotation'
sexprLexer :: (MonadParsec e s m) =>
               m b -> -- ^ Start delimiter 
               m c -> -- ^ End delimiter 
               m a -> -- ^ The atom parser
               m sep -> -- ^ The separator parser
               (a -> a -> SeparatorRule) -> -- ^ Separator rule between atoms.
               m [SExprTokenPos b c a]
sexprLexer s e a sep r =
   let rSExpr = sexprRule r
       p = toToken a AtomToken <|>
           toToken s OpenDelimiter <|>
           toToken e CloseDelimiter
   in sepEndByRule p sep rSExpr

toToken :: (MonadParsec e s m) =>
             m z ->
             (z -> SExprToken b c a) ->
             m (SExprTokenPos b c a)
toToken p f = do
  o <- getOffset
  x <- p
  e <- getOffset
  return $ ((o, e - o), f x)

sexprStream :: (MonadParsec e s m) =>
               m b -> -- ^ Start delimiter 
               m c -> -- ^ End delimiter 
               m a -> -- ^ The atom parser
               m sep -> -- ^ The separator parser
               (a -> a -> SeparatorRule) -> -- ^ Separator rule between atoms.
               m (SExprStream s b c a)
sexprStream s e a sep r = do
  offset <- getOffset
  pos <- statePosState <$> getParserState
  tok <- sexprLexer s e a sep r
  return $ SExprStream pos offset tok

atom :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
        a ->
        m a
atom x = token
         (\c -> if c == AtomToken x then Just x else Nothing)
         (Set.singleton $ Tokens $ AtomToken x NE.:| [])


atomToken :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
     (a -> Maybe d) ->
     Maybe [a] ->
     m d
atomToken f expected = token test expected'
  where test OpenDelimiter{} = Nothing
        test CloseDelimiter{} = Nothing
        test (AtomToken x) = f x

        expected' =
          case expected of
            Nothing -> Set.empty
            Just [] -> Set.empty
            Just (a : as) -> Set.singleton $ Tokens $ AtomToken a NE.:| map AtomToken as

atomSatisfy :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
               (a -> Bool) ->
               m a
atomSatisfy f = atomToken test Nothing
  where test x = if f x then Just x else Nothing

openDelimiter :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
        b ->
        m b
openDelimiter x = token
         (\c -> if c == OpenDelimiter x then Just x else Nothing)
         (Set.singleton $ Tokens $ OpenDelimiter x NE.:| [])
         
openToken :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
     (b -> Maybe d) ->
     Maybe [b] -> 
     m d
openToken f expected = token test expected'
  where test (OpenDelimiter x) = f x
        test CloseDelimiter{} = Nothing
        test AtomToken{} = Nothing

        expected' =
          case expected of
            Nothing -> Set.empty
            Just [] -> Set.empty
            Just (a : as) -> Set.singleton $ Tokens $ OpenDelimiter a NE.:| map OpenDelimiter as


openSatisfy :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
               (b -> Bool) ->
               m b
openSatisfy f = openToken test Nothing
  where test x = if f x then Just x else Nothing


closeDelimiter :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
        c ->
        m c
closeDelimiter x = token
         (\c -> if c == CloseDelimiter x then Just x else Nothing)
         (Set.singleton $ Tokens $ CloseDelimiter x NE.:| [])
         
closeToken :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
     (c -> Maybe d) ->
     Maybe [c] ->
     m d
closeToken f expected = token test expected'
  where test OpenDelimiter{} = Nothing
        test (CloseDelimiter x) = f x
        test AtomToken{} = Nothing

        expected' =
          case expected of
            Nothing -> Set.empty
            Just [] -> Set.empty
            Just (a : as) -> Set.singleton $ Tokens $ CloseDelimiter a NE.:| map CloseDelimiter as


closeSatisfy :: (MonadParsec e s m, Token s ~ SExprToken b c a) =>
               (c -> Bool) ->
               m c
closeSatisfy f = closeToken test Nothing
  where test x = if f x then Just x else Nothing

getInputOffset :: (MonadParsec e s m, s ~ SExprStream s1 b c a) => m Int
getInputOffset = inputOffset . stateInput <$> getParserState

sexprAnnL :: (Eq b, Eq c, MonadParsec e s m, s ~ SExprStream s1 b c a) =>
         m x ->
         (x -> m y) ->
         m (SExprAnn SourceOffset a)
sexprAnnL open close = atomP <|> listP

  where atomP = do
          offset1 <- getInputOffset
          a <- atomToken Just Nothing <?> "atom"
          offset2 <- getInputOffset
          return $ CAtom (offset1, offset2 - offset1) a

        listP = do
          offset1 <- getInputOffset
          o <- open
          xs <- many (atomP <|> listP)
          _ <- close o
          offset2 <- getInputOffset
          return $ CList (offset1, offset2 - offset1) xs

sexprL :: (Eq b, Eq c, MonadParsec e s m, s ~ SExprStream s1 b c a) =>
         m x ->
         (x -> m y) ->
         m (SExpr a)
sexprL open close = removeAnn <$> sexprAnnL open close
