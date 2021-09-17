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
-- You can customize your 'SExpr' parser by specifying the following:
--
--   * The parser for atoms
--
--   * The opening tag, the closing tag, and a possible dependency of
--     the closing tag on the opening one.
--
--   * If some space is required or optional between any pair of
--     atoms.
--
--   * How to parse space (ex: treat comments as whitespace)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.SExpresso.Parse.Generic
  (
    SExprParser(..),
    
    getAtom,
    getSpace,
    getSpacingRule,

    setTags,
    setTagsFromList,
    setTagsFromMap,
    setSpace,
    setSpacingRule,
    setAtom,

    SpacingRule(..),
    spaceIsMandatory,
    spaceIsOptional,
    mkSpacingRule,
    
    withLocation,

    parseSExprList,
    parseSExpr,
    decodeOne,
    decode
   )
  where

import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad (mzero)
import Text.Megaparsec
import Data.SExpresso.SExpr
import Data.SExpresso.Parse.Location

-- | The 'SpacingRule' datatype is used to indicate if space is optional or mandatory between two consecutive @'SAtom' _@.
data SpacingRule =
  -- | Space is mandatory
  SMandatory
  -- | Space is optional
  | SOptional
   deriving (Show, Eq)

-- | The @'SExprParser' m b a@ datatype defines how to parse an
-- @'SExpr' b a@. Most parsing functions require the underlying monad
-- @m@ to be an instance of ('MonadParsec' e s m).

data SExprParser m b a
  -- | The @c@ parameter in the first two arguments is the type of the
  -- relation between the opening tag and the closing one.
  = forall c. SExprParser
  (m c) -- ^ The parser for the opening tag. Returns an object of an
        -- arbitrary type @c@ that will be used to create the closing
        -- tag parser.
  (c -> m b) -- ^ A function that takes the object returned by the
             -- opening tag parser and provide a parser for the
             -- closing tag.
  (m a) -- ^ The parser for atoms
  (m ()) -- ^ A parser for space tokens which does not accept empty
         -- input (e.g. 'Text.Megaparsec.Char.space1')
  (a -> a -> SpacingRule) -- ^ A function to tell if two consecutive
                          -- atoms must be separated by space or
                          -- not. See also 'mkSpacingRule' and
                          -- 'setSpacingRule'

-- | The 'getSpace' function returns the parser for whitespace of an 'SExprParser' object.
getSpace :: SExprParser m b a -> m ()
getSpace (SExprParser _ _ _ sp _) = sp

-- | The 'getSpacingRule' function returns spacing rule function of an 'SExprParser' object.
getSpacingRule :: SExprParser m b a -> (a -> a -> SpacingRule)
getSpacingRule (SExprParser _ _ _ _ sr) = sr

-- | The 'getAtom' function returns the parser for atoms of an 'SExprParser' object.
getAtom :: SExprParser m b a -> m a
getAtom (SExprParser _ _ a _ _) = a

-- | The 'withLocation' function adds source location to a @'SExprParser'@. See also 'Location'.
withLocation :: (MonadParsec e s m, TraversableStream s) => SExprParser m b a -> SExprParser m (Located b) (Located a)
withLocation (SExprParser pSTag pETag atom sp sr) =
  let s = do
        pos <- getSourcePos
        c <- pSTag
        return (pos, c)
      e = \(pos, c) -> do
        b <- pETag c
        pos2 <- getSourcePos
        return $ At (Span pos pos2) b
  in SExprParser s e (located atom) sp (\(At _ a1) (At _ a2) -> sr a1 a2)

-- | The 'setAtom' function updates a parser with a new parser for atoms and and new spacing rule function.
setAtom :: m a -> (a -> a -> SpacingRule) -> SExprParser m b a' -> SExprParser m b a
setAtom a sr (SExprParser pSTag pETag _ sp _) = SExprParser pSTag pETag a sp sr

-- | The 'setTags' function updates a parser with a new parser for the opening and closing tags.
setTags :: m c -> (c -> m b) -> SExprParser m b' a -> SExprParser m b a
setTags s e (SExprParser _ _ a sp sr) = SExprParser s e a sp sr

-- | The 'setTagsFromList' function helps you build the opening and
-- closing parsers from a list of triplets. Each triplet specifies a
-- stream of tokens to parse as the opening tag, a stream of tokens to
-- parse at the closing tag and what to return when this pair is
-- encountered. The 'setTagsFromList' can handle multiple triplets
-- with the same opening tags. See also 'setTagsFromMap'.
--
-- The example e1 parses "()" as @'SList' () []@.
--
-- > e1 = setTagsFromList [("(", ")", ()] p
--
-- The example e2 parses both "()" and "[]" as @'SList' () []@ but does
-- not parse "(]" or "[)"
--
-- > e2 = setTagsFromList [("(", ")", ()), ("[", "]", ())] p 
--
-- The example e3 parses "()" as @'SList' List []@ and "#()" as
-- @'SList' Vector []@, but does not parse "(]" or "[)"
--
-- > e3 = setTagsFromList [("(", ")", List), ("#(",")",Vector)] p
--
-- The example e4 parses "()" as @'SList' ')' []@ and "(]" as
-- @'SList' ']' []@, but does not parse "])"
--
-- > e4 = setTagsFromList [("(", ")", ')'), ("(", "]", ']')] p 
setTagsFromList ::  (MonadParsec e s m) =>
                    [(Tokens s, Tokens s, b)] -> SExprParser m b' a -> SExprParser m b a
setTagsFromList l p =
  let m = M.fromListWith (++) $ map (\(s,e,b) -> (s, [(e,b)])) l
  in setTagsFromMap m p

-- | The 'setTagsFromMap' function helps you build the opening and
-- closing parsers from a map. Each key specifies a stream of tokens to
-- parse as the opening tag and the value of the map specifies one or
-- more streams of tokens to parse at the closing tag and what to
-- return when this pair is encountered. See also 'setTagsFromList'.
--
-- The example e1 parses "()" as @'SList' () []@.
--
-- > e1 = setTagsFromList $ M.fromList [("(", [")", ()]] p
--
-- The example e2 parses both "()" and "[]" as @'SList' () []@ but does
-- not parse "(]" or "[)"
--
-- > e2 = setTagsFromList $ M.fromList [("(", [")", ()]), ("[", ["]", ()])] p 
--
-- The example e3 parses "()" as @'SList' List []@ and "#()" as
-- @'SList' Vector []@, but does not parse "(]" or "[)"
--
-- > e3 = setTagsFromList $ M.fromList [("(", [")", List]), ("#(", [")",Vector])] p
--
-- The example e4 parses "()" as @'SList' ')' []@ and "(]" as
-- @'SList' ']' []@, but does not parse "])"
--
-- > e4 = setTagsFromList $ M.fromList [("(", [(")", ')'), ("]", ']')])] p 
setTagsFromMap :: (MonadParsec e s m) =>
                  M.Map (Tokens s) [(Tokens s, b)] -> SExprParser m b' a -> SExprParser m b a
setTagsFromMap m p =
  let l = M.toList m

      choose [] = empty
      choose ((s, eb) : ts) = (chunk s >> return eb) <|> choose ts
      
      stag = choose l
      
      etag = \xs -> choice $ map (\(e, b) -> chunk e >> return b) xs
  in setTags stag etag p

-- | The 'spaceIsMandatory' function is a spacing rule where space is always mandatory. See also 'getSpacingRule'.
spaceIsMandatory :: a -> a -> SpacingRule
spaceIsMandatory = \_ _ -> SMandatory

-- | The 'spaceIsOptional' function is a spacing rule where space is always optional. See also 'getSpacingRule'.
spaceIsOptional :: a -> a -> SpacingRule
spaceIsOptional = \_ _ -> SOptional

-- | The 'setSpacingRule' function modifies a 'SExprParser' by setting
-- the function to tell if two consecutive atoms must be separated by
-- space or not. See also 'mkSpacingRule'.
setSpacingRule :: (a -> a -> SpacingRule) -> SExprParser m b a -> SExprParser m b a
setSpacingRule r p@(SExprParser pSTag pETag _ _ _) = SExprParser pSTag pETag (getAtom p) (getSpace p) r

-- | The 'mkSpacingRule' function is a helper to create a valid
-- spacing rule function for 'SExprParser' when some atoms have the
-- same 'SpacingRule' both before and after no matter what the other
-- atom is. It takes as argument a function @f@ that takes a single
-- atom and returns the 'SpacingRule' that applies both before and
-- after this atom.
--
-- For example, to create a spacing rule where space is optional both
-- before and after the fictitious @MyString@ token:
--
-- > s (MyString _) = SOptional
-- > s _ = Mandatory
-- > spacingRule = mkSpacingRule s
--
-- The above is equivalent to :
--
-- > spacingRule (MyString _) _ = SOptional
-- > spacingRule _ (MyString _) = SOptional
-- > spacingRule _ _ = SMandatory

mkSpacingRule :: (a -> SpacingRule) -> (a -> a -> SpacingRule)
mkSpacingRule f = \a1 a2 -> case f a1 of
                              SOptional -> SOptional
                              SMandatory -> f a2

-- | The 'setSpace' function modifies a 'SExprParser' by setting the
-- parser to parse whitespace. The parser for whitespace must not
-- accept the empty input (e.g. 'Text.Megaparsec.Char.space1')
setSpace :: m () -> SExprParser m b a -> SExprParser m b a
setSpace sp (SExprParser s e a _ sr) = SExprParser s e a sp sr

-- Tells if the space (or absence of) between two atoms is valid or not 
spaceIsOK :: (a -> a -> SpacingRule) -> (SExpr b a) -> (SExpr b a) -> Bool -> Bool
spaceIsOK getSpacingRule' sexp1 sexp2 spaceInBetween =
  case (sexp1, sexp2, spaceInBetween) of
    (_, _, True) -> True
    (SList _ _, _, _) -> True
    (_, SList _ _, _) -> True
    (SAtom a1, SAtom a2, _) -> getSpacingRule' a1 a2 == SOptional

sepEndBy' :: (MonadParsec e s m, TraversableStream s) => m (SExpr b a) -> m () -> (a -> a -> SpacingRule) -> m [SExpr b a]
sepEndBy' p sep f = sepEndBy1' p sep f <|> pure []

sepEndBy1' :: (MonadParsec e s m, TraversableStream s) => m (SExpr b a) -> m () -> (a -> a -> SpacingRule) -> m [SExpr b a]
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
              else label ("The previous two atoms are not separated by space.\n" <>
                         "A space was expected at " <> sourcePosPretty (fromJust mpos)) mzero

-- | The 'parseSExprList' function return a parser for parsing S-expression of the form @'SList' _ _@.
parseSExprList :: (MonadParsec e s m, TraversableStream s) =>
                SExprParser m b a -> m (SExpr b a)
parseSExprList def@(SExprParser pSTag pETag _ sp sr)  = do
          c <- pSTag
          _ <- optional sp
          xs <- sepEndBy' (parseSExpr def) sp sr
          b <- pETag c
          return $ SList b xs

-- | The 'parseSExpr' function return a parser for parsing
-- S-expression ('SExpr'), that is either an atom (@'SAtom' _@) or a
-- list @'SList' _ _@. See also 'decodeOne' and 'decode'.
parseSExpr :: (MonadParsec e s m, TraversableStream s) =>
              SExprParser m b a -> m (SExpr b a)
parseSExpr def = (getAtom def >>= return . SAtom) <|> (parseSExprList def)

-- | The 'decodeOne' function return a parser for parsing a file
-- containing only one S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decode'.
decodeOne :: (MonadParsec e s m, TraversableStream s) => SExprParser m b a -> m (SExpr b a)
decodeOne def =
  let ws = getSpace def
  in optional ws *> parseSExpr def <* (optional ws >> eof)

-- | The 'decode' function return a parser for parsing a file
-- containing many S-expression ('SExpr'). It can parse extra
-- whitespace at the beginning and at the end of the file. See also
-- 'parseSExpr' and 'decodeOne'.
decode :: (MonadParsec e s m, TraversableStream s) => SExprParser m b a -> m [SExpr b a]
decode def =
  let ws = getSpace def
  in optional ws *> sepEndBy' (parseSExpr def) ws (getSpacingRule def) <* eof
