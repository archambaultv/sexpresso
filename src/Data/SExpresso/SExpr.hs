-- |
-- Module      :  Data.SExpresso.SExpr
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- Definition of S-expression

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.SExpresso.SExpr
  (
    SExpr(..),
    pattern A,
    pattern L,
    pattern (:::),
    pattern Nil,
    isAtom,
    sAtom,
    isList,
    sList,
    sexprToTree,
    treeToSExpr
  )
  where

import Data.Tree
import Data.Functor.Foldable.TH

-- | The datatype 'SExpr' is the definition of an S-expression for the
-- library S-expresso.
--
-- The parameter @a@ allows you to specify the datatype of atoms
data SExpr a = SList [SExpr a]
             | SAtom a
             deriving (Eq, Show, Functor, Traversable, Foldable)

$(makeBaseFunctor ''SExpr)

-- | Pattern for matching only the sublist of the 'SList' constructor.
-- See also the Sexp pattern synonym.
--
-- >foo (L xs) = xs -- Equivalent to foo (SList xs) = xs
pattern L :: [SExpr a] -> SExpr a
pattern L xs = SList xs

-- | Shorthand for 'SAtom'.
--
-- >foo (A x) = x -- Equivalent to foo (SAtom x) = x
-- > a = A 3      -- Equivalent to a = SAtom 3
pattern A :: a -> SExpr a
pattern A x = SAtom x

uncons :: SExpr a -> Maybe (SExpr a, SExpr a)
uncons (SAtom _) = Nothing
uncons (SList []) = Nothing
uncons (SList (x:xs)) = Just (x, SList xs)

-- | Pattern specifying the shape of the sublist of the 'SList' constructor.
-- See also 'Nil'.
--
-- Although it aims to mimic the behavior of the cons (:) constructor
-- for list, this pattern behavior is a little bit different. Indeed
-- its signature is @SExpr b a -> SExpr b a -> SExpr b a@ while the
-- cons (:) constructor signature is @a -> [a] -> [a]@. The first
-- argument type is different in the case of the cons constructor but all
-- the types are identical for the pattern `:::`.
--
-- This implies that the following code
--
-- >foo (x ::: xs) = ...
-- is equivalent to
--
-- >foo (SList b (x : rest)) = let xs = SList b rest
-- >                           in ...
-- If you wish for the @xs@ above to match the remaining of the list,
-- you need to use the 'L' pattern
--
-- >foo (A x ::: L xs)
-- which is equivalent to
-- 
-- >foo (SList b (x : rest)) = let (SList _ xs) = SList b rest
-- >                           in ...
--
-- Other examples :
--
-- >foo (A x1 ::: A x2 ::: Nil)   -- Equivalent to foo (SList [SAtom x1, SAtom x2])
-- >foo (L ys ::: A x ::: L xs)   -- Equivalent to foo (SList (SList ys : SAtom x : xs))
infixr 5 :::
pattern (:::) :: SExpr a -> SExpr a -> SExpr a
pattern x ::: xs <- (uncons -> Just (x, xs))

-- | Pattern to mark the end of the list when using the pattern synonym ':::'
pattern Nil :: SExpr a
pattern Nil = SList []

-- | The 'isAtom' function returns 'True' iff its argument is of the
-- form @SAtom _@.
isAtom :: SExpr a -> Bool
isAtom (A _) = True
isAtom _ = False

-- | The 'sAtom' function returns 'Nothing' if its argument is of the
-- form @SList _ _@ and @'Just' a@ if its argument is of the form @SAtom _@..
sAtom :: SExpr a -> Maybe a
sAtom (A x) = Just x
sAtom _ = Nothing

-- | The 'isList' function returns 'True' iff its argument is of the
-- form @SList _ _@.
isList :: SExpr a -> Bool
isList (L _) = True
isList _ = False

-- | The 'sList' function returns 'Nothing' if its argument is of the
-- form @SAtom _@ and the sublist @xs@ if its argument is of the form
-- @SList _ xs@.
sList :: SExpr a -> Maybe [SExpr a]
sList (L l) = Just l
sList _ = Nothing

-- | The 'sexprToTree' function returns a @'Tree' ('Maybe' a)@ object
-- where all internal nodes have value @'Nothing'@ and the leaves have
-- value @'Just' a@
sexprToTree :: SExpr a -> Tree (Maybe a)
sexprToTree (SAtom a) = Node (Just a) []
sexprToTree (SList as) = Node Nothing (map sexprToTree as)

-- | The 'treeToSExpr' function returns a 'SExpr' object from a 'Tree'
-- object by only considering the leaves and ignoring the values of
-- internal nodes.
treeToSExpr :: Tree a -> SExpr a
treeToSExpr (Node a []) = A a
treeToSExpr (Node _ as) = L (map treeToSExpr as)
