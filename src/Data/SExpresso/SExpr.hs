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
    Sexp,
    pattern A,
    pattern L,
    pattern Sexp,
    pattern (:::),
    pattern Nil,
    isAtom,
    sAtom,
    isList,
    sList
  )
  where

import Data.Bifunctor.TH
import Data.Functor.Foldable.TH

-- | The datatype 'SExpr' is the definition of an S-expression for the
-- library S-expresso.
--
-- The parameter @a@ allows you to specify the datatype of atoms and
-- the parameter @b@ is usefull for keeping metadata about S-expression
-- like source position for example.
data SExpr b a = SList b [SExpr b a]
               | SAtom a
               deriving (Eq, Show, Functor, Traversable, Foldable)

$(deriveBifunctor ''SExpr)
$(deriveBifoldable ''SExpr)
$(deriveBitraversable ''SExpr)
$(makeBaseFunctor ''SExpr)

-- | The type synonym 'Sexp' is a variant of the more general 'SExpr'
-- datatype with no data for the 'SList' constructor.
type Sexp a = SExpr () a

-- | Bidirectional pattern synonym for the type synonym 'Sexp'. See
-- also the 'L' pattern synonym.
--
-- >foo (Sexp x) = x -- Equivalent to foo (SList () x) = x
-- >s = Sexp []      -- Equivalent to s = SList () []
pattern Sexp :: [Sexp a] -> Sexp a
pattern Sexp xs = SList () xs

-- | Pattern for matching only the sublist of the 'SList' constructor.
-- See also the Sexp pattern synonym.
--
-- >foo (L xs) = xs -- Equivalent to foo (SList _ xs) = xs
pattern L :: [SExpr b a] -> SExpr b a
pattern L xs <- SList _ xs

-- | Shorthand for 'SAtom'.
--
-- >foo (A x) = x -- Equivalent to foo (SAtom x) = x
-- > a = A 3      -- Equivalent to a = SAtom 3
pattern A :: a -> SExpr b a
pattern A x = SAtom x

uncons :: SExpr b a -> Maybe (SExpr b a, SExpr b a)
uncons (SAtom _) = Nothing
uncons (SList _ []) = Nothing
uncons (SList b (x:xs)) = Just (x, SList b xs)

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
-- >foo (A x1 ::: A x2 ::: Nil)   -- Equivalent to foo (SList _ [SAtom x1, SAtom x2])
-- >foo (L ys ::: A x ::: L xs)   -- Equivalent to foo (SList _ (SList _ ys : SAtom x : xs))
infixr 5 :::
pattern (:::) :: SExpr b a -> SExpr b a -> SExpr b a
pattern x ::: xs <- (uncons -> Just (x, xs))

-- | Pattern to mark the end of the list when using the pattern synonym ':::'
pattern Nil :: SExpr b a
pattern Nil <- SList _ []

-- | The 'isAtom' function returns 'True' iff its argument is of the
-- form @SAtom _@.
isAtom :: SExpr b a -> Bool
isAtom (A _) = True
isAtom _ = False

-- | The 'sAtom' function returns 'Nothing' if its argument is of the
-- form @SList _ _@ and @'Just' a@ if its argument is of the form @SAtom _@..
sAtom :: SExpr b a -> Maybe a
sAtom (A x) = Just x
sAtom _ = Nothing

-- | The 'isList' function returns 'True' iff its argument is of the
-- form @SList _ _@.
isList :: SExpr b a -> Bool
isList (L _) = True
isList _ = False

-- | The 'sList' function returns 'Nothing' if its argument is of the
-- form @SAtom _@ and the sublist @xs@ if its argument is of the form
-- @SList _ xs@.
sList :: SExpr b a -> Maybe [SExpr b a]
sList (L l) = Just l
sList _ = Nothing
