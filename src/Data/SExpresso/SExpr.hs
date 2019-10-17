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
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module Data.SExpresso.SExpr
  (
    SExpr(..),
    Sexp,
    pattern A,
    pattern L,
    pattern Sexp,
    isAtom,
    sAtom,
    isList,
    sList
  )
  where

-- | The datatype 'SExpr' is the definition of an S-expression for the
-- library S-expresso.
--
-- The parameter @a@ allows you to specify the datatype of atoms and
-- the parameter @b@ is usefull for keeping metadata about S-expression
-- like source position for example.
data SExpr b a = SList b [SExpr b a]
               | SAtom a
               deriving (Eq, Show, Functor, Traversable, Foldable)

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
