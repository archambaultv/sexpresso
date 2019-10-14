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
    pattern (:::),
    pattern Nil,
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

-- | Bidirectional pattern synonym for the type synonym 'Sexp'
--
-- >foo (Sexp x) = x -- Equivalent to foo (SList () x) = x
-- >s = Sexp [] -- Equivalent to s = SList () []
pattern Sexp :: [Sexp a] -> Sexp a
pattern Sexp xs = SList () xs

-- | Pattern for matching only the sublist of the 'SList' constructor
--
-- >foo (L xs) = xs -- Equivalent to foo (SList _ xs) = xs
pattern L :: [SExpr b a] -> SExpr b a
pattern L xs <- SList _ xs

-- | Shorthand for 'SAtom'
--
-- >foo (A x) = x -- Equivalent to foo (SAtom x) = x
pattern A :: a -> SExpr b a
pattern A x = SAtom x

uncons :: SExpr b a -> Maybe (SExpr b a, SExpr b a)
uncons (SAtom _) = Nothing
uncons (SList _ []) = Nothing
uncons (SList b (x:xs)) = Just (x, SList b xs)

-- | Pattern specifying the shape of the sublist of the 'SList' constructor.
-- See also 'Nil'.
--
-- >foo (A x1 ::: A x2 ::: Nil) -- Equivalent to foo (SList _ [SAtom x1, SAtom x2])
-- >foo (A x ::: xs)            -- Equivalent to foo (SList _ (SAtom x : xs))
-- >foo (L ys ::: A x ::: xs)   -- Equivalent to foo (SList _ (SList _ ys : SAtom x : xs))
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
sAtom (SAtom x) = Just x
sAtom (SList _ _) = Nothing

-- | The 'isList' function returns 'True' iff its argument is of the
-- form @SList _ _@.
isList :: SExpr b a -> Bool
isList (L _) = True
isList _ = False

-- | The 'sList' function returns 'Nothing' if its argument is of the
-- form @SAtom _@ and the sublist @xs@ if its argument is of the form
-- @SList _ xs@.
sList :: SExpr b a -> Maybe [SExpr b a]
sList (SAtom _) = Nothing
sList (SList _ l) = Just l
