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
{-# LANGUAGE TupleSections #-}

module Data.SExpresso.SExpr
  (
    SExpr(..),
    SExprF(..),
    SExprAnn,
    getAnn,
    removeAnn,
    getSExprF,
    pattern (:&),
    pattern CAtom,
    pattern CList,
    pattern (:::),
    pattern SNil,
--    pattern CNil,
    isAtom,
    sAtom,
    isList,
    sList,
    sexprToTree
  )
  where

import Data.Tree
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

-- | The datatype 'SExpr' is the definition of an S-expression for the
-- library S-expresso.
--
-- The parameter @a@ allows you to specify the datatype of atoms
data SExpr a = SList [SExpr a]
             | SAtom a
             deriving (Eq, Show, Functor, Traversable, Foldable)

$(makeBaseFunctor ''SExpr)

-- | The type 'SExprAnn' represents a annotated SExpr where each node is annotated
type SExprAnn info a = Fix (Compose ((,) info) (SExprF a))

getAnn :: SExprAnn info a -> info
getAnn = fst . getCompose . unfix

getSExprF :: SExprAnn info a -> SExprF a (SExprAnn info a)
getSExprF = snd . getCompose . unfix

-- | Pattern for the Compose functor. Usefull for writing algebra and
-- co-algebra for 'SExprAnn'.
pattern (:&) :: info -> f a -> Compose ((,) info) f a
pattern x :& y = Compose (x, y)

removeAnn :: SExprAnn info a -> SExpr a
removeAnn = hoist (snd . getCompose)
                             
-- | Pattern for matching only the sublist of the 'SList' constructor.
-- See also the Sexp pattern synonym.
--
-- >foo (L xs) = xs -- Equivalent to foo (SList xs) = xs
pattern CList :: info -> [SExprAnn info a] -> SExprAnn info a
pattern CList info xs = Fix (Compose (info, SListF xs))

-- pattern LC :: info -> [SExprAnn info a] -> SExprAnn info a
-- pattern LC info xs = Fix (info :& SListF xs)

-- | Shorthand for 'SAtom'.
--
-- >foo (A x) = x -- Equivalent to foo (SAtom x) = x
-- > a = A 3      -- Equivalent to a = SAtom 3
pattern CAtom :: info -> a -> SExprAnn info a
pattern CAtom info x = Fix (Compose (info, SAtomF x))

-- pattern AC :: info -> a -> SExprAnn info a
-- pattern AC info x = Fix (Compose (info, SAtomF x))

uncons :: SExpr a -> Maybe (SExpr a, SExpr a)
uncons (SAtom _) = Nothing
uncons (SList []) = Nothing
uncons (SList (x:xs)) = Just (x, SList xs)

-- | Pattern specifying the shape of the sublist of the 'SList' constructor.
-- See also 'SNil'.
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
pattern SNil :: SExpr a
pattern SNil = SList []

-- pattern CNil :: info -> SExprAnn info a
-- pattern CNil info = Fix (Compose (info, SListF []))

-- | The 'isAtom' function returns 'True' iff its argument is of the
-- form @SAtom _@.
isAtom :: SExpr a -> Bool
isAtom (SAtom _) = True
isAtom _ = False

-- | The 'sAtom' function returns 'Nothing' if its argument is of the
-- form @SList _ _@ and @'Just' a@ if its argument is of the form @SAtom _@..
sAtom :: SExpr a -> Maybe a
sAtom (SAtom x) = Just x
sAtom _ = Nothing

-- | The 'isList' function returns 'True' iff its argument is of the
-- form @SList _ _@.
isList :: SExpr a -> Bool
isList (SList _) = True
isList _ = False

-- | The 'sList' function returns 'Nothing' if its argument is of the
-- form @SAtom _@ and the sublist @xs@ if its argument is of the form
-- @SList _ xs@.
sList :: SExpr a -> Maybe [SExpr a]
sList (SList l) = Just l
sList _ = Nothing

-- | The 'sexprToTree' function returns a @'Tree' ('Maybe' a)@ object
-- where all internal nodes have value @'Nothing'@ and the leaves have
-- value @'Just' a@
sexprToTree :: SExpr a -> Tree (Maybe a)
sexprToTree (SAtom a) = Node (Just a) []
sexprToTree (SList as) = Node Nothing (map sexprToTree as)
