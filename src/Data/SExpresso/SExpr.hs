{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module Data.SExpresso.SExpr
  (
    SExpr(..),
    pattern L,
    pattern A,
    pattern (:::),
    pattern Nil,
    sAtom,
    isAtom,
    sList,
    isList
  )
  where

data SExpr b a = SList b [SExpr b a]
               | SAtom a
               deriving (Eq, Show, Functor, Traversable, Foldable)

pattern L :: [SExpr b a] -> SExpr b a
pattern L xs <- SList _ xs

pattern A :: a -> SExpr b a
pattern A x = SAtom x

uncons :: SExpr b a -> Maybe (SExpr b a, SExpr b a)
uncons (SAtom _) = Nothing
uncons (SList _ []) = Nothing
uncons (SList b (x:xs)) = Just (x, SList b xs)

infixr 5 :::
pattern (:::) :: SExpr b a -> SExpr b a -> SExpr b a
pattern x ::: xs <- (uncons -> Just (x, xs))

pattern Nil :: SExpr b a
pattern Nil <- SList _ []

isAtom :: SExpr b a -> Bool
isAtom (A _) = True
isAtom _ = False

sAtom :: SExpr b a -> Maybe a
sAtom (SAtom x) = Just x
sAtom (SList _ _) = Nothing

sList :: SExpr b a -> Maybe [SExpr b a]
sList (SAtom _) = Nothing
sList (SList _ l) = Just l

isList :: SExpr b a -> Bool
isList = not . isAtom
