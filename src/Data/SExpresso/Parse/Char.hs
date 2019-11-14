-- |
-- Module      :  Data.SExpresso.Parse.Char
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- The module "Data.SExpresso.Parse" re-exports the functions of this
-- module.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SExpresso.Parse.Char
  (
    simpleSExpr,
    simpleManySExpr,
    simpleManySExpr1
   )
  where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Generic
import Data.SExpresso.SExpr

-- | The function 'simpleSExpr' accepts a parser for atoms and
-- returns a parser for @'SExpr' a@ for a stream of 'Char' with the following
-- properties :
--
--  * The opening tag is (.
--  * The closing tag is ).
--  * The space parser is 'space1'.
--  * Space is always mandatory between atoms.
simpleSExpr :: (MonadParsec e s m, Token s ~ Char) =>
               m a ->
               m (SExpr a)
simpleSExpr p = sexpr (char '(')
                      (char ')')
                      p
                      space1
                      sepIsMandatory


-- | The function 'simpleSExpr' accepts a parser for atoms and
-- returns a parser for @'SExpr' a@ for a stream of 'Char' with the following
-- properties :
--
--  * The opening tag is (.
--  * The closing tag is ).
--  * The space parser is 'space1'.
--  * Space is always mandatory between atoms.
simpleManySExpr :: (MonadParsec e s m, Token s ~ Char) =>
               m a ->
               m [SExpr a]
simpleManySExpr p = manySExpr (char '(')
                    (char ')')
                    p
                    space1
                    sepIsMandatory

-- | The function 'simpleSExpr' accepts a parser for atoms and
-- returns a parser for @'SExpr' a@ for a stream of 'Char' with the following
-- properties :
--
--  * The opening tag is (.
--  * The closing tag is ).
--  * The space parser is 'space1'.
--  * Space is always mandatory between atoms.
simpleManySExpr1 :: (MonadParsec e s m, Token s ~ Char) =>
               m a ->
               m [SExpr a]
simpleManySExpr1 p = manySExpr1 (char '(')
                    (char ')')
                    p
                    space1
                    sepIsMandatory
