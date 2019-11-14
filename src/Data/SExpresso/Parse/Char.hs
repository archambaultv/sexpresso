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
    plainSListDefinition
   )
  where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Generic


-- | The function 'plainSExprParser' accepts a parser for atoms and
-- returns a 'SExprParser' for a stream of 'Char' with the following
-- properties :
--
--  * The opening tag is (.
--  * The closing tag is ).
--  * The space parser is 'space1'.
--  * Space is always mandatory between atoms.
plainSListDefinition :: (MonadParsec e s m, Token s ~ Char) =>
                        m a -> SListDefinition m a
plainSListDefinition p = SListDefinition
                         (char '(' >> return ())
                         (char ')' >> return ())
                         p
                         space1
                         sepIsMandatory
