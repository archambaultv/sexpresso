-- |
-- Module      :  Data.SExpresso.Print
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- Printing 'SExpr' as 'Data.Text'. To print as lazy text
-- ("Data.Text.Lazy") see "Data.Sexpresso.Print.Lazy"

module Data.SExpresso.Print (
  PL.SExprPrinter(..),
  PL.mkPrinter,
  flatPrint
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.SExpresso.SExpr
import qualified Data.SExpresso.Print.Lazy as PL

-- | Prints an 'SExpr' on a single line
flatPrint :: PL.SExprPrinter b a -> SExpr b a -> T.Text
flatPrint p s = L.toStrict $ PL.flatPrint p s
