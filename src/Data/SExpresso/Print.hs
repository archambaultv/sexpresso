{-# LANGUAGE OverloadedStrings #-}

module Data.SExpresso.Print (
  PL.SExprPrinter(..),
  PL.mkPrinter,
  flatPrint
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.SExpresso.SExpr
import qualified Data.SExpresso.Print.Lazy as PL

------------------------- Generic SExpression Printer -------------------------
flatPrint :: PL.SExprPrinter b a -> SExpr b a -> T.Text
flatPrint p s = L.toStrict $ PL.flatPrint p s
