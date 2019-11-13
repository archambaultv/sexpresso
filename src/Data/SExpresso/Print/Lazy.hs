-- |
-- Module      :  Data.SExpresso.Print.Lazy
-- Copyright   :  © 2019 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- Printing 'SExpr' as 'Data.Text.Lazy'. To print as strict text
-- ("Data.Text") see "Data.Sexpresso.Print"

{-# LANGUAGE OverloadedStrings #-}

module Data.SExpresso.Print.Lazy (
  SExprPrinter(..),
  mkPrinter,
  flatPrint,
  flatPrintBuilder
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Data.SExpresso.SExpr

-- | The 'SExprPrinter' defines how to print an 'SExpr'. 
data SExprPrinter b a = SExprPrinter {
  -- | The opening and closing tags based on the content of the 'SList'
  printTags :: b -> [SExpr b a] -> (T.Text, T.Text),
  -- | How to print an atom
  printAtom :: a -> T.Text
  }

-- | An 'SExprPrinter' with the opening tag defined as '(' and the
-- closing tag defined as ')'
mkPrinter :: (a -> T.Text) -> SExprPrinter b a
mkPrinter p = SExprParser (\_ _ -> ("(", ")")) p

-- | Prints an 'SExpr' on a single line. Returns a 'B.Builder' instead of a lazy text 'L.Text'
flatPrintBuilder :: SExprPrinter b a -> SExpr b a -> B.Builder
flatPrintBuilder p (SAtom a) = B.fromText $ printAtom p a
flatPrintBuilder p (SList b xs) =
  let (sTag, eTag) = printTags p b xs
  in B.fromText sTag <> flatPrintList xs <> B.fromText eTag

  where flatPrintList [] = B.fromText ""
        flatPrintList [x] = flatPrintBuilder p x
        flatPrintList (y : ys) = flatPrintBuilder p y <> B.fromText " " <> flatPrintList ys

-- | Prints an 'SExpr' on a single line
flatPrint :: SExprPrinter b a -> SExpr b a -> L.Text
flatPrint p s = B.toLazyText $ flatPrintBuilder p s
