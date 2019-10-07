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

------------------------- Generic SExpression Printer -------------------------
data SExprPrinter b a = SExprParser {
  printTags :: b -> [SExpr b a] -> (T.Text, T.Text),
  printAtom :: a -> T.Text
  }

mkPrinter :: (a -> T.Text) -> SExprPrinter b a
mkPrinter p = SExprParser (\_ _ -> ("(", ")")) p

flatPrintBuilder :: SExprPrinter b a -> SExpr b a -> B.Builder
flatPrintBuilder p (SAtom a) = B.fromText $ printAtom p a
flatPrintBuilder p (SList b xs) =
  let (sTag, eTag) = printTags p b xs
  in B.fromText sTag <> flatPrintList xs <> B.fromText eTag

  where flatPrintList [] = B.fromText ""
        flatPrintList [x] = flatPrintBuilder p x
        flatPrintList (y : ys) = flatPrintBuilder p y <> B.fromText " " <> flatPrintList ys

flatPrint :: SExprPrinter b a -> SExpr b a -> L.Text
flatPrint p s = B.toLazyText $ flatPrintBuilder p s
