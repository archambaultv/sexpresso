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
  SExprPrinter(..),
  simplePrinter,
  sexprTranslator,
  flatPrint,
  ) where

import Data.List
import Data.SExpresso.SExpr

type Translator s a = a -> Either (s, s, [a]) s

-- | The 'SExprPrinter' defines how to print an object.
data SExprPrinter s a = SExprPrinter {
  translator :: Translator s a,
  separator :: s
  }

-- | The 'simplePrinter' function returns a 'SExprPrinter' object configure with basic settings
simplePrinter :: Translator s a -> s -> SExprPrinter s a
simplePrinter t s = SExprPrinter t s

sexprTranslator :: s -> s -> (a -> s) -> Translator s (SExpr a)
sexprTranslator sTag eTag atom =
  let f s = case s of
              SList as -> Left (sTag, eTag, as)
              SAtom a -> Right $ atom a
  in f

-- | Prints an 'SExpr' on a single line. Returns a 'B.Builder' instead of a lazy text 'L.Text'
flatPrint :: (Monoid s) => SExprPrinter s a -> a -> s
flatPrint p x = either printSList id (translator p x)

  where printSList (sTag, eTag, as) = sTag <> flatPrintList as <> eTag

        flatPrintList = mconcat . intersperse (separator p) . map (flatPrint p)
