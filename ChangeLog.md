# Changelog for S-expresso

Version 2.0.0.0
---------------
This version features a different approach for parsing, printing and for the `SExpr` data
type. It will break existing code with version 1.x.x.x.

* Remove the `b` parameter in the datatype `SExpr`. For keeping metadata about
  the `SList` constructor use the recursive scheme approach. See the README
file for examples

* Rename data type Location to Span
* Located in now a alias for the tuple (Span, a)

* Remove SchemeToken and related functions. Scheme R5RS directly parses Datum.

* Remove SExprParser
* Remove decode and decodeOne functions
* Rename parseSExpr -> sexpr
* Add manySExpr and manySExpr1 parsers
* Add sepByRule, sepEndByRule, sepByList families of functions to help parse custom S-expression

* Add Foldable, Traversable instance for Located

* Change constructor of SExprPrinter so that it works for any type isomorphic to SExpr
* The printer now works for any Monoid instead of text
* Remove Print.Lazy since it now works for all Monoid instances

Version 1.1.0.0
---------------

* Add startPosPretty and endPosPretty function
* Add Bifunctor, Bifoldable and Bitraversable instances for SExpr
* Add Base SExpr, Recursive, Corecursive instances (see package recursion-schemes)
* Add Functor instance for Located

Version 1.0.0.2
---------------

* Update Resolver
* Update synopsis

Version 1.0.0.1
---------------

* Add version bounds to the dependencies

Version 1.0.0.0
---------------

* Change type of SExprParser from `SExpParser m c b a` to `SExprParser m
  b a`. The `c` parameter is now an existential. 

* `SExprParser` is not a record anymore. So `pAtom`, `pSpace` and
  `pSpacingRule` are now functions and cannot be used in record
  syntax. The have been rename to `getAtom`, `getSpace` and
  `getSpacingRule`.

* The `pSTag` and `pETag` functions have been removed since `SExprParser`
  is defined using an existential.
  
* Documentation improvements.

Version 0.1.1.1
---------------

* Fix documentation error for the pattern :::

Version 0.1.1.0
---------------

* Add Scheme R5RS language

Version 0.1.0.0
--------------- 

* SExpr datatype
* Generic SExpr parser
* SExpr parser for character
* SExpr flat printer
