# Changelog for S-expresso

Version 1.1.0.0
---------------

* Add startPosPretty and endPosPretty function
* Add Bifunctor, Bifoldable and Bitraversable instances for SExpr
* Add Base SExpr, Recursive, Corecursive instances (see package recursion-schemes)
* Add Functor instance for Located
* Fix SExprPrinter constructor name (SExprParser -> SExprPrinter)
* Improve documentation

Version 1.0.0.2
---------------

* Initial Hackage Release
* SExpr datatype for representing S-expression
* Generic SExpr parser
* Specialized SExpr parser for character
* SExpr flat printer
* Scheme R5RS parser implementation
