# Changelog for S-expresso

Version 1.1.0.0
---------------

* Add startPosPretty and endPosPretty function
* Add Bifunctor, Bifoldable and Bitraversable instances for SExpr
* Add Base SExpr, Recursive, Corecursive instances (see package recursion-schemes)
* Add Functor instance for Located
* Fix SExprPrinter constructor name (SExprParser -> SExprPrinter)
* Improve documentation
* Merge [pull request \#6](https://github.com/archambaultv/sexpresso/pull/6) to prepare for MonadFail
* Fix bug with R5RS negative number (issue \#7 on [github](https://github.com/archambaultv/sexpresso/issues/7))

Version 1.0.0.2
---------------

* Initial Hackage Release
* SExpr datatype for representing S-expression
* Generic SExpr parser
* Specialized SExpr parser for character
* SExpr flat printer
* Scheme R5RS parser implementation
