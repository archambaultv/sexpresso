# Changelog for S-expresso

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
  
* Remove the `:::` pattern synonym for `SExpr`.

* Documentation improvements.


Version 0.1.1.0
---------------

* Add Scheme R5RS language

Version 0.1.0.0
--------------- 

* SExpr datatype
* Generic SExpr parser
* SExpr parser for character
* SExpr flat printer
