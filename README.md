[![Hackage](https://img.shields.io/hackage/v/sexpresso.svg)](http://hackage.haskell.org/package/sexpresso)

# S-expresso

S-expresso is a Haskell library designed to help you parse and print
data or source code encoded as an S-expression. It provides a very
flexible parser and (for now) a flat printer.

# What is an S-expression
Basically, an S-expression is a special form of tree structured
data. An S-expression object is either an atom or a list of atoms and other S-expressions.

This datatype is the definition of an S-expression for
S-expresso. 

~~~haskell
data SExpr b a = SList b [SExpr b a]
               | SAtom a
~~~

The parameter `a` allows you to specify the datatype of atoms and the
parameter `b` is usefull for keeping metadata about S-expression like
source position for example.

`SExpr` is not equivalent to `[a]` because the later cannot
distinguish between an atom `(SAtom _)` and a tree containing only one
atom `(SList _ [SAtom _])`. `SExpr` is also not equivalent to `Tree a`
from `Data.Tree` because the later cannot encode the empty tree
`(SList _ [])` and does not enforce that atoms are at the leaves.

## The Sexp type
If you are only interested by the atoms, you can use the type alias
`Sexp` that is a variant of the more general 'SExpr' data type with no
data for the 'SList' constructor.
~~~haskell
type Sexp a = SExpr () a
~~~

This type also comes with a bidirectional pattern synonym also named
`Sexp` for object of the form `SExpr () _`.
~~~
x = Sexp [A 3]                   <-> x = SList () [SAtom 3]
foo (Sexp xs)                    <-> foo (SList () xs)
foo (Sexp (Sexp ys : A x : xs))  <-> foo (SList () (SList () ys : SAtom x : xs))
~~~

## Pattern synonyms
S-expresso defines four pattern synonyms to ease your programming with
`SExpr`. The patterns `L` helps you match the `SList` constructor and only
its sublist, disregarding the `b` field. The pattern `:::` and `Nil` helps
you specify the shape of the sublist of an `SList` constructor and
finally the pattern `A` is a shorthand for `SAtom`.

Together they make working with `SExpr` a little easier.
~~~
a = A 3                      <-> a = SAtom 3
foo (A x)                    <-> foo (SAtom x)
foo (A x1 ::: A x2 ::: Nil)  <-> foo (SList _ [SAtom x1, SAtom x2])
foo (A x ::: L xs))          <-> foo (SList _ (SAtom x : xs))
foo (L ys ::: A x ::: L xs)) <-> foo (SList _ (SList _ ys : SAtom x : xs))
foo (L x)                    <-> foo (SList _ x)
~~~

Notice that you need to end the pattern `:::` with `Nil` for the empty
list or `L xs` for matching the remainder of the list. Indeed, if you write

~~~
foo (x ::: xs) = ...
~~~

this is equivalent to :

~~~
foo (SList b (x : rest)) = let xs = SList b rest
                           in ...
~~~

You can refer to the documentation of the `:::` constructor for more information.

# Parsing S-expressions
The parsing is based on
[megaparsec](http://hackage.haskell.org/package/megaparsec). S-expresso
allows you to customize the following :
* The parser for atoms
* The opening tag (usually "("), the closing tag (usually ")") and a
  possible dependency of the closing tag on the opening one.
* If some space is required or optional between any pair of atoms.
* How to parse space (ex: treat comments as whitespace)

The library offers amoung others the `decodeOne` and `decode`
functions. The former only reads one S-expression while the other
parses many S-expressions.  Both functions creates a megaparsec
parser from a `SExprParser` argument.

The `SExprParser` is the data type that defines how to read an
S-expression.  The easiest way to create a `SExprParser` is to use the
function `plainSExprParser` with your own custom atom parser. This
will create a parser where S-expression starts with "(", ends with ")"
and space is mandatory between atoms.

~~~haskell
Import Data.Void
Import qualified Data.Text as T
Import Text.Megaparsec
Import Text.Megaparsec.Char
Import qualified Text.Megaparser.Char.Lexer as L

atom = some letter

sexp = decode $ plainSExprParser atom

-- Returns (SList () [SAtom "hello", SAtom "world"])
ex1 = parse sexp "" "(hello world)"

-- Returns (SList () [SAtom "hello", SAtom "world", SList () [SAtom "bonjour"]])
ex2 = parse sexp "" "  (hello world(bonjour))  "

-- Returns SAtom "hola"
ex2 = parse sexp "" "hola"
~~~

## Customizing the SExprParser
S-expresso provides many functions to modify the behavior of the
parser. For example, you can use the functions `setTags`,
`setTagsFromList`, `setSpace` and `setSpacingRule` to modify the
behavior of the parser. Following on the preceding example:

~~~haskell
-- setTags
data MyType = List | Vector

listOrVector =
  let sTag = (char '(' >> return List) <|> (string "#(" >> return Vector)
      eTag = \t -> char ')' >> return t
      p = setTags sTag eTag $
          plainSExprParser atom
  in decode p

-- Returns (SList List [SList Vector [SAtom "a", SAtom "b"], SAtom "c"])
ex3 = parse listOrVector "" "(#(a b) c)"

-- setTagsFromList
listOrVector2 = decode $ 
                setTagsFromList [("(",")",List),("#(",")",Vector)] $
                plainSExprParser atom


-- Returns (SList List [SList Vector [SAtom "a", SAtom "b"], SAtom "c"])
ex4 = parse listOrVector2 "" "(#(a b) c)"

-- setSpace
withComments = decode $
               -- See megaparsec Space in Megaparsec.Char.Lexer
               setSpace (L.Space Space1 (skipLineComment ";") empty) $
               plainSExprParser atom

-- Returns (SList () [SAtom "hello", SList () [SAtom "bonjour"]])
ex5 = parse withComments "" "(hello ;world\n (bonjour))"

-- setSpacingRule
optionalSpace = decode $
                setSpacingRule spaceIsOptional $
                plainSExprParser (some letter <|> some digitChar)

-- Returns (SList () [SAtom "hello", SAtom "1234", SAtom "world"])
ex5 = parse optionalSpace "" "(hello1234world)"
~~~

You can also directly build a custom SExprParser with the constructor `SExprParser`.

## Adding Source Location
If you need the source position of the atoms and s-expression, the
function `withLocation` transforms an `SExprParser b a` into
`SExprParser (Located b) (Located a)`. The `Located` datatype is
defined
[here](https://github.com/archambaultv/sexpresso/blob/master/src/Data/SExpresso/Parse/Location.hs).
