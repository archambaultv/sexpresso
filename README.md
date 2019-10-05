# S-expresso

A small library to parse and print S-expressions. Also provides a literate mode to freely mix text and S-expression. 

## What is an S-expression ?

[S-expression](https://en.wikipedia.org/wiki/S-expression), used by languages like Lisp and Scheme, is a simple notation to define nested tree-structured data. Often used to represent source code, they can encode any tree like structure.

## Literate S-expression mode

S-expression usually start and end with a parenthesis. Therefore, it is easy to parse them. The idea is simple, if the first non whitespace character is an opening parenthesis, we parse the following characters as a standard S-expression. Otherwise, we skip the remaining of the line. Then we repeat the process, voila !

```
This line a interpreted as text, literate programming is great !

This line is also text, but the following line is interpreted as an S-expression
(define answer (+ 40 2))

Multiple S-expressions can be on the same line
(display answer) (define question "Unknown")

But remember, after reading a S-expression the parsing algorithm return to its
initial state, looking for the next non whitespace character.
So in the next line the second S-expression is interpreted as text
(display question) Who is the author ? (define author "Douglas Adams")
```

Lisp users may have noticed that I don't use Lisp syntax to define strings, that is because the library let you choose how you want to parse your atoms.

# How to use this library
Coming soon ...
