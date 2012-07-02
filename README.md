# JavaScript: The Good Parts -- an AST and Pretty Printer

## Introduction

In Chapter 2 of "JavaScript: The Good Parts", Douglas Crockford presents a
concrete grammar for "the good parts" of JavaScript.

This package provides an abstract grammar and pretty printer for those good parts. We will abbreviate this language to JS:TGP.

Crockford presents the grammar as a series of railroad diagrams.
The correspondence between the concrete grammar and the abstract grammar
in this module is NOT one-to-one. However, the following property does hold: the
pretty printing of an abstract syntax tree will be parseable by the concrete grammar. i.e.
For each valid program produced by the concrete grammar there is a corresponding
abstract syntax tree that when pretty printed will produce that program (modulo whitespace).

The abstract grammar:
  * removes unnecessary characters such as parentheses (normal, curly and square)
  * represents JavaScript's string, name and number literals directly in Haskell as
    'String', 'String' and 'Double' respectively.

## Correct by construction

This library was designed so that it would be impossible (save for name and string literals)
to construct an incorrect JS:TGP program. To this end some of the data structures may look like
they contain redundancy.

For instance, consider the 'JSESDelete' constructor which is defined

```haskell
JSESDelete JSExpression JSInvocation
```

Why not just define it as `JSESDelete JSExpression` since type `JSExpression`
has a constructor defined as `JSExpressionInvocation JSExpression JSInvocation`?
The reason is that this would allow incorrect programs. A `JSExpression` is
not necessarily an invocation.

## Precedence of JavaScript operators

Interestingly, the precedence of JavaScript operators is
not defined in the ECMAScript standard. The precedence used in this library was guided by a page on 
the Mozilla Developer's Network pages.
(https://developer.mozilla.org/en/JavaScript/Reference/Operators/Operator_Precedence)

The precise precedence numbers from that page were not used but they have been stratified into
the same groups.

Interestingly, we don't even need to consider the associativity/precedence of
"=", "+=", "-=" etc. In JS:TGP the notion of *expression statements* is quite different
to that of *expressions*. It simply isn't legal to write an expression statement like

```javascript
(a += 2) -= 3
```

or

```javascript
 a = (b = c) = (c = d)
```

although it is perfectly legal to write

```javascript
 a = b = c = d += 2
```
which if we add brackets to disambiguate is really

```javascript
a = (b = (c = (d += 2)))
```

## Interesting aspects of "the good parts"

### Functions are not declared; they are values

A JS:TGP program is a collection of statements. You'll note that there is no
statement to declare a function in JS:TGP. However you can assign a function literal
to a variable.

e.g. ```var fun = function(x) { return x + 1;}```

What about recursive functions then? There is the option to give the function a name which is
local to the literal.

e.g. 

```javascript
var factorial = function f(n) {
                       if ( n > 0 ) {
                         return n * f(n - 1);
                       } else {
                         return 1;
                       }
                     }
```

`f` is local. It will not be in scope outside of the function body.

