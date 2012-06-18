# JavaScript: The Good Parts -- an AST and Pretty Printer

## Introduction

In Chapter 2 of "JavaScript: The Good Parts", Douglas Crockford presents a
concrete grammar for "the good parts" of JavaScript.

This module provides an abstract grammar for those good parts. We will abbreviate this
language to JS:TGP

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

## The data structure ensures no incorrect JS:TGP programs

This library was designed so that it would be impossible, save for name, string literals
to construct an incorrect JS:TGP program.

To this end some of the data structures may look like they contain redundancy.
For instance, consider the 'JSESDelete' constructor which is defined

```haskell
JSESDelete JSExpression JSInvocation
```

Why not just define it as `JSESDelete JSExpression` since type `JSExpression`
has a constructor defined as `JSExpressionInvocation JSExpression JSInvocation`?
The reason is that this would allow incorrect programs. A `JSExpression` is
not necessarily an invocation.

However, even though none of the programs are incorrect there are still some things
it cannot check for. Although labels can appear before any statement, they should
only be used on statements that interact with a switch, while, do, or for statement.
This cannot be checked except by a second pass over the AST. This library does not
provide this functionality.

## A note on precedence of JavaScript operators

Interestingly, the precedence of JavaScript operators is
not defined in the ECMAScript standard. The precedence used in this library comes from
the Mozilla Developer's Network pages.
(https://developer.mozilla.org/en/JavaScript/Reference/Operators/Operator_Precedence)

I have not used the precise precedence numbers from that page since in this module
a lower precedence means the operator binds more tightly (as opposed to the page where
a higher precedence does the same). Also, we have need for less precedence values so they
have been normalised to what we are using in JS:TGP

You will also note that we don't even consider the associativity/precedence of
"=", "+=", "-=" etc. In JS:TGP the notion of expression statements is quite different
to that of expressions. It simply isn't legal to write an expression statement like

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