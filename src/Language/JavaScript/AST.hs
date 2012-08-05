{-# OPTIONS_GHC -Wall #-}
--
-- Module: Language.Javascript.AST
-- Author: Sean Seefried
--
-- © 2012
--
-- | In Chapter 2 of \"JavaScript: The Good Parts\", Douglas Crockford presents a
-- concrete grammar for \"the good parts\" of JavaScript.
--
-- This module provides an abstract grammar for those good parts. Henceforth, we abbreviate this
-- language to JS:TGP
--
-- Crockford presents the grammar as a series of railroad diagrams.
-- The correspondence between the concrete grammar and the abstract grammar
-- in this module is NOT one-to-one. However, the following property does hold: the
-- pretty printing of an abstract syntax tree will be parseable by the concrete grammar. i.e.
-- For each valid program produced by the concrete grammar there is a corresponding
-- abstract syntax tree that when pretty printed will produce that program (modulo whitespace).
--
-- /The abstract grammar/
--
--   * removes unnecessary characters such as parentheses (normal, curly and square)
--
--   * represents JavaScript's string, name and number literals directly in Haskell as
--     'String', 'String' and 'Double' respectively.
--
-- /Conventions for concrete syntax/
--
--  *  Non-terminals appear in angle brackets e.g. \<Name\>
--
--  *  ? means zero or one. e.g. \<Expr\>?
--
--  *  * means zero or more e.g. \<Stmt\>*
--
--  *  + means one  or more e.g. \<Stmt\>+
--
--  *  \( \) are meta-brackets used to enclose a concrete-syntax expression so that ?,* or +
--     can be applied. e.g. \(= \<Expr\>\)*
--     This means zero or more repetitions of: = \<Expr\>
--
-- This library was designed so that it would be impossible, save for name, string literals
-- to construct an incorrect JS:TGP program. To this end some of the data structures may look like
-- they contain redundancy. For instance, consider the 'ESDelete' constructor which is defined
--
-- @ESDelete Expr Invocation@
--
-- Why not just define it as @ESDelete Expr@ since type @Expr@
-- has a constructor defined as @ExprInvocation Expr Invocation@?
-- The reason is that this would allow incorrect programs. A 'Expr' is
-- not necessarily a 'Invocation'.
--
-- /A note on precedence of JavaScript operators/
--
-- Interestingly, the precedence of JavaScript operators is
-- not defined in the ECMAScript standard. The precedence used in this library comes from
-- the Mozilla Developer's Network pages.
-- (https://developer.mozilla.org/en/JavaScript/Reference/Operators/Operator_Precedence)
--
-- I have not used the precise precedence numbers from that page since in this module
-- a lower precedence means the operator binds more tightly (as opposed to the page where
-- a higher precedence does the same). Also, we have need for less precedence values so they
-- have been normalised to what we are using in JS:TGP
--
-- You will also note that we don't even consider the associativity/precedence of
-- \"=\", \"+=\", \"-=\" etc. In JS:TGP the notion of expression statements is quite different
-- to that of expressions. It simply isn't legal to write an expression statement like
--
--  @(a += 2) -= 3@
--
--    or
--
--  @a = (b = c) = (c = d)@
--
-- although it is perfectly legal to write
--
--  @a = b = c = d += 2@
--
-- which if we add brackets to disambiguate is really
--
-- @a = (b = (c = (d += 2)))@
--
--
-- Interesting aspects of \"the good parts\":
--
--
-- A JS:TGP program is a collection of statements. You'll note that there is no
-- statement to declare a function in JS:TGP. However you can assign a function literal
-- to a variable.
--
-- e.g.
--
-- @var fun = function(x) { return x + 1;}@
--
--
-- What about recursive functions then? There is the option to give the function a name which is
-- local to the literal.
--
-- e.g.
--
-- @var factorial = function f(n) {
--                    if ( n > 0 ) {
--                      return n * f(n - 1);
--                    } else {
--                      return 1;
--                    }
--                  }@
--
-- @f@ is local. It will not be in scope outside of the function body.
--
-- Abbreviations:
--
-- > Stmt = Statement, Expr = Expression, Fn = Function, Decl = Declaration
--
module Language.JavaScript.AST (
  -- JSString, Name can't be create except with constructors
  JSString, Name, 
  unString, unName,
  jsString, name,
   
  -- * Data types
  Number(..),
  VarStmt(..), VarDecl(..), Stmt(..),
  DisruptiveStmt(..), IfStmt(..), SwitchStmt(..),
  CaseAndDisruptive(..), CaseClause(..), ForStmt(..),
  DoStmt(..), WhileStmt(..), TryStmt(..),
  ThrowStmt(..), ReturnStmt(..), BreakStmt(..),
  ExprStmt(..), LValue(..), RValue(..), Expr(..),
  PrefixOperator(..), InfixOperator(..), Invocation(..), Refinement(..),
  Lit(..), ObjectLit(..), ObjectField(..), ArrayLit(..),
  FnLit(..), FnBody(..), Program(..)
) where

import Language.JavaScript.NonEmptyList


data Name = Name { unName :: String }

--
-- | 'jsName' is the only way you can create a Name
--
name :: String -> Either String Name
name = Right . Name -- FIXME: Return Left on error.

data JSString = JSString { unString :: String }

--
-- | The only way you can create a Javascript string.
--   This function needs to correctly encode all special characters.
--   See p9 of \"JavaScript: The Good Parts\"
--
jsString :: String -> Either String JSString 
jsString = Right . JSString -- FIXME: Return Left on error


newtype Number = Number Double -- 64 bit floating point number

--
-- | Concrete syntax:
--
-- @var \<VarDecl\> [, \<VarDecl\>]* ;@
--
-- e.g. @var x = 1, y;@
--
data VarStmt = VarStmt (NonEmptyList VarDecl)

--
-- | Concrete syntax:
--
--   1. @\<Name\> \(= \<Expr\>\)?@
--
-- e.g.
--
-- 1. @x@
--
-- 2. @x = 2 + y@
--
data VarDecl = VarDecl Name (Maybe Expr) -- optional initialization

--
-- | The many different kinds of statements
--
data Stmt
  = StmtExpr   ExprStmt -- ^ @\<ExprStmt\>;@
  | StmtDisruptive  DisruptiveStmt -- ^ @\<DisruptiveStmt\>@
  | StmtTry         TryStmt        -- ^ @\<TryStmt\>@
  | StmtIf          IfStmt         -- ^ @\<IfStmt\>@

  -- | @\(\<Name\> : \) \<SwitchStmt\>@
  | StmtSwitch      (Maybe Name) SwitchStmt
  -- | @\(\<Name\> : \) \<WhileStmt\>@
  | StmtWhile       (Maybe Name) WhileStmt
  -- | @\(\<Name\> : \) \<ForStmt\>@
  | StmtFor         (Maybe Name) ForStmt
  -- | @\(\<Name\> : \) \<DoStmt\>@
  | StmtDo          (Maybe Name) DoStmt

--
-- | Disruptive statements
--
data DisruptiveStmt
  = DSBreak   BreakStmt  -- ^ @\<BreakStmt\>@
  | DSReturn  ReturnStmt -- ^ @syntax: \<ReturnStmt\>@
  | DSThrow   ThrowStmt  -- ^ @syntax: \<ThrowStmt\>@

--
-- | Concrete syntax:
--
-- @if ( \<Expr\> ) { \<Stmt\>* }@                        -- for 'Nothing'
--
-- or
--
-- @if ( \<Expr\> ) { \<Stmt\>* } else { \<Stmt\>* }@ -- for 'Just . Left'
--
-- or
--
-- @if ( \<Expr\> ) { \<Stmt\>* } else \<IfStmt\>@    -- for 'Just . Right'
--
--   e.g.
--
-- 1. @if (x > 3) { y = 2; }@
--
-- 2. @if (x < 2) { y = 1; } else { y = 3; z = 2; }@
--
-- 3. @if (x > 0) { y = 20; } else if ( x > 10) { y = 30; } else { y = 10; }@
--
data IfStmt = IfStmt Expr
                                   [Stmt]
                                   (Maybe (Either [Stmt] IfStmt))


--
-- | Concrete syntax:
--
-- @switch ( \<Expr\> ) { \<CaseClause\> }@
--
-- or
--
-- @
-- switch ( \<Expr\> ) {
--  \<CaseAndDisruptive\>+
--  default : \<Stmt\>*
-- }
-- @
--
-- e.g.
--
-- 1.
--
-- @
-- switch ( x ) {
--   case 1:
--     y = 2;
--   }
-- @
--
-- 2.
--
-- @
-- switch ( x ) {
--   case 1:
--     y = 2;
--     break;
--   case 2:
--     y = 3;
--     break;
--   default:
--     y = 4;
-- }
-- @
--
data SwitchStmt
  = SwitchStmtSingleCase Expr CaseClause
  | SwitchStmt           Expr
                                (NonEmptyList CaseAndDisruptive)
                                --  ^ non-default case clauses
                                [Stmt]
                                -- ^ default clause statements

--
-- | A case clause followed by a disruptive statement
--
--   Concrete syntax:
--
--  @\<CaseClause\> \<DisruptiveStmt\>@
--
--   e.g.
--
-- 1.
-- @
-- case 2:
--   y = 2;
--   break;
-- @
--
data CaseAndDisruptive = CaseAndDisruptive CaseClause DisruptiveStmt

--
-- | Concrete syntax:
--
-- @case \<Expr\> : \<Stmt\>*@
--
-- e.g.
--
-- 1.
--
-- @case 2:   \/\/ zero statements following the case expression is valid.@
--
-- 2.
--
-- @
-- case 2:
--   y = 1;
-- @
--
data CaseClause = CaseClause Expr [Stmt]

--
-- | Two style of for-statements -- C-style and In-style.
--
--   Concrete syntax:
--
-- 1.
--
-- @
-- for (\<ExprStmt\>? ; \<Expr\>? ; \<ExprStmt\>? ) {
--   \<Stmt\>*
-- }
-- @
--
-- 2.
--
-- @
-- for ( \<Name\> in \<Expr\> ) {
--   \<Stmt\>*
-- }
-- @
--
--   e.g.
--
-- 1. @for ( ; ; ) { }@
--
-- 2. @for ( ; x < 10 ;) { x += 1; }@
--
-- 3.
--
-- @
-- for (i = 0; i < 10; i += 1) {
--   x += i;
-- }
-- @
--
-- 4. @for ( i in indices ) { a[i] = 66; }@
--
data ForStmt
  = ForStmtCStyle
       (Maybe ExprStmt)
       (Maybe Expr)
       (Maybe ExprStmt)
       [Stmt]
    | ForStmtInStyle
       Name
       Expr
       [Stmt]

--
-- | Concrete syntax:
--
-- @do { \<Stmt\>* } while ( \<Expr\> );@
--
data DoStmt = DoStmt [Stmt] Expr

--
-- | Concrete syntax:
--
-- @while ( \<Expr\>) { \<Stmt\>* }@
--
data WhileStmt = WhileStmt Expr [Stmt]

--
-- | Concrete syntax:
--
-- @try { \<Stmt\>* } catch ( \<Name\> ) { \<Stmt\>* }@
--
data TryStmt = TryStmt [Stmt] Name [Stmt]

--
-- | Concrete syntax:
--
-- @throw \<Expr\>;@
--
data ThrowStmt = ThrowStmt Expr

--
-- | Concrete syntax:
--
-- @return \<Expr\>?;@
--
--   e.g.
--
--   1. @return;@
--
--   2. @return 2 + x;@
--
data ReturnStmt = ReturnStmt (Maybe Expr)

--
-- | Concrete syntax:
--
-- @break \<Name\>?;@
--
-- e.g.
--
-- 1. @break;@
--
-- 2. @break some_label;@
--
data BreakStmt = BreakStmt (Maybe Name)

--
-- | Concrete syntax:
--
-- @\<Value\>+ \<RValue\>@
--
-- or
--
-- @delete \<Expr\> \<Refinement\>@
--
data ExprStmt
  = ESApply (NonEmptyList LValue) RValue
  | ESDelete Expr Refinement

--
-- | Concrete syntax:
--
-- @\<Name\> \(\<Invocation\>* \<Refinement\>\)*@
--
--   e.g.
--
--   1. @x@
--
--   2. @x.field_1@
--
--   3. @fun().field_1@
--
--   4. @fun(1)(2)@
--
--   5. @fun(1)(2).field_1@
--
--   6. @x.fun_field_1(x+2).fun_field_2(y+3).field_3@
--
data LValue = LValue Name [([Invocation], Refinement)]

--
-- | Concrete syntax:
--
-- @=  \<Expr\>@
--
-- or
--
-- @+= \<Expr\>@
--
-- or
--
-- @-= \<Expr\>@
--
-- or
--
-- @\<Invocation\>+@
--
-- e.g.
--
-- 1. @= 2@
--
-- 2. @+= 3@
--
-- 3. @-= (4 + y)@
--
-- 4. @()@
--
-- 5. @(1)@
--
-- 6. @(x,y,z)@
--
data RValue
  = RVAssign    Expr
  | RVAddAssign Expr
  | RVSubAssign Expr
  | RVInvoke    (NonEmptyList Invocation)

data Expr 
  = ExprLit    Lit -- ^ @\<Lit\>@
  | ExprName       Name    -- ^ @\<Name\>@

  -- | @\<PrefixOperator> \<Expr\>@
  | ExprPrefix     PrefixOperator Expr

  -- | @\<Expr\> \<InfixOperator\> \<Expr\>@
  | ExprInfix      InfixOperator  Expr Expr

  -- | @\<Expr\> ? \<Expr\> : \<Expr\>@
  | ExprTernary    Expr     Expr Expr

  -- | @\<Expr\>\<Invocation\>@
  | ExprInvocation Expr     Invocation

  -- | @\<Expr\>\<Refinement\>@
  | ExprRefinement Expr     Refinement

  -- | new @\<Expr\>\<Invocation\>@
  | ExprNew        Expr     Invocation

  -- | delete @\<Expr\>\<Refinement\>@
  | ExprDelete     Expr     Refinement

data PrefixOperator
  = TypeOf   -- ^ @typeof@
  | ToNumber -- ^ @+@
  | Negate   -- ^ @-@
  | Not      -- ^ @!@

data InfixOperator
  = Mul  -- ^ @*@
  | Div  -- ^ @/@
  | Mod  -- ^ @%@
  | Add  -- ^ @+@
  | Sub  -- ^ @-@
  | GTE  -- ^ @>=@
  | LTE  -- ^ @<=@
  | GT   -- ^ @>@
  | LT   -- ^ @<@
  | Eq   -- ^ @===@
  | NotEq-- ^ @!==@
  | Or   -- ^ @||@
  | And  -- ^ @&&@

--
-- | Concrete syntax:
--
-- @\<Expr\>*@
--
--   e.g.
--
-- 1. @()@
--
-- 2. @(1)@
--
-- 3. @(x,z,y)@
--
data Invocation = Invocation [Expr]

-- | Concrete syntax:
--
-- @.\<Name\>@
--
-- or
--
-- @[\<Expr\>]@
--
-- e.g.
--
-- 1. @.field_1@
--
-- 2. @[i+1]@
--
data Refinement
  = Property  Name
  | Subscript Expr

--
-- | Interestingly, the syntax diagrams presented in the book don't include
--   boolean literals. I can only assume this is an oversight as they
--   are used throughout the book.
--


data Lit
  = LitNumber   Number      -- ^ @\<Number\>@
  | LitBool     Bool        -- ^ @\<true | false\>@
  | LitString   JSString    -- ^ @\<String\>@
  | LitObject   ObjectLit   -- ^ @\<ObjectLit\>@
  | LitArray    ArrayLit    -- ^ @\<ArrayLit\>@
  | LitFn FnLit -- ^ @\<FnLit\>@
--  | LitRegexp   RegexpLit -- TODO: Add regexps

--
-- | Concrete syntax:
--
-- @{}@                                               -- no  fields
--
-- or
--
-- @\{\<ObjectField\> \(, \<ObjectField\> \)*\}@    -- one or more fields
--
data ObjectLit = ObjectLit [ObjectField]

--
-- | Concrete syntax:
--
-- @\<Name\>: \<Expr\>        @ -- for Left
--
-- or
--
-- @\<String\>: \<Expr\>      @ -- for Right
--
--   e.g.
--
--   1. @x: y + 3@
--
--   2. @\"value\": 3 - z@
--
data ObjectField  = ObjectField (Either Name String) Expr

--
-- | Concrete syntax:
--
-- @[]@ -- empty array
--
-- or
--
-- @[\<Expr\> \(, \<Expr\>*\) ]@ -- non empty array
--
data ArrayLit = ArrayLit [Expr]

--
-- | Concrete syntax:
--
-- @function \<Name\>? \<FnBody\>@
--
data FnLit = FnLit (Maybe Name) [Name] FnBody

--
-- | Concrete syntax:
--
-- @{ \<VarStmt\>+ \<Stmt\>+ }@
--
data FnBody = FnBody [VarStmt] [Stmt]

-- | Programs. All variable statements come first.
data Program = Program [VarStmt] [Stmt]
