module Language.JavaScript.Pretty (
 -- | This module just defines and exports 'Pretty' and 'PrettyPrec' instances
 Pretty(..)
) where

-- System libraries
import Text.PrettyPrint.Leijen
import Text.PrettyPrint.Leijen.PrettyPrec

-- friends
import Language.JavaScript.AST
import Language.JavaScript.NonEmptyList

-- FIXME: This will be a little tricky to get right.
instance Pretty JSString where
  pretty s = char '"' <> text (unJSString s) <> char '"' -- enclosed in double quotes

instance Pretty JSName where
  pretty = text . unJSName

sepWith :: Pretty a => Doc -> [a] -> Doc
sepWith s = encloseSep empty empty s . map pretty

endWith :: Pretty a => Doc -> [a] -> Doc
endWith s xs = sepWith s xs <> s

sepWith' :: Pretty a => Doc -> NonEmptyList a -> Doc
sepWith' s = encloseSep empty empty s . map pretty . toList

endWith' :: Pretty a => Doc -> NonEmptyList a -> Doc
endWith' s xs = sepWith' s xs <> s

prettyBlock :: Pretty a => [a] -> Doc
prettyBlock stmts = lbrace <$> indent 2 (endWith (semi <$> empty) stmts) <$> rbrace

------------------------------------------------------------------------
--
-- Associativity
--

data Associativity = LeftToRight | RightToLeft deriving Eq
type Fixity = (Associativity, Int)

assoc :: Associativity -> Int -> Fixity
assoc ass n = (ass, n)

leftToRight, rightToLeft :: Int -> Fixity

leftToRight = assoc LeftToRight
rightToLeft = assoc RightToLeft


prettyInfixOpApp :: (PrettyPrec a, PrettyPrec b) => Int ->  OpInfo -> a -> b -> Doc
prettyInfixOpApp prec (OpInfo opPrec assoc name) a b =
  docParen (prec > opPrec) $ bump LeftToRight a <+> text name <+> bump RightToLeft b
  where
    bump assoc' d = prettyPrec opPrec' d
      where opPrec' = if assoc == assoc' then opPrec else opPrec + 1

-- LAST: Check that bump works in 'prettyInfixOpApp' and do the right thing for prefix ops.
--
prettyPrefixOpApp :: PrettyPrec a => Int -> OpInfo -> a -> Doc
prettyPrefixOpApp prec (OpInfo opPrec assoc name) a =
  text name <> docParen (prec > opPrec) (prettyPrec prec a)


docParen :: Bool -> Doc -> Doc
docParen True  = parens
docParen False = id

data OpInfo = OpInfo Int           -- recedence
                     Associativity -- associativity
                     String        -- name
--
-- FIXME: What about the associativity of +=, -=, etc. It's not defined in your
-- grammar. How will you handle it? Is it even defined in JS:TGP? Answer this question
-- and then write a note about it.
--

--
-- Lower precedence means the operatorbinds more tightly
--
infixOpInfo :: JSInfixOperator -> OpInfo
infixOpInfo op = case op of
  JSMul   -> go 6 "*"
  JSDiv   -> go 6 "/"
  JSMod   -> go 6 "%"
  JSAdd   -> go 5 "+"
  JSSub   -> go 5 "-"
  JSGTE   -> go 4 ">="
  JSLTE   -> go 4 "<="
  JSGT    -> go 4 ">"
  JSLT    -> go 4 "<"
  JSEq    -> go 3 "==="
  JSNotEq -> go 3 "!=="
  JSOr    -> go 1 "||"
  JSAnd   -> go 2 "&&"
 where go i s = OpInfo i LeftToRight s

-----------------------------------------------------------------------

instance Pretty JSNumber where
  pretty (JSNumber n) = pretty n -- FIXME: Make sure this always produce valid Javascript numbers.

instance PrettyPrec JSNumber -- default

instance Pretty JSVarStatement where
  pretty (JSVarStatement varDecls) = sepWith' (comma <+> empty) varDecls

instance PrettyPrec JSVarStatement -- default

instance Pretty JSVarDecl where
  pretty (JSVarDecl nm Nothing)    = pretty nm
  pretty (JSVarDecl nm (Just exp)) = pretty nm <+> text "=" <+> pretty exp

instance PrettyPrec JSVarDecl -- default

instance Pretty JSStatement where
  pretty stmt = case stmt of
    (JSStatementExpression  es)  -> pretty es <> semi
    (JSStatementDisruptive  ds)  -> pretty ds
    (JSStatementTry         ts)  -> pretty ts
    (JSStatementIf          is)  -> pretty is
    (JSStatementSwitch mbLbl ss) -> pp mbLbl ss
    (JSStatementWhile  mbLbl ws) -> pp mbLbl ws
    (JSStatementFor    mbLbl fs) -> pp mbLbl fs
    (JSStatementDo     mbLbl ds) -> pp mbLbl ds
    where
      pp :: Pretty a => Maybe JSName -> a -> Doc
      pp (Just label) doc = pretty label <> colon <+> pretty doc
      pp Nothing      doc = pretty doc

instance PrettyPrec JSStatement-- default

instance Pretty JSDisruptiveStatement where
  pretty stmt = case stmt of
    JSDSBreak  bs -> pretty bs
    JSDSReturn rs -> pretty rs
    JSDSThrow  ts -> pretty ts

instance PrettyPrec JSDisruptiveStatement -- default

instance Pretty JSIfStatement where
  pretty (JSIfStatement cond thenStmts blockOrIf) =
    text "if" <+> parens (pretty cond) <+> prettyBlock thenStmts <+> ppRest
    where
      ppRest = case blockOrIf of
        Nothing               -> empty
        Just (Left elseStmts) -> text "else" <+> prettyBlock elseStmts
        Just (Right ifStmt)   -> pretty ifStmt

instance PrettyPrec JSIfStatement -- default

instance Pretty JSSwitchStatement where
  pretty (JSSwitchStatementSingleCase cond caseClause) =
    text "switch" <+> parens (pretty cond) <+> lbrace <$> pretty caseClause <$> rbrace
  pretty (JSSwitchStatement cond cds stmts) =
    text "switch" <+> parens (pretty cond) <+> lbrace <$>
      indent 2 (vcat (toList . fmap pretty $ cds) <$>
               (text "default:" <$>
                  indent 2 (endWith semi stmts))) <$>
    rbrace

instance PrettyPrec JSSwitchStatement -- default

instance Pretty JSCaseAndDisruptive where
  pretty (JSCaseAndDisruptive caseClause disruptive) =
    pretty caseClause <$> pretty disruptive

instance PrettyPrec JSCaseAndDisruptive -- default

instance Pretty JSCaseClause where
  pretty (JSCaseClause exp stmts) =
    text "case" <+> pretty exp <> colon <+> endWith semi stmts

instance PrettyPrec JSCaseClause -- default

instance Pretty JSForStatement where
  pretty (JSForStatementCStyle init cond incr stmts) =
    text "for" <+> parens (pretty init <> semi <+> pretty cond <> semi <+>
                           pretty incr) <+> prettyBlock stmts

instance PrettyPrec JSForStatement -- default

instance Pretty JSDoStatement where
  pretty (JSDoStatement stmts cond) =
    text "do" <+> prettyBlock stmts <+> text "while" <+>
      parens (pretty cond) <> semi

instance PrettyPrec JSDoStatement -- default

instance Pretty JSWhileStatement where
  pretty (JSWhileStatement cond stmts) =
    text "while" <+> parens (pretty cond) <+> prettyBlock stmts

instance PrettyPrec JSWhileStatement -- default

instance Pretty JSTryStatement where
  pretty (JSTryStatement tryStmts varName catchStmts) =
    text "try" <+> prettyBlock tryStmts <+> parens (pretty varName) <+> prettyBlock catchStmts

instance PrettyPrec JSTryStatement -- default

instance Pretty JSThrowStatement where
  pretty (JSThrowStatement exp) =
    text "throw" <+> pretty exp <> semi

instance PrettyPrec JSThrowStatement -- default

instance Pretty JSReturnStatement where
  pretty (JSReturnStatement mbExp) = case mbExp of
    Nothing  -> text "return;"
    Just exp -> text "return" <+> pretty exp <> semi

instance PrettyPrec JSReturnStatement -- default

instance Pretty JSBreakStatement      where
  pretty (JSBreakStatement mbExp) = case mbExp of
    Nothing  -> text "break;"
    Just exp -> text "break" <+> pretty exp <> semi

instance PrettyPrec JSBreakStatement -- default

instance Pretty JSExpressionStatement where
  pretty (JSESApply lvalues rvalue) =
    sepWith' (space <> text "=" <> space) lvalues <+> pretty rvalue
  pretty (JSESDelete exp refine) =
    text "delete" <+> pretty exp <> pretty refine

instance PrettyPrec JSExpressionStatement -- default

instance Pretty JSLValue              where
  pretty (JSLValue name invsAndRefines) = pretty name <> (hcat . map ppIR $ invsAndRefines)
    where
      ppIR (invs, refine) = (hcat . map pretty $ invs) <> pretty refine

instance PrettyPrec JSLValue -- default

instance Pretty JSRValue              where
  pretty rvalue = case rvalue of
    JSRVAssign e    -> text "="  <+> pretty e
    JSRVAddAssign e -> text "+=" <+> pretty e
    JSRVSubAssign e -> text "-=" <+> pretty e
    JSRVInvoke invs -> hcat . toList . fmap pretty $ invs

instance PrettyPrec JSRValue -- default

instance Pretty JSExpression          where
  pretty = prettyPrec 0

instance PrettyPrec JSExpression where
  prettyPrec i exp = case exp of
    JSExpressionLiteral literal      -> pretty literal
    JSExpressionName name            -> pretty name
    JSExpressionPrefix prefixOp e    -> pretty prefixOp <> pretty e
    JSExpressionInfix infixOp e e'   -> prettyInfixOpApp i (infixOpInfo infixOp) e e'
    JSExpressionTernary cond thn els ->
      pretty cond <+> char '?' <+> pretty thn <+> colon <+> pretty els
    JSExpressionInvocation e i       -> pretty e <> pretty i
    JSExpressionRefinement e r       -> pretty e <> pretty r
    JSExpressionNew e i              -> text "new" <+> pretty e <> pretty i
    JSExpressionDelete e r           -> text "new" <+> pretty e <> pretty r

instance Pretty JSPrefixOperator      where
  pretty op = case op of
    JSTypeOf   -> text "typeof" <+> empty
    JSToNumber -> char '+'
    JSNegate   -> char '-'
    JSNot      -> char '!'

instance PrettyPrec JSPrefixOperator --default

instance Pretty JSInfixOperator where
  pretty = prettyPrec 0

instance PrettyPrec JSInfixOperator where
  prettyPrec = error "we never print an operator by itself"

instance Pretty JSInvocation          where
  pretty (JSInvocation es) = lparen <> sepWith (comma <+> empty) es <> rparen

instance PrettyPrec JSInvocation -- default

instance Pretty JSRefinement          where
  pretty (JSProperty name) = char '.' <> pretty name
  pretty (JSSubscript e)   = char '[' <> pretty e <> char ']'

instance PrettyPrec JSRefinement -- default

instance Pretty JSLiteral             where
  pretty lit = case lit of
    JSLiteralNumber n   -> pretty n
    JSLiteralString s   -> pretty s
    JSLiteralObject o   -> pretty o
    JSLiteralArray  a   -> pretty a
    JSLiteralFunction f -> pretty f

instance PrettyPrec JSLiteral -- default

instance Pretty JSObjectLiteral       where
  pretty (JSObjectLiteral fields) = lbrace <> sepWith (comma <$> empty) fields <> rbrace

instance PrettyPrec JSObjectLiteral -- default

instance Pretty JSObjectField         where
  pretty (JSObjectField eitherNameString e) = ppEitherNameString <> colon <+> pretty e
    where ppEitherNameString = case eitherNameString of
                 Left name -> pretty name
                 Right s   -> pretty s

instance PrettyPrec JSObjectField -- default

instance Pretty JSArrayLiteral        where
  pretty (JSArrayLiteral es) = lbracket <> sepWith (comma <+> empty) es <> rbracket

instance PrettyPrec JSArrayLiteral -- default

instance Pretty JSFunctionLiteral     where
  pretty (JSFunctionLiteral mbName params body) =
    text "function" `join` (parens . hcat . map pretty $ params) <+> pretty body
      where join = case mbName of
                     Just name -> (\a b -> a <+> pretty name <> b)
                     Nothing   -> (<>)

instance PrettyPrec JSFunctionLiteral -- default

instance Pretty JSFunctionBody        where
  pretty (JSFunctionBody varStmts stmts) =
    lbrace <$>
    indent 2 (sepWith (semi <$> empty) (map pretty varStmts ++ map pretty stmts)) <$>
    rbrace

instance PrettyPrec JSFunctionBody -- default

instance Pretty JSProgram where
  pretty (JSProgram varStmts stmts) = vcat (map pretty varStmts ++ map pretty stmts)

------------------------


test1 = add (n 1) (add (n 2) (add (add (n 3) (n 4)) (n 5)))

test2  = add (n 1) (mul (n 2) (n 3))
test2' = ((n 1) `add` (n 2)) `mul` (n 3)

test3 :: JSExpressionStatement
test3 = case jsName "x" of
  Right nm ->
    case jsName "y" of
      Right nm' -> JSESApply ((JSLValue nm' []) <:> singleton (JSLValue nm []))
                     (JSRVAssign test2')


test4 :: JSStatement
test4 = JSStatementExpression test3

-- test4a = JSStatement

test5 :: JSProgram
test5 = JSProgram [] [test4, test4]

test6 :: JSFunctionLiteral
test6 = JSFunctionLiteral Nothing [] (JSFunctionBody [] [test4])

add e e' = JSExpressionInfix JSAdd e e'
mul e e' = JSExpressionInfix JSMul e e'
n x = JSExpressionLiteral (JSLiteralNumber (JSNumber x))