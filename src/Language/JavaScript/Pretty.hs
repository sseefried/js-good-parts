{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.JavaScript.Pretty (
 -- | This module just defines and exports 'Pretty' and 'PrettyPrec' instances
 Pretty(..)
) where

-- System libraries
import Text.PrettyPrint.Leijen
import Text.PrettyPrint.Leijen.PrettyPrec
import Prelude hiding (GT, LT, (<$>))
import Numeric (showFFloat)
import qualified Data.List.NonEmpty as NE

-- friends
import Language.JavaScript.AST

-- FIXME: This will be a little tricky to get right.
-- If the string contains double quotes you need to escape.
-- If the string contains single quotes you need to escape.
instance Pretty JSString where
  pretty s = char '"' <> text (unString s) <> char '"' -- enclosed in double quotes

instance Pretty Name where
  pretty = text . unName

sepWith :: Pretty a => Doc -> [a] -> Doc
sepWith s = encloseSep empty empty s . map pretty

endWith :: Pretty a => Doc -> [a] -> Doc
endWith s xs = sepWith s xs <> s

sepWith' :: Pretty a => Doc -> NE.NonEmpty a -> Doc
sepWith' s = encloseSep empty empty s . map pretty . NE.toList

prettyBlock :: Pretty a => [a] -> Doc
prettyBlock stmts = lbrace <$> indent 2 (endWith (semi <$> empty) stmts) <$> rbrace

------------------------------------------------------------------------
--
-- Associativity
--

data Associativity = LeftToRight | RightToLeft deriving Eq


prettyInfixOpApp :: (PrettyPrec a, PrettyPrec b) => Int ->  OpInfo -> a -> b -> Doc
prettyInfixOpApp prec (OpInfo opPrec assoc nm) a b =
  docParen (prec > opPrec) $ bump LeftToRight a <+> text nm <+> bump RightToLeft b
  where
    bump assoc' d = prettyPrec opPrec' d
      where opPrec' = if assoc == assoc' then opPrec else opPrec + 1

docParen :: Bool -> Doc -> Doc
docParen True  = parens
docParen False = id

data OpInfo = OpInfo Int           -- precedence
                     Associativity -- associativity
                     String        -- name

--
-- Lower precedence means the operator binds more tightly
--
infixOpInfo :: InfixOperator -> OpInfo
infixOpInfo op = case op of
  Mul   -> go 6 "*"
  Div   -> go 6 "/"
  Mod   -> go 6 "%"
  Add   -> go 5 "+"
  Sub   -> go 5 "-"
  GTE   -> go 4 ">="
  LTE   -> go 4 "<="
  GT    -> go 4 ">"
  LT    -> go 4 "<"
  Eq    -> go 3 "==="
  NotEq -> go 3 "!=="
  Or    -> go 1 "||"
  And   -> go 2 "&&"
 where go i s = OpInfo i LeftToRight s

-----------------------------------------------------------------------

instance Pretty Number where
  pretty (Number n) = text (showFFloat Nothing n "")

instance PrettyPrec Number -- default

instance Pretty VarStmt where
  pretty (VarStmt varDecls) = text "var" <+> sepWith' (comma <+> empty) varDecls <> semi
  pretty (ConstStmt constDecls) = text "const" <+> pretty constDecls <> semi

instance PrettyPrec VarStmt -- default

instance Pretty VarDecl where
  pretty (VarDecl nm Nothing)    = pretty nm
  pretty (VarDecl nm (Just exp')) = pretty nm <+> text "=" <+> pretty exp'

instance PrettyPrec VarDecl -- default

instance Pretty Stmt where
  pretty stmt = case stmt of
    (StmtExpr  es)  -> pretty es <> semi
    (StmtDisruptive  ds)  -> pretty ds
    (StmtTry         ts)  -> pretty ts
    (StmtIf          is)  -> pretty is
    (StmtSwitch mbLbl ss) -> pp mbLbl ss
    (StmtWhile  mbLbl ws) -> pp mbLbl ws
    (StmtFor    mbLbl fs) -> pp mbLbl fs
    (StmtDo     mbLbl ds) -> pp mbLbl ds
    where
      pp :: Pretty a => Maybe Name -> a -> Doc
      pp (Just label) doc = pretty label <> colon <+> pretty doc
      pp Nothing      doc = pretty doc

instance PrettyPrec Stmt-- default

instance Pretty DisruptiveStmt where
  pretty stmt = case stmt of
    DSBreak  bs -> pretty bs
    DSReturn rs -> pretty rs
    DSThrow  ts -> pretty ts

instance PrettyPrec DisruptiveStmt -- default

instance Pretty IfStmt where
  pretty (IfStmt cond thenStmts blockOrIf) =
    text "if" <+> parens (pretty cond) <+> prettyBlock thenStmts <+> ppRest
    where
      ppRest = case blockOrIf of
        Nothing               -> empty
        Just (Left elseStmts) -> text "else" <+> prettyBlock elseStmts
        Just (Right ifStmt)   -> pretty ifStmt

instance PrettyPrec IfStmt -- default

instance Pretty SwitchStmt where
  pretty (SwitchStmtSingleCase cond caseClause) =
    text "switch" <+> parens (pretty cond) <+> lbrace <$> pretty caseClause <$> rbrace
  pretty (SwitchStmt cond cds stmts) =
    text "switch" <+> parens (pretty cond) <+> lbrace <$>
      indent 2 (vcat (NE.toList . fmap pretty $ cds) <$>
               (text "default:" <$>
                  indent 2 (endWith semi stmts))) <$>
    rbrace

instance PrettyPrec SwitchStmt -- default

instance Pretty CaseAndDisruptive where
  pretty (CaseAndDisruptive caseClause disruptive) =
    pretty caseClause <$> pretty disruptive

instance PrettyPrec CaseAndDisruptive -- default

instance Pretty CaseClause where
  pretty (CaseClause exp' stmts) =
    text "case" <+> pretty exp' <> colon <+> endWith semi stmts

instance PrettyPrec CaseClause -- default

instance Pretty ForStmt where
  pretty (ForStmtCStyle init_ cond incr stmts) =
    text "for" <+> parens (pretty init_ <> semi <+> pretty cond <> semi <+>
                           pretty incr) <+> prettyBlock stmts
  pretty (ForStmtInStyle nm exp' stmts) =
    text "for" <+> parens (pretty nm <+> text "in" <+> pretty exp') <+> prettyBlock stmts

instance PrettyPrec ForStmt -- default

instance Pretty DoStmt where
  pretty (DoStmt stmts cond) =
    text "do" <+> prettyBlock stmts <+> text "while" <+>
      parens (pretty cond) <> semi

instance PrettyPrec DoStmt -- default

instance Pretty WhileStmt where
  pretty (WhileStmt cond stmts) =
    text "while" <+> parens (pretty cond) <+> prettyBlock stmts

instance PrettyPrec WhileStmt -- default

instance Pretty TryStmt where
  pretty (TryStmt tryStmts varName catchStmts) =
    text "try" <+> prettyBlock tryStmts <+> parens (pretty varName) <+> prettyBlock catchStmts

instance PrettyPrec TryStmt -- default

instance Pretty ThrowStmt where
  pretty (ThrowStmt exp_) =
    text "throw" <+> pretty exp_ <> semi

instance PrettyPrec ThrowStmt -- default

instance Pretty ReturnStmt where
  pretty (ReturnStmt mbExp) = case mbExp of
    Nothing  -> text "return;"
    Just exp_ -> text "return" <+> pretty exp_ <> semi

instance PrettyPrec ReturnStmt -- default

instance Pretty BreakStmt where
  pretty (BreakStmt mbExp) = case mbExp of
    Nothing  -> text "break;"
    Just exp_ -> text "break" <+> pretty exp_ <> semi

instance PrettyPrec BreakStmt -- default

instance Pretty ExprStmt where
  pretty (ESApply lvalues rvalue) =
    sepWith' (space <> text "=" <> space) lvalues <+> pretty rvalue
  pretty (ESDelete exp_ refine) =
    text "delete" <+> pretty exp_ <> pretty refine

instance PrettyPrec ExprStmt -- default

instance Pretty LValue              where
  pretty (LValue nm invsAndRefines) = pretty nm <> (hcat . map ppIR $ invsAndRefines)
    where
      ppIR (invs, refine) = (hcat . map pretty $ invs) <> pretty refine

instance PrettyPrec LValue -- default

instance Pretty RValue              where
  pretty rvalue = case rvalue of
    RVAssign e    -> text "="  <+> pretty e
    RVAddAssign e -> text "+=" <+> pretty e
    RVSubAssign e -> text "-=" <+> pretty e
    RVInvoke invs -> hcat . NE.toList . fmap pretty $ invs

instance PrettyPrec RValue -- default

instance Pretty Expr          where
  pretty = prettyPrec 0

instance PrettyPrec Expr where
  prettyPrec i exp_ = case exp_ of
    ExprLit literal      -> pretty literal
    ExprName nm            -> pretty nm
    ExprPrefix prefixOp e    -> pretty prefixOp <> pretty e
    ExprInfix infixOp e e'   -> prettyInfixOpApp i (infixOpInfo infixOp) e e'
    ExprTernary cond thn els ->
      pretty cond <+> char '?' <+> pretty thn <+> colon <+> pretty els
    ExprInvocation e i'       -> pretty e <> pretty i'
    ExprRefinement e r       -> pretty e <> pretty r
    ExprNew e i'              -> text "new" <+> pretty e <> pretty i'
    ExprDelete e r           -> text "new" <+> pretty e <> pretty r

instance Pretty PrefixOperator      where
  pretty op = case op of
    TypeOf   -> text "typeof" <+> empty
    ToNumber -> char '+'
    Negate   -> char '-'
    Not      -> char '!'

instance PrettyPrec PrefixOperator --default

instance Pretty InfixOperator where
  pretty = prettyPrec 0

instance PrettyPrec InfixOperator where
  prettyPrec = error "we never print an operator by itself"

instance Pretty Invocation where
  pretty (Invocation es) = lparen <> sepWith (comma <+> empty) es <> rparen

instance PrettyPrec Invocation -- default

instance Pretty Refinement where
  pretty (Property nm) = char '.' <> pretty nm
  pretty (Subscript e)   = char '[' <> pretty e <> char ']'

instance PrettyPrec Refinement -- default

instance Pretty Lit where
  pretty lit = case lit of
    LitNumber n   -> pretty n
    LitBool   b   -> if b then text "true" else text "false"
    LitString s   -> pretty s
    LitObject o   -> pretty o
    LitArray  a   -> pretty a
    LitFn f -> pretty f

instance PrettyPrec Lit -- default

instance Pretty ObjectLit       where
  pretty (ObjectLit fields) = lbrace <> sepWith (comma <$> empty) fields <> rbrace

instance PrettyPrec ObjectLit -- default

instance Pretty ObjectField         where
  pretty (ObjectField eitherNameString e) = ppEitherNameString <> colon <+> pretty e
    where ppEitherNameString = case eitherNameString of
                 Left nm -> pretty nm
                 Right s   -> pretty s

instance PrettyPrec ObjectField -- default

instance Pretty ArrayLit where
  pretty (ArrayLit es) = lbracket <> sepWith (comma <+> empty) es <> rbracket

instance PrettyPrec ArrayLit -- default

instance Pretty FnLit where
  pretty (FnLit mbName params body) =
    text "function" `join` (parens . hcat . map pretty $ params) <+> pretty body
      where join = case mbName of
                     Just nm -> (\a b -> a <+> pretty nm <> b)
                     Nothing   -> (<>)

instance PrettyPrec FnLit -- default

instance Pretty FnBody where
  pretty (FnBody varStmts stmts) =
    lbrace <$>
    indent 2 (sepWith (semi <$> empty) (map pretty varStmts ++ map pretty stmts)) <$>
    rbrace

instance PrettyPrec FnBody -- default

instance Pretty Program where
  pretty (Program varStmts stmts) = vcat (map pretty varStmts ++ map pretty stmts)

------------------------

{-
test1 = add (n 1) (add (n 2) (add (add (n 3) (n 4)) (n 5)))

test2  = add (n 1) (mul (n 2) (n 3))
test2' = ((n 1) `add` (n 2)) `mul` (n 3)

test3 :: ExprStmt
test3 = case name "x" of
  Right nm ->
    case name "y" of
      Right nm' -> ESApply ((LValue nm' []) NE.<| pure (LValue nm []))
                     (RVAssign test2')

test4 :: Stmt
test4 = StmtExpr test3

test5 :: Program
test5 = Program [] [test4, test4]

test6 :: FnLit
test6 = FnLit Nothing [] (FnBody [] [test4])

add e e' = ExprInfix Add e e'
mul e e' = ExprInfix Mul e e'
n x = ExprLit (LitNumber (Number x))
-}
