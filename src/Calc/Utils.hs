module Calc.Utils (initOpts, errMessage) where

import Calc.Types

errMessage :: Err
errMessage = Err { during="unspecified situation", reason="unknown" }

initOpts :: Options
initOpts = Options
  { numberMode=Default
  , numberFormat=Normal
  , joke=False
  , imprecise=False
  , help=False
  , version=False
  , convert=False
  , cats=False
  }

-- TODO: move this into a impl Show
-- | Helper method to pretty print an Expression (AST).
prettyprint :: Expr -> String
prettyprint = exprPrint

exprPrint :: Expr -> String
exprPrint (ELit lit)          = literalPrint lit
exprPrint (EBinOp op lhs rhs) = "(" ++ exprPrint lhs ++ ")" ++ binopPrint op ++ "(" ++ exprPrint rhs ++ ")"
exprPrint (EUnaryOp op expr)  = unaryopPrint op ++ "(" ++ exprPrint expr ++ ")"

binopPrint :: BinOp -> String
binopPrint Add = "+"
binopPrint Sub = "-"
binopPrint Mul = "*"
binopPrint Div = "/"
binopPrint Pow = "^"

unaryopPrint :: UnaryOp -> String
unaryopPrint Neg = "-"

literalPrint :: Literal -> String
literalPrint (LInt i) = show i
literalPrint (LFloat f) = show f
