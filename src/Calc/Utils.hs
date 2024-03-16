module Calc.Utils (red, green, prettyprint) where

import Calc.Types

-- | Helper method to print a particular string as red text to the terminal.
red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

-- | Helper method to print a particular string as green text to the terminal.
green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

-- | Helper method to pretty print an Expression (AST).
prettyprint :: Expr -> String
prettyprint = exprPrint

exprPrint :: Expr -> String
exprPrint (ELit lit)          = literalPrint lit
exprPrint (EBinOp op lhs rhs) = "(" ++ (exprPrint lhs) ++ ")" ++ (binopPrint op) ++ "(" ++ (exprPrint rhs) ++ ")"
exprPrint (EUnaryOp op expr)  = (unaryopPrint op) ++ "(" ++ (exprPrint expr) ++ ")"

binopPrint :: BinOp -> String
binopPrint (Add) = "+"
binopPrint (Sub) = "-"
binopPrint (Mul) = "*"
binopPrint (Div) = "/"

unaryopPrint :: UnaryOp -> String
unaryopPrint (Neg) = "-"

literalPrint :: Literal -> String
literalPrint (LInt i) = show i
literalPrint (LFloat f) = show f
