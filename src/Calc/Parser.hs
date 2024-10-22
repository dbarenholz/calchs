module Calc.Parser (parse) where

import Calc.Types
import Calc.Utils (errMessage)

-- TODO: Convert this into a pratt parser with support for n levels precedence.

-- parse -> parseAddSub
parse :: [Token] -> Either String Expr
parse tokens = case parseAddSub tokens of
   Left errMsg       -> Left errMsg
   Right (expr, ts') -> case ts' of
     [] -> Right expr
     _  -> Left $ show errMessage { during="parsing: function 'parse'", reason ="remaining tokens?" }


-- parseBlock -> ( parseAddSub ) | - parseBlock
parseBlock :: [Token] -> Either String (Expr, [Token])
parseBlock (TLit lit : ts) = Right (ELit lit, ts)
parseBlock (TParen L : ts) = case parseAddSub ts of
  Left errMsg   -> Left errMsg
  Right (expr, ts') -> case ts' of
    TParen R : ts'' -> Right (expr, ts'')
    _               -> Left $ show errMessage { during="parsing: function 'parseBlock'", reason = "mismatched parens" }
parseBlock (TBinOp Sub : ts) = case parseBlock ts of
  Left errMsg       -> Left errMsg
  Right (expr, ts') -> Right (EUnaryOp Neg expr, ts')
parseBlock _ = Left $ show errMessage { during="parsing: function 'parseBlock'", reason = "unknown token or EOF" }

parseAddSubs :: Expr -> [Token] -> Either String (Expr, [Token])
parseAddSubs lhs ts = case ts of
  TBinOp Add : ts' -> case parseMultDiv ts' of
    Left errMsg       -> Left errMsg
    Right (rhs, ts'') -> let lhs' = EBinOp Add lhs rhs
                         in  parseAddSubs lhs' ts''
  TBinOp Sub : ts' -> case parseMultDiv ts' of
    Left errMsg       -> Left errMsg
    Right (rhs, ts'') -> let lhs' = EBinOp Sub lhs rhs
                         in  parseAddSubs lhs' ts''
  _ -> Right (lhs, ts)

-- parseAddSub -> parseMultDiv
parseAddSub :: [Token] -> Either String (Expr, [Token])
parseAddSub ts = case parseMultDiv ts of
  Left errMsg      -> Left errMsg
  Right (lhs, ts') -> parseAddSubs lhs ts'

parseMultDivs :: Expr -> [Token] -> Either String (Expr, [Token])
parseMultDivs lhs ts = case ts of
  TBinOp Mul : ts' -> case parsePow ts' of
    Left errMsg       -> Left errMsg
    Right (rhs, ts'') -> let lhs' = EBinOp Mul lhs rhs
                         in  parseMultDivs lhs' ts''
  TBinOp Div : ts' -> case parsePow ts' of
    Left errMsg       -> Left errMsg
    Right (rhs, ts'') -> let lhs' = EBinOp Div lhs rhs
                         in  parseMultDivs lhs' ts''
  _ -> Right (lhs, ts)

-- parseMultDiv -> parsePow
parseMultDiv :: [Token] -> Either String (Expr, [Token])
parseMultDiv ts = case parsePow ts of
  Left errMsg  -> Left errMsg
  Right (lhs, ts') -> parseMultDivs lhs ts'

-- parsePow -> parseBlock
parsePow :: [Token] -> Either String (Expr, [Token])
parsePow ts = case parseBlock ts of
  Left errMsg      -> Left errMsg
  Right (lhs, ts') -> case ts' of
    TBinOp Pow : ts'' -> case parsePow ts'' of
      Left errMsg        -> Left errMsg
      Right (rhs, ts''') -> Right (EBinOp Pow lhs rhs, ts''')
    _                 -> Right (lhs, ts')
