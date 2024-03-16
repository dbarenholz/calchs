module Calc.Parser (parse) where

import Calc.Types

-- parse -> parseAddSub
parse :: [Token] -> Either String Expr
parse tokens = case parseAddSub tokens of
   Left errMessage   -> Left errMessage
   Right (expr, ts') -> case ts' of
     [] -> Right expr
     _  -> Left $ "parse error: remaining tokens? ->\ntokens: " ++ show ts' ++ "\nexpr: " ++ show expr


-- parseBlock -> ( parseAddSub ) | - parseBlock
parseBlock :: [Token] -> Either String (Expr, [Token])
parseBlock ((TLit lit) : ts) = Right (ELit lit, ts)
parseBlock ((TParen L) : ts) = case parseAddSub ts of
  Left errMessage   -> Left errMessage
  Right (expr, ts') -> case ts' of
    TParen R : ts'' -> Right (expr, ts'')
    _               -> Left $  "parse error: mismatched parenthesis?"
parseBlock ((TBinOp Sub) : ts) = case parseBlock ts of
  Left errMessage   -> Left errMessage
  Right (expr, ts') -> Right (EUnaryOp Neg expr, ts')
parseBlock _ = Left $ "parse error: unknown symbol in parseBlock (or EOF)"

parseAddSubs :: Expr -> [Token] -> Either String (Expr, [Token])
parseAddSubs lhs ts = case ts of
  TBinOp Add : ts' -> case parseMultDiv ts' of
    Left errMessage -> Left errMessage
    Right (rhs, ts'') -> let lhs' = EBinOp Add lhs rhs
                         in  parseAddSubs lhs' ts''
  TBinOp Sub : ts' -> case parseMultDiv ts' of
    Left errMessage -> Left errMessage
    Right (rhs, ts'') -> let lhs' = EBinOp Sub lhs rhs
                         in  parseAddSubs lhs' ts''
  _ -> Right (lhs, ts)

-- parseAddSub -> parseMultDiv
parseAddSub :: [Token] -> Either String (Expr, [Token])
parseAddSub ts = case parseMultDiv ts of
  Left errMessage  -> Left errMessage
  Right (lhs, ts') -> parseAddSubs lhs ts'

parseMultDivs :: Expr -> [Token] -> Either String (Expr, [Token])
parseMultDivs lhs ts = case ts of
  TBinOp Mul : ts' -> case parseBlock ts' of
    Left errMessage -> Left errMessage
    Right (rhs, ts'') -> let lhs' = EBinOp Mul lhs rhs
                         in  parseMultDivs lhs' ts''
  TBinOp Div : ts' -> case parseBlock ts' of
    Left errMessage -> Left errMessage
    Right (rhs, ts'') -> let lhs' = EBinOp Div lhs rhs
                          in parseMultDivs lhs' ts''
  _ -> Right (lhs, ts)

-- parseMultDiv -> parseBlock
parseMultDiv :: [Token] -> Either String (Expr, [Token])
parseMultDiv ts = case parseBlock ts of
  Left errMessage  -> Left errMessage
  Right (lhs, ts') -> parseMultDivs lhs ts'
