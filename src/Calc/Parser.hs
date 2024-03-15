module Calc.Parser(parse) where

import Calc.Types
import Calc.Utils

{--- public methods ---}

-- | The sole method we _really_ care about: parse.
--   Given a list of tokens, parse it to an expression to evaluate.
parse :: [Token] -> Expr 
parse tokens = 
  let 
    (expr, ts') = parseAddSub tokens
  in
    case ts' of
      [] -> expr
      _  -> error $ red $ "parse error: remaining tokens? ->\ntokens: " ++ show ts' ++ "\nexpr: " ++ show expr


{--- private methods ---}

parseBlock :: [Token] -> (Expr, [Token])   -- parses (Expr)
parseBlock ((TLit lit) : ts) = (ELit lit, ts)
parseBlock ((TParen L) : ts) = 
  let
    (expr, ts') = parseAddSub ts
  in
    case ts' of
      TParen R : ts'' -> (expr, ts'')
      _ -> error $ red $ "parse error: mismatched parenthesis?"
parseBlock ((TBinOp Sub) : ts) = 
  let 
    (expr, ts') = parseBlock ts
  in
    (EUnaryOp Neg expr , ts')
parseBlock _ = error $ red $ "parse error: unknown symbol in parseBlock (or EOF)"

parseAddSubs :: Expr -> [Token] -> (Expr, [Token])
parseAddSubs lhs ts =
  case ts of
    TBinOp Add : ts' -> 
      let
        (rhs, ts'') = parseMultDiv ts'
        lhs' = EBinOp Add lhs rhs
      in
        parseAddSubs lhs' ts'' 
    TBinOp Sub : ts' ->
      let
        (rhs, ts'') = parseMultDiv ts'
        lhs' = EBinOp Sub lhs rhs
      in
        parseAddSubs lhs' ts'' 
    _ -> (lhs, ts)

parseAddSub :: [Token] -> (Expr, [Token]) -- parses */ expressions
parseAddSub ts = 
  let
    (lhs, ts') = parseMultDiv ts
  in
    parseAddSubs lhs ts'

parseMultDivs :: Expr -> [Token] -> (Expr, [Token])
parseMultDivs lhs ts =
  case ts of
    TBinOp Mul : ts' -> 
      let
        (rhs, ts'') = parseBlock ts'
        lhs' = EBinOp Mul lhs rhs
      in
        parseMultDivs lhs' ts'' 
    TBinOp Div : ts' ->
      let
        (rhs, ts'') = parseBlock ts'
        lhs' = EBinOp Div lhs rhs
      in
        parseMultDivs lhs' ts'' 
    _ -> (lhs, ts)

parseMultDiv :: [Token] -> (Expr, [Token]) -- parses */ expressions
parseMultDiv ts = 
  let
    (lhs, ts') = parseBlock ts
  in
    parseMultDivs lhs ts'
