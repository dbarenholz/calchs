module Parser where

import Types

parse :: [Token] -> Expr 
parse tokens = 
  let 
    (expr, ts') = parseAddSub tokens
  in
    case ts' of
      [] -> expr
      _ -> error $ "parse error: remaining tokens? ->\ntokens: " ++ show ts' ++ "\nexpr: " ++ show expr

parseBlock :: [Token] -> (Expr, [Token])   -- parses ()
parseBlock ((TParen L) : ts) = 
  let
    (expr, ts') = parseAddSub ts
  in
    case ts' of
      TParen R : ts'' -> (expr, ts'') 
      _ -> error "parse error: mismatched parenthesis"
parseBlock ((TLit lit) : ts) = (ELit lit, ts)
parseBlock ((TBinOp Sub) : ts) = 
  let 
    (expr, ts') = parseBlock ts
  in
    (EUnaryOp Neg expr , ts')
parseBlock _ = error "parse error: unknown symbol in parseBlock (EOF?)"

-- 1 - 2*2 - 3*3 - 4 - 5
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
