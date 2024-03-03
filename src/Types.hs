module Types where

data Literal 
  = LInt Int
  | LFloat Float
  deriving (Show)

data BinOp 
  = Add 
  | Sub 
  | Mul 
  | Div
  deriving (Show)

data LR = L | R
  deriving (Show)

data Token 
  = TLit Literal 
  | TBinOp BinOp 
  | TParen LR
  deriving (Show)

data UnaryOp
  = Neg
  deriving (Show)

data Expr
  = ELit Literal
  | EBinOp BinOp Expr Expr
  | EUnaryOp UnaryOp Expr
  deriving (Show)
