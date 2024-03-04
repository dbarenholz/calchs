module Types where

-- | There are 2 literals we support
data Literal 
  = LInt Int                      -- Integers
  | LFloat Float                  -- Floats
  deriving (Show)

-- | There is 1 unary operation we support
data UnaryOp  
  = Neg                           -- Negation,       i.e  -a
  deriving (Show)

-- | There are 4 binary operations we support
data BinOp 
  = Add                           -- Addition,       i.e. a + b
  | Sub                           -- Subtraction,    i.e. a - b
  | Mul                           -- Multiplication, i.e. a * b
  | Div                           -- Division,       i.e. a / b
  deriving (Show)

-- | There are 2 types of parens we support: L ( and R )
data LR = L | R
  deriving (Show)

-- | A Token in the Lexer can be 3 things
data Token 
  = TLit Literal                  -- A literal
  | TBinOp BinOp                  -- A binary operation
  | TParen LR                     -- A paren
  deriving (Show)

-- | An expression (Expr) in the Parser can be 3 things
data Expr
  = ELit Literal                  -- A literal
  | EBinOp BinOp Expr Expr        -- A binary operation
  | EUnaryOp UnaryOp Expr         -- A unary operation
  deriving (Show)
