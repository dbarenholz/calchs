module Calc.Types where

-- | There are 2 literals we support
data Literal
  = LInt Int                      -- Integers
  | LFloat Float                  -- Floats
  deriving (Show, Eq)

-- | There is 1 unary operation we support
data UnaryOp
  = Neg                           -- Negation,       i.e  -a
  deriving (Show, Eq)

-- | There are 4 binary operations we support
data BinOp
  = Add                           -- Addition,       i.e. a + b
  | Sub                           -- Subtraction,    i.e. a - b
  | Mul                           -- Multiplication, i.e. a * b
  | Div                           -- Division,       i.e. a / b
  | Pow                           -- Power,          i.e. a ^ b
  deriving (Show, Eq)

-- | There are 2 types of parens we support: L ( and R )
data LR = L | R
  deriving (Show, Eq)

-- | A Token in the Lexer can be 3 things
data Token
  = TLit Literal                  -- A literal
  | TBinOp BinOp                  -- A binary operation
  | TParen LR                     -- A paren
  deriving (Show, Eq)

-- | An expression (Expr) in the Parser can be 3 things
data Expr
  = ELit Literal                  -- A literal
  | EBinOp BinOp Expr Expr        -- A binary operation
  | EUnaryOp UnaryOp Expr         -- A unary operation
  deriving (Show, Eq)

-- | A result (from evaluating an expression) can be 2 things
data Result
  = I Int                         -- An integer value
  | F Float                       -- A floaty value

-- | Supported number formats are normal (10000) and scientific (10e4)
data NumberFormat = Normal | Scientific  deriving (Show)

-- | Supported number modes are binary (11111), hexadecimal () and default, which is base 10. You can also specify the base directly.
data NumberMode   = Binary | Hex | Base Int | Default deriving (Show)


-- | These are options that calchs support
data Options = Options
  { help :: Bool
  , version :: Bool
  , joke :: Bool
  , imprecise :: Bool
  , cats :: Bool
  , convert :: Bool
  , numberFormat :: NumberFormat
  , numberMode :: NumberMode
  } deriving (Show)
