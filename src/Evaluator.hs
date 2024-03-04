module Evaluator (eval) where

import Numeric (showFFloat)

import Types

{--- public methods ---}

-- | The sole method we _really_ care about: eval.
--   It evaluates an expression, and returns it as string.
eval :: Expr -> String
eval e = showResult $ compute e

{--- private methods ---}

-- | Haskell has no union types.
--   We define a "Result" datatype, that has two fields:
--     I Int   -> construct a result as I from an Int
--     F Float -> construct a result as F from a Float
data Result 
  = I Int 
  | F Float

-- | We define how to print the Result datatype, which really is just `show`.
showResult :: Result -> String
showResult (I i) = show i
showResult (F f) = showFFloat Nothing f ""

-- | given an expression, compute the result
compute :: Expr -> Result
compute (ELit (LFloat f))    = F f
compute (ELit (LInt i))      = I i
compute (EUnaryOp Neg expr)  = negate' (compute expr)
compute (EBinOp Add lhs rhs) = add' (compute lhs) (compute rhs)
compute (EBinOp Sub lhs rhs) = sub' (compute lhs) (compute rhs)
compute (EBinOp Mul lhs rhs) = mul' (compute lhs) (compute rhs)
compute (EBinOp Div lhs rhs) = div' (compute lhs) (compute rhs)

{--- helpers ---}

-- | negates a result
--   does what you'd expect
negate' :: Result -> Result
negate' (I i)  = I  (negate i)
negate' (F f) = F (negate f)

-- | adds two results together
--   does what you'd expect
add' :: Result -> Result -> Result
(I i) `add'` (I i') = I (i              + i'            )
(I i) `add'` (F f)  = F (fromIntegral i + f             )
(F f) `add'` (I i)  = F (f              + fromIntegral i)
(F f) `add'` (F f') = F (f              + f'            )

-- | subtract a result from a different result
--   does what you'd expect
sub' :: Result -> Result -> Result
(I i) `sub'` (I i') = I (i              - i'            )
(I i) `sub'` (F f)  = F (fromIntegral i - f             )
(F f) `sub'` (I i)  = F (f              - fromIntegral i)
(F f) `sub'` (F f') = F (f              - f'            )

-- | multiplies two results together
--   does what you'd expect
mul' :: Result -> Result -> Result
(I i) `mul'` (I i') = I (i              * i'            )
(I i) `mul'` (F f)  = F (fromIntegral i * f             )
(F f) `mul'` (I i)  = F (f              * fromIntegral i)
(F f) `mul'` (F f') = F (f              * f'            )

-- | divides a result by a different result
--   there is no integer division
--   will always result in a floaty value
div' :: Result -> Result -> Result
(I i) `div'` (I i') = F (fromIntegral i / fromIntegral i')
(I i) `div'` (F f)  = F (fromIntegral i / f              )
(F f) `div'` (I i)  = F (f              / fromIntegral i )
(F f) `div'` (F f') = F (f              / f'             )

