module Calc.Evaluator (eval) where

import Numeric (showFFloat)

import Calc.Types

eval :: Expr -> String
eval e = showResult $ compute e

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
compute (EBinOp Pow lhs rhs) = pow' (compute lhs) (compute rhs)

negate' :: Result -> Result
negate' (I i)  = I  (negate i)
negate' (F f) = F (negate f)

add' :: Result -> Result -> Result
(I i) `add'` (I i') = I (i              + i'            )
(I i) `add'` (F f)  = F (fromIntegral i + f             )
(F f) `add'` (I i)  = F (f              + fromIntegral i)
(F f) `add'` (F f') = F (f              + f'            )

sub' :: Result -> Result -> Result
(I i) `sub'` (I i') = I (i              - i'            )
(I i) `sub'` (F f)  = F (fromIntegral i - f             )
(F f) `sub'` (I i)  = F (f              - fromIntegral i)
(F f) `sub'` (F f') = F (f              - f'            )

mul' :: Result -> Result -> Result
(I i) `mul'` (I i') = I (i              * i'            )
(I i) `mul'` (F f)  = F (fromIntegral i * f             )
(F f) `mul'` (I i)  = F (f              * fromIntegral i)
(F f) `mul'` (F f') = F (f              * f'            )

-- Division is always floaty.
div' :: Result -> Result -> Result
(I i) `div'` (I i') = F (fromIntegral i / fromIntegral i')
(I i) `div'` (F f)  = F (fromIntegral i / f              )
(F f) `div'` (I i)  = F (f              / fromIntegral i )
(F f) `div'` (F f') = F (f              / f'             )

-- Haskell has different operators for computing powers.
pow' :: Result -> Result -> Result
(I i) `pow'` (I i') | i' >= 0  = I (i ^ i')
(I i) `pow'` (I i') | i' < 0   = F (fromIntegral i ^^ i')
(I i) `pow'` (I i')            = error $ "why do we get here? i = " ++ show i ++ "; i' = " ++ show i'
(I i) `pow'` (F f)  = F (fromIntegral i ** f             )
(F f) `pow'` (I i)  = F (f              ^^ i             )
(F f) `pow'` (F f') = F (f              *  f'            )
