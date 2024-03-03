module Lexer (lex) where

import Prelude hiding (lex)
import Data.Char (isDigit)

import Types

-- | Documentation here :^)
lstAsZero :: String -> (String, String)
lstAsZero s = 
  let
    (digits', rest) = span isDigit s
    digits = if null digits' then "0" else digits'
  in
    (digits, rest)

-- | Documentation here :^)
lex :: String -> Either String [Token]
lex ('+' : cs)           = fmap (TBinOp Add :) (lex cs) -- fmap (\lst -> Add : lst) (lex cs)
lex ('-' : cs)           = fmap (TBinOp Sub :) (lex cs)
lex ('*' : cs)           = fmap (TBinOp Mul :) (lex cs)
lex ('/' : cs)           = fmap (TBinOp Div :) (lex cs)
lex (' ' : cs)           = lex cs
lex ('(' : cs)           = fmap (TParen L :) (lex cs)
lex (')' : cs)           = fmap (TParen R :) (lex cs)
-- this function uses 'guards':
--  `| isDigit c || c == '.'` -> specifies that c is either a digit or a period
lex (c : cs) | isDigit c || c == '.' =
  let 
    (digits, rest) = lstAsZero (c : cs)
  in 
    case rest of
      '.' : rest' ->
        let 
          (fDigits, fRest) = lstAsZero rest'
          fVal = read (digits ++ "." ++ fDigits)
        in
          fmap (TLit (LFloat fVal) :) (lex fRest)
      _ -> fmap (TLit (LInt (read digits)) :) (lex rest)

lex []                   = Right []
lex (c : _)             = Left ("Unrecognised Symbol: '" ++ [c] ++ "'") 
