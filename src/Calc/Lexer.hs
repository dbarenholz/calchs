module Calc.Lexer (lex) where

import Prelude hiding (lex)
import Data.Char (isDigit)

import Calc.Types
import Calc.Utils

{--- public methods ---}

-- | The sole method we _really_ care about: lex.
--   It lexes the input string into a list of tokens, or an error message.
lex :: String -> [Token]
lex ('+' : cs)                         = (TBinOp Add :) (lex cs)
lex ('-' : cs)                         = (TBinOp Sub :) (lex cs)
lex ('*' : cs)                         = (TBinOp Mul :) (lex cs)
lex ('/' : cs)                         = (TBinOp Div :) (lex cs)
lex (' ' : cs)                         = lex cs                       -- 'ignore' spaces
lex ('(' : cs)                         = (TParen L :) (lex cs)
lex (')' : cs)                         = (TParen R :) (lex cs)
lex (c   : cs) | isDigit c || c == '.' =
  let 
    (digits, rest) = lstAsZero (c : cs)
  in 
    case rest of
      '.' : rest' -> 
        let
          (fDigits, fRest) = lstAsZero rest'
          fVal             = read (digits ++ "." ++ fDigits) -- manually add the . in a float
        in
           (TLit (LFloat fVal) :) (lex fRest)
      _ -> (TLit (LInt (read digits)) :) (lex rest)

lex []                                = []
lex (c : _)                           = error $ red $ ("Lexer: Unrecognised Symbol: '" ++ [c] ++ "'") 


{--- private methods ---}

-- | When lexing a floaty value, we need to tell Haskell that ".5" is "0.5"
lstAsZero :: String -> (String, String)
lstAsZero s = 
  let
    (digits', rest) = span isDigit s
    digits = if null digits' then "0" else digits'
  in
    (digits, rest)

