module Calc.Lexer (lex) where

import Prelude hiding (lex)
import Data.Char (isDigit)

import Calc.Types

lex :: String -> Either String [Token]
lex ('+' : cs)                         = fmap (TBinOp Add :) (lex cs)
lex ('-' : cs)                         = fmap (TBinOp Sub :) (lex cs)
lex ('*' : cs)                         = fmap (TBinOp Mul :) (lex cs)
lex ('/' : cs)                         = fmap (TBinOp Div :) (lex cs)
lex (' ' : cs)                         = lex cs                       -- 'ignore' spaces
lex ('(' : cs)                         = fmap (TParen L :) (lex cs)
lex (')' : cs)                         = fmap (TParen R :) (lex cs)
lex (c   : cs) | isDigit c || c == '.' =
  let (digits, rest) = lstAsZero (c : cs)
  in  case rest of
    '.' : rest' ->
      let
        (fDigits, fRest) = lstAsZero rest'
        fVal             = read (digits ++ "." ++ fDigits) -- manually add the . in a float
      in
         fmap (TLit (LFloat fVal) :) (lex fRest)
    _ -> fmap (TLit (LInt (read digits)) :) (lex rest)

lex []                                = Right []
lex (c : _)                           = Left $ "Lexer: unrecognised symbol: '" ++ [c] ++ "'"


-- | When lexing a floaty value, we need to tell Haskell that ".5" is "0.5"
lstAsZero :: String -> (String, String)
lstAsZero s =
  let
    (digits', rest) = span isDigit s
    digits = if null digits' then "0" else digits'
  in
    (digits, rest)

