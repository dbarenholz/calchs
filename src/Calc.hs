module Calc where

import qualified Calc.Lexer     as Lexer     (lex)
import qualified Calc.Parser    as Parser    (parse)
import qualified Calc.Evaluator as Evaluator (eval)

-- | Given an input string, lexes, parses, and evaluates it.
--   Returns either an error message occured during computation, or the result.
compute :: String -> Either String String
compute input =
  Lexer.lex input
    >>= Parser.parse
      >>= (\expr -> Right $ Evaluator.eval expr)

