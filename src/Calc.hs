module Calc where

import qualified Calc.Lexer     as Lexer     (lex)
import qualified Calc.Parser    as Parser    (parse)
import qualified Calc.Evaluator as Evaluator (eval)
import Calc.Types (Options(..))

-- | Given an input string, lexes, parses, and evaluates it.
--   Returns either an error message occured during computation, or the result.
compute :: Options -> String -> Either String String
compute opts input =
  Lexer.lex input
    >>= Parser.parse
      >>= Right . Evaluator.eval opts
