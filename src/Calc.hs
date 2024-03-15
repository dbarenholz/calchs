module Calc where

import qualified Calc.Lexer     as Lexer     (lex)
import qualified Calc.Parser    as Parser    (parse)
import qualified Calc.Evaluator as Evaluator (eval)

-- | Given an input string, lexes, parses, and evaluates it.
compute :: String -> String
compute = Evaluator.eval . Parser.parse . Lexer.lex
