module Main where

import Data.Either (fromRight)

import Types
import Lexer (lex)
import Parser (parse)

main :: IO ()
main = test

-- Tests


-- | A testcase is a tuple of the following items:
--      * the number of testcase, e.g. "Testcase 1"
--      * the raw string to parse, e.g. "1+1"
--      * the result of lexing the raw string: a list of `Token`s
--      * the result of parsing the tokens: an AST
--      * the result after evaluation, e.g. "2"
type Testcase = (Int, String, [Token], Expr, String)

goodInputs :: [String]
goodInputs = [
  "1",
  "0",
  "-1",
  "1+1",
  "1-1",
  "1/1",
  "1*1",
  "(1)",
  "(-1)",
  "(1)+(1)",
  "(1)-(1)",
  "(1)/(1)",
  "(1)*(1)",
  "(1+1)",
  "(1-1)",
  "(1/1)",
  "(1*1)",
  " 1  ",
  " 0 ",
  " - 1",
  " 1  +1",
  " 1-  1",
  "  1 / 1",
  " 1 *1",
  " ( 1)   ",
  " (-   1 )",
  " (1  ) + ( 1) ",
  " (1  ) -(1)",
  " (1 ) /(1)",
  " (1 ) * (1)",
  " (1+  1)",
  " (1-1   )",
  " (1/1               )",
  " (1*1)  ",
  -- Tom Tests
  "3 + 12",
  "3*12",
  "3 * (12 + 2)",
  "(3 * 12) + 2",
  "3 * 12 + 2",
  "12 + 3 * 6",
  "1 + 2 + 3 + 4 + 5",
  " 1 + 2+ 3 *4+5",
  "(1 + ( ((2) * 7)) + 3 )*(4+5)"
  ]

-- for good inputs
lexed :: [Either String [Token]]
lexed = map Lexer.lex goodInputs

parsed :: [Either String Expr]
parsed = map (fmap Parser.parse) lexed

evaluated :: [String]
evaluated = map show {-evaluate-} parsed

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"


test :: IO ()
test = mapM_ print parsed

