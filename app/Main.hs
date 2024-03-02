module Main where

import Data.Either (fromRight)

import Lexer (lex)
import Parser (parse)


main :: IO ()
main = test

-- Testsuite

tests :: [(Int, (String, String))]
tests = zip [1..] [
  ("1+1","2"),
  ("1-1","0"),
  ("1/1","1"),
  ("1*1","1"),
  ("(1)+(1)","2"),
  ("(1)-(1)","0"),
  ("(1)/(1)","1"),
  ("(1)*(1)","1"),
  ("(1+1)","2"),
  ("(1-1)","0"),
  ("(1/1)","1"),
  ("(1*1)","1")
 ]

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

prettyPrintTestcase :: (Int, (String, String)) -> String
prettyPrintTestcase (tc, (inp, expected)) = 
  let
    -- TODO: get error message fromLeft
    actual = fromRight "" (parse inp)
    res    = if actual == expected then green "OK" else red "FAIL"
  in 
    "Testcase " ++ show tc ++ ": " ++ res ++ "\n"
      ++ "\texpected: '" ++ expected ++ "'\n"
      ++ "\t  actual: '" ++ actual   ++ "'"


test :: IO ()
test = mapM_ (putStrLn . prettyPrintTestcase) tests

