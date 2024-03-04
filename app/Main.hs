module Main where

import System.Environment

import Lexer (lex)
import Parser (parse)
import Evaluator (eval)

main :: IO ()
main = do
  args   <- getArgs
  arg    <- return (unwords args)
  tokens <- return (Lexer.lex arg)
  expr   <- return (Parser.parse tokens)
  res    <- return (Evaluator.eval expr)
  putStrLn res

