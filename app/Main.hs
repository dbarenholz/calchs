module Main where

import System.Environment (getArgs)

import Calc (compute)

-- | Main function. Parsers arguments, then computes the result and prints it.
main :: IO ()
main = do
  args      <- getArgs
  let arg    = unwords args
  let result = compute arg
  putStrLn result
