module Main where

import System.Environment (getArgs)

import Calc.Interactive (runInteractively)
import Calc (compute)

-- | Main function. Parsers arguments, then computes the result and prints it.
main :: IO ()
main = do
  args      <- getArgs
  if args == []  -- potentially also manage newlines or extra spaces if getArgs doesn't handle that the way I want it
    then runInteractively
    else do
      let arg    = unwords args
      let result = compute arg
      case result of
        Left errMsg -> putStrLn $ errMsg
        Right ok    -> putStrLn $ ok
