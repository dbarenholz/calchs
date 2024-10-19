module Main where

import System.Environment (getArgs)
import Data.Maybe (fromJust, isNothing)
import Data.Char (isDigit)

import qualified Data.Version as C (showVersion)
import qualified Paths_calchs as C (version)

import Calc (compute)
import Calc.Interactive (runInteractively)
import Calc.Types (Options(..), NumberMode (..), NumberFormat (..))

-- | Main function. Parsers arguments, then computes the result and prints it.
main :: IO ()
main = do
  -- get arguments passed to calchs
  args <- getArgs
  -- parse those arguments into options, and the desired query
  let (opts, maybeQuery) = parseArguments args initOpts
  -- Show help if desired
  if help opts then showHelp
  -- Otherwise show version if desired
  else if version opts then showVersion
       -- Otherwise run interactively if no query
       else if isNothing maybeQuery then runInteractively opts
            -- Otherwise compute the result of the query
            else do
                 let result = compute opts (fromJust maybeQuery)
                 case result of
                   Left errMsg -> putStrLn errMsg
                   Right ok    -> putStrLn ok

-- | Default values for options
initOpts :: Options
initOpts = Options
  { numberMode=Default
  , numberFormat=Normal
  , joke=False
  , imprecise=False
  , help=False
  , version=False
  , convert=False
  , cats=False
  }

showVersion :: IO ()
showVersion = putStr (C.showVersion C.version)

showHelp :: IO ()
showHelp =
  let
    opts =
      [ "--help (or -h): shows help for the program (you're looking at it)"
      , "--joke: enables the joke mode"
      , "--imprecise: enables an imprecise mode"
      , "--cats: emulate cats walking over your keyboard in interactive mode"
      , "--scientific: enables scientific notation for results"
      , "--convert: enable conversion mode"
      , "--mode [b(inary) | h(ex) | 1234]: sets binary (b and binary work), hex (h and hex work), or arbitary base mode by passing an Int."
      ]
    header = "calchs v" ++ C.showVersion C.version ++ "  -  a command line calculator, written in Haskell"
    footer = "a learning project by dbarenholz"
    helpMsg =
      [ header
      , ""
      , "Usage: calchs [OPTIONS] \"[COMPUTATION]\""
      , ""
      , "Options:"
      ] ++ opts ++
      [ "\n"
      , footer
      ]
  in putStr (unlines helpMsg)


-- | Parses options and an optional computation from the program arguments.
parseArguments :: [String] -> Options -> (Options, Maybe String)
parseArguments args opts =
  -- Parse options from the arguments
  let (args', opts') = parseOption (args, opts)
      -- If we have elements left, then we have some computation.
      query          = if null args' then Nothing else Just (unwords args')
  in  (opts', query)

-- | Parse options recursively from the program arguments, returning the options found, and possibly remaining arguments.
parseOption :: ([String], Options) -> ([String], Options)
parseOption (args, opts) =
  -- If we have no arguments, then we're done.
  if null args then (args, opts)
  -- Otherwise, take 2 arguments and attempt to parse an option from it.
  else let items            = take 2 args
           (skipped, opts') = parseOptionPair (head items) (last items) opts
           -- If we didn't skip anything, we're done after this parse.
       in if skipped == 0 then  (args, opts')
           -- Othwerwise, continue parsing options, dropping the items to skip
          else parseOption (drop skipped args, opts')

-- | Helper function for parsing options.
parseOptionPair :: String -> String -> Options -> (Int, Options)
parseOptionPair opt _ opts   | opt == "--help"    || opt == "-h"     = (1, opts { help=True })
parseOptionPair opt _ opts   | opt == "--convert" || opt == "-c"     = (1, opts { convert=True })
parseOptionPair opt _ opts   | opt == "--joke"                       = (1, opts { joke=True })
parseOptionPair opt _ opts   | opt == "--version"                    = (1, opts { version=True })
parseOptionPair opt _ opts   | opt == "--imprecise"                  = (1, opts { imprecise=True })
parseOptionPair opt _ opts   | opt == "--cats"                       = (1, opts { cats=True })
parseOptionPair opt _ opts   | opt == "--scientific"                 = (1, opts { numberFormat=Scientific })
parseOptionPair opt val opts | opt == "--mode" && head val == 'b'    = (2, opts { numberMode=Binary })
parseOptionPair opt val opts | opt == "--mode" && head val == 'h'    = (2, opts { numberMode=Hex })
parseOptionPair opt val opts | opt == "--mode" && isDigit (head val)
  = if read val == (10 :: Int) then (2, opts {numberMode=Default})
    else (2, opts { numberMode=Base (read val) })
parseOptionPair _ _ opts = (0, opts)


{-
-- This section is left for debugging purposes.
-- There are a few functions with identical implementation, but wrapped in an IO monad for debugging.

parseArguments' :: [String] -> Options -> IO (Options, Maybe String)
parseArguments' args opts = do
  putStrLn "parseArguments:"
  (args', opts') <-  parseOption' (args, opts)
  putStrLn $ "  opts' = " ++ show opts'
  putStrLn $ "  args' = " ++ show args'
  let query = if null args' then Nothing else Just (unwords args')
  putStrLn $ "  query = " ++ show query
  return (opts', query)

parseOption' :: ([String], Options) -> IO ([String], Options)
parseOption' (args, opts) = do
  putStrLn "parseOption:"
  putStrLn $ "  args = " ++ show args
  putStrLn $ "  opts = " ++ show opts
  if null args
    then return (args, opts)
    else do
      let items = take 2 args
      putStrLn $ "  items = " ++ show items
      let (skipped, opts') = parseOptionPair (head items) (last items) opts
      putStrLn $ "    skipped = " ++ show skipped
      putStrLn $ "    opts' = " ++ show opts'
      if skipped == 0
          then return (args, opts')
          else parseOption' (drop skipped args, opts')

-}
