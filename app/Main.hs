module Main where

import System.Environment (getArgs)
import Data.Maybe (fromJust, isNothing)
import Data.Char (isDigit, toLower)

import qualified Data.Version as C (showVersion)
import qualified Paths_calchs as C (version)

import Calc (compute)
import Calc.Interactive (runInteractively)
import Calc.Types (Options(..), NumberMode (..), NumberFormat (..))
import Control.Applicative (optional)

-- | Parses options and arguments. Computes the result and prints it, or runs the calculator interactively.
main :: IO ()
main = do
  args <- getArgs
  let (opts, maybeQuery) = parseArguments args initOpts
  if help opts then showHelp
  else if version opts then showVersion
  else if isNothing maybeQuery then runInteractively opts
  else do
    let result = compute opts (fromJust maybeQuery)
    case result of
      Left errMsg -> putStrLn errMsg
      Right ok    -> putStrLn ok

-- | Default values for options.
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

-- | Shows the program version, then exits.
showVersion :: IO ()
showVersion = putStr (C.showVersion C.version)

-- | Shows the program help, then exits.
showHelp :: IO ()
showHelp =
  let
    opts =
      [ "--help (or -h): shows help for the program (you're looking at it)"
      , "--version: return the version number (without newline)"
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

-- | Parses options and query from program arguments.
parseArguments :: [String] -> Options -> (Options, Maybe String)
parseArguments args opts =
  let (args', opts') = parseOption args opts
      query          = if null args' then Nothing else Just (unwords args')
  in  (opts', query)

-- | Parse options from program arguments, without consuming optional query.
parseOption :: [String] -> Options -> ([String], Options)
parseOption ("--mode":base:args) opts | base == "10"               = parseOption args opts { numberMode = Default }
parseOption ("--mode":base:args) opts | isDigit (head base)        = parseOption args opts { numberMode = Base (read base) }
parseOption ("--mode":mode:args) opts | toLower (head mode) == 'b' = parseOption args opts { numberMode = Binary }
parseOption ("--mode":mode:args) opts | toLower (head mode) == 'h' = parseOption args opts { numberMode = Hex }
parseOption (flag:args) opts | flag `elem` ["-h", "--help"]        = parseOption args opts { help = True }
parseOption (flag:args) opts | flag `elem` ["-c", "--convert"]     = parseOption args opts { convert = True }
parseOption (flag:args) opts | flag == "--joke"                    = parseOption args opts { joke = True }
parseOption (flag:args) opts | flag == "--version"                 = parseOption args opts { version = True }
parseOption (flag:args) opts | flag == "--imprecise"               = parseOption args opts { imprecise = True }
parseOption (flag:args) opts | flag == "--cats"                    = parseOption args opts { cats = True }
parseOption (flag:args) opts | flag == "--scientific"              = parseOption args opts { numberFormat = Scientific }
parseOption args opts                                              = (args, opts)

