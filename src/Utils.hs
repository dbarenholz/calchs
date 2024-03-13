module Utils where

-- | Helper method to print a particular string as red text to the terminal.
red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

-- | Helper method to print a particular string as green text to the terminal.
green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"
