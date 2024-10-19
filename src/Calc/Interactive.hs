module Calc.Interactive where

import System.Console.Haskeline
import Control.Monad.IO.Class (MonadIO)

import Calc (compute)
import Calc.Types (Options)

-- | Helper method to show the result.
showResult :: MonadIO m => Either String String -> InputT m ()
showResult (Left err) = outputStrLn err
showResult (Right ok) = outputStrLn $ "= " ++ ok

-- | REPL using haskeline
runInteractively :: Options -> IO ()
runInteractively opts = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     minput <- getInputLine "$ "
     case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just input  -> do
         showResult (compute opts input)
         loop
