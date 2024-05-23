module Calc.Interactive.Runner where

import System.IO   (hSetBuffering, stdin, BufferMode (NoBuffering), stdout, hSetEcho)
import System.Exit (exitSuccess)
import Data.Char   (isPrint)

import Calc (compute)
import Calc.Interactive.Key as Key


-- | Run the calculator interactively.
go :: IO ()
go = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdout False
  repl []

type History = [(String, Either String String)]

-- | Repl loop. Read line, evaluate line, print line, manage state.
repl :: History -> IO ()
repl histOfLines = do
  -- print $ "history=" ++ show histOfLines
  toEval    <- processState Ready [] 0 ""
  -- print $ "toEval=" ++ toEval
  let res   =  compute toEval
  -- print $ "res=" ++ show res
  let hist' =  histOfLines ++ [(toEval, res)]
  showResult res
  repl hist'

-- | Shows a result
-- TODO: move to utils?
showResult :: Either String String -> IO ()
showResult (Left err) = putStrLn err
showResult (Right ok) = putStrLn $ "= " ++ ok

-- | Processing a state means:
-- * get the action to handle
-- * make a single step with that action
-- * perform some IO output
-- * continue processing of the next state obtained by doing the step
processState :: State -> [Key] -> Int -> String -> IO String
processState state keys pos repr = do
  c <- getChar
  -- print $ "c=" ++ [c]
  let (newState, key) = step state c
  -- print $ "new state=" ++ show newState
  -- print $ "key=" ++ show key
  -- print $ "pos=" ++ show pos
  -- print $ "repr=" ++ repr
  case newState of
    Done       -> exitSuccess
    ParseEsc _ -> processState newState keys pos repr
    Ready      -> case key of
      -- Normal operation: writing + pressing enter
      KeyEnter -> do
          putChar '\n'
          pure repr
      NormalKey k ->
          let
            (begin, end) = splitAt pos repr
            nRepr        = begin ++ [k] ++ end
            moveLeft     = length end
          in if moveLeft == 0
             then do
               putChar k
               processState newState [] (pos + 1) nRepr
             else do
               putStr $ k : end
               putStr $ "\ESC[" ++ show moveLeft ++ "D"
               processState newState [] (pos + 1) nRepr

      -- Movement to the left
      KeyLeft -> do
        putStr "\ESC[D"
        processState newState keys (max 0 (pos - 1)) repr
      KeyMod [CTRL] KeyLeft ->
       if pos > 0 then
         let
           (begin, _)   = splitAt pos repr
           numSpaces    = 1 + (length $ takeWhile (== ' ') $ reverse begin)
           moveLeft     = numSpaces + length (last (words begin))
         in do
           putStr $ "\ESC[" ++ show moveLeft ++ "D"
           processState newState [] (pos - moveLeft) repr
       else do
         processState newState [] pos repr
      KeyHome -> do
        putStr $ "\ESC[" ++ show pos ++ "D"
        processState newState keys 0 repr

      -- Movement to the right
      KeyRight     ->
        if pos >= length repr then do
          processState newState keys pos repr
        else do
          putStr "\ESC[C"
          processState newState keys (pos + 1) repr
      KeyMod [CTRL] KeyRight -> 
        if pos < length repr then
          let
           (_, end)  = splitAt pos repr
           numSpaces = length $ takeWhile (== ' ') end
           moveRight = numSpaces + length (head (words end))
          in do
            putStr $ "\ESC[" ++ show moveRight ++ "C"
            processState newState [] (pos + moveRight) repr
        else do
          processState newState [] pos repr
      KeyEnd       ->
        if pos >= length repr then do
          processState newState keys pos repr
        else let
          (_, end) = splitAt pos repr
        in do
          putStr $ "\ESC[" ++ show (length end) ++ "C"
          processState newState keys (length repr) repr

      -- Deleting chars to the left of cursor
      KeyBackspace ->
        if pos > 0 then
          let
            (begin, end)     = splitAt pos repr
            nRepr            = init begin ++ end
            moveCursorLeftBy = length end + 1 -- (The +1 is for the space that gets written after `end`)
          in do
            putStr $ "\ESC[D" ++ end ++ " \ESC[" ++ show moveCursorLeftBy ++ "D"
            processState newState [] (pos - 1) nRepr
        else do
          processState newState [] pos repr
      KeyMod [CTRL] KeyBackspace ->
        if pos > 0 then
          let
            (begin, end) = splitAt pos repr
          in
            if words begin == [] then
              let
                -- there are only potential spaces to the left
                numSpaces = length begin
                spaces = take numSpaces $ repeat ' '
              in do
                putStr $ "\ESC[" ++ show numSpaces ++ "D"
                putStr $ end ++ spaces
                putStr $ "\ESC[" ++ show (numSpaces + length end) ++ "D"
                processState newState [] 0 end
            else let
              numChars = length (takeWhile (== ' ') (reverse begin)) + length (head (reverse (words begin)))
              spaces   = take numChars $ repeat ' '
              nPos     = pos - numChars
              (nBegin, _) = splitAt nPos begin
              nRepr    = nBegin ++ end
            in do
              putStr $ "\ESC[" ++ show numChars ++ "D"
              putStr $ end ++ spaces
              putStr $ "\ESC[" ++ show (numChars + length end) ++ "D"
              processState newState [] nPos nRepr
        else do
          processState newState [] pos repr

      -- Deleting chars to the right of cursor
      KeyDelete    ->
        if pos < length repr then
          let
            (begin, end) = splitAt pos repr
            nEnd         = drop 1 end
            nRepr        = begin ++ nEnd
            moveLeft     = length nEnd + 1 -- (+1 for the added space)
          in do
            putStr $ nEnd ++ " "
            putStr $ "\ESC[" ++ show moveLeft ++ "D"
            processState newState [] pos nRepr
        else do
          processState newState [] pos repr
      KeyMod [CTRL] KeyDelete ->
        if pos < length repr then
          let
            (begin, end) = splitAt pos repr
          in
            if words end == [] then
              processState newState [] pos repr
            else let
              numChars  = length (takeWhile (== ' ') end) + length (head (words end))
              spaces    = take numChars $ repeat ' '
              (_, nEnd) = splitAt numChars end
              nRepr     = begin ++ nEnd
          in do
            putStr $ nEnd ++ spaces
            putStr $ "\ESC[" ++ show (numChars + length nEnd) ++ "D"
            processState newState [] pos nRepr
        else do
          processState newState [] pos repr




      --
      --
      --
      --
      -- Everything below this line is WIP and nonfunctional
      --
      --
      --
      --
      KeyUp        -> todo KeyUp       newState keys pos repr
      KeyDown      -> todo KeyDown     newState keys pos repr
      -- Go to beginning of history
      KeyPageUp    -> todo KeyPageUp   newState keys pos repr
      -- Go to end of history
      KeyPageDown  -> todo KeyPageDown newState keys pos repr
      -- Eventually: autocomplete
      KeyTab       -> todo KeyTab      newState keys pos repr
      -- Exit the app
      KeyEsc       -> todo KeyEsc      newState keys pos repr

      wtf -> error $ "don't yet know how to handle: " ++ show wtf


todo :: Key -> State -> [Key] -> Int -> String -> IO String
todo k state keys pos repr= do
  putStr $ "TODO: " ++ show k ++ "\n"
  processState state keys pos repr



-- | The state of the state machine is one of two things:
-- * we are ready to accept "new input"
-- * we are parsing some escape sequence
-- * the user pressed CTRL+D and wishes to exit
data State = Ready | ParseEsc String | Done deriving (Show)

step :: State -> Char -> (State, Key)
step (Ready) ('\NUL')      = (Ready, NormalKey ' ')                                      -- CTRL + Space -> Space
step (Ready) ('\SOH')      = (Ready, KeyMod [CTRL] (NormalKey 'a'))
step (Ready) ('\FF')       = (Ready, KeyMod [CTRL] (NormalKey 'l'))
step (Ready) ('\ESC')      = (ParseEsc "", undefined)
step (Ready) ('\DEL')      = (Ready, KeyBackspace)
step (Ready) ('\HT')       = (Ready, KeyTab)
step (Ready) ('\LF')       = (Ready, KeyEnter)
step (Ready) ('\EOT')      = (Done, undefined)
step (Ready) ('\b')        = (Ready, KeyMod [CTRL] KeyBackspace)
step (Ready) c | isPrint c = (Ready, NormalKey c)
step (Ready) c | otherwise = (Ready, UnknownKey $ "I don't know what '" ++ [c] ++ "' is")

step (ParseEsc ('[':'1':'2':'7':';':'5':_)) ('u') = (Ready, KeyMod [CTRL] KeyBackspace)
step (ParseEsc ('[':'1':'2':'7':';':_))     ('5') = (ParseEsc "[127;5", undefined)
step (ParseEsc ('[':'1':'2':'7':_))         (';') = (ParseEsc "[127;", undefined)
step (ParseEsc ('[':'1':'2':_))             ('7') = (ParseEsc "[127", undefined)
step (ParseEsc ('[':'1':'3':';':'2':_))     ('u') = (Ready, KeyEnter)                    -- Shift Enter -> Enter
step (ParseEsc ('[':'1':'3':';':_))         ('2') = (ParseEsc "[13;2", undefined)
step (ParseEsc ('[':'1':'3':_))             (';') = (ParseEsc "[13;", undefined)
step (ParseEsc ('[':'1':';':'7':_))         ('A') = (Ready, KeyMod [CTRL, ALT] KeyUp)
step (ParseEsc ('[':'1':';':'7':_))         ('B') = (Ready, KeyMod [CTRL, ALT] KeyDown)
step (ParseEsc ('[':'1':';':'7':_))         ('C') = (Ready, KeyMod [CTRL, ALT] KeyRight)
step (ParseEsc ('[':'1':';':'7':_))         ('D') = (Ready, KeyMod [CTRL, ALT] KeyLeft)
step (ParseEsc ('[':'1':';':'7':_))         ('F') = (Ready, KeyMod [CTRL, ALT] KeyEnd)
step (ParseEsc ('[':'1':';':'7':_))         ('H') = (Ready, KeyMod [CTRL, ALT] KeyHome)
step (ParseEsc ('[':'1':';':'5':_))         ('A') = (Ready, KeyMod [CTRL] KeyUp)
step (ParseEsc ('[':'1':';':'5':_))         ('B') = (Ready, KeyMod [CTRL] KeyDown)
step (ParseEsc ('[':'1':';':'5':_))         ('C') = (Ready, KeyMod [CTRL] KeyRight)
step (ParseEsc ('[':'1':';':'5':_))         ('D') = (Ready, KeyMod [CTRL] KeyLeft)
step (ParseEsc ('[':'1':';':'5':_))         ('F') = (Ready, KeyMod [CTRL] KeyEnd)
step (ParseEsc ('[':'1':';':'5':_))         ('H') = (Ready, KeyMod [CTRL] KeyHome)
step (ParseEsc ('[':'1':';':'3':_))         ('A') = (Ready, KeyMod [ALT] KeyUp)
step (ParseEsc ('[':'1':';':'3':_))         ('B') = (Ready, KeyMod [ALT] KeyDown)
step (ParseEsc ('[':'1':';':'3':_))         ('C') = (Ready, KeyMod [ALT] KeyRight)
step (ParseEsc ('[':'1':';':'3':_))         ('D') = (Ready, KeyMod [ALT] KeyLeft)
step (ParseEsc ('[':'1':';':'3':_))         ('F') = (Ready, KeyMod [ALT] KeyEnd)
step (ParseEsc ('[':'1':';':'3':_))         ('H') = (Ready, KeyMod [ALT] KeyHome)
step (ParseEsc ('[':'1':';':_))             ('7') = (ParseEsc "[1;7", undefined)
step (ParseEsc ('[':'1':';':_))             ('5') = (ParseEsc "[1;5", undefined)
step (ParseEsc ('[':'1':';':_))             ('3') = (ParseEsc "[1;3", undefined)
step (ParseEsc ('[':'1':_))                 (';') = (ParseEsc "[1;", undefined)
step (ParseEsc ('[':'1':_))                 ('2') = (ParseEsc "[12", undefined)
step (ParseEsc ('[':'1':_))                 ('3') = (ParseEsc "[13", undefined)
step (ParseEsc ('[':'3':';':'7':_))         ('~') = (Ready, KeyMod [CTRL, ALT] KeyDelete)
step (ParseEsc ('[':'3':';':'5':_))         ('~') = (Ready, KeyMod [CTRL] KeyDelete)
step (ParseEsc ('[':'3':';':'3':_))         ('~') = (Ready, KeyMod [ALT] KeyDelete)
step (ParseEsc ('[':'3':'2':';':'2':_))     ('u') = (Ready, NormalKey ' ')                -- Shift Space -> Space
step (ParseEsc ('[':'3':'2':';':'6':_))     ('u') = (Ready, NormalKey ' ')                -- CTRL+Shift Space -> Space
step (ParseEsc ('[':'3':'2':';':_))         ('6') = (ParseEsc "[32;6", undefined)
step (ParseEsc ('[':'3':'2':';':_))         ('2') = (ParseEsc "[32;2", undefined)
step (ParseEsc ('[':'3':'2':_))             (';') = (ParseEsc "[32;", undefined)
step (ParseEsc ('[':'3':';':_))             ('7') = (ParseEsc "[3;7", undefined)
step (ParseEsc ('[':'3':';':_))             ('5') = (ParseEsc "[3;5", undefined)
step (ParseEsc ('[':'3':';':_))             ('3') = (ParseEsc "[3;3", undefined)
step (ParseEsc ('[':'3':_))                 (';') = (ParseEsc "[3;", undefined)
step (ParseEsc ('[':'3':_))                 ('2') = (ParseEsc "[32", undefined)
step (ParseEsc ('[':'3':_))                 ('~') = (Ready, KeyDelete)
step (ParseEsc ('[':'5':_))                 ('u') = (Ready, KeyEnter)                     -- CTRL Enter -> Enter
step (ParseEsc ('[':'5':_))                 ('~') = (Ready, KeyPageUp)
step (ParseEsc ('[':'6':_))                 ('u') = (Ready, KeyEnter)                     -- CTRL+Shift Enter -> Enter
step (ParseEsc ('[':'6':_))                 ('~') = (Ready, KeyPageDown)
step (ParseEsc ('[':_))                     ('A') = (Ready, KeyUp)
step (ParseEsc ('[':_))                     ('B') = (Ready, KeyDown)
step (ParseEsc ('[':_))                     ('C') = (Ready, KeyRight)
step (ParseEsc ('[':_))                     ('D') = (Ready, KeyLeft)
step (ParseEsc ('[':_))                     ('F') = (Ready, KeyEnd)
step (ParseEsc ('[':_))                     ('H') = (Ready, KeyHome)
step (ParseEsc ('[':_))                     ('1') = (ParseEsc "[1", undefined)
step (ParseEsc ('[':_))                     ('3') = (ParseEsc "[3", undefined)
step (ParseEsc ('[':_))                     ('5') = (ParseEsc "[5", undefined)
step (ParseEsc ('[':_))                     ('6') = (ParseEsc "[6", undefined)
step (ParseEsc [])                          ('[') = (ParseEsc "[", undefined)
step (ParseEsc [])                          ('f') = (Ready, KeyMod [ALT] (NormalKey 'f'))
step (ParseEsc [])                          ('b') = (Ready, KeyMod [ALT] (NormalKey 'b'))
step (ParseEsc s)                            c    = error $ "help:\n" ++ s ++ [c]

-- TODO: this is inaccurate; inputs for which this fails
-- '\ESC', '\ESC'
-- '\ESC', '<C-KeyRight>'
-- step (ParseEsc s) c | isPrint c = (Ready, KeyEsc : others)
--   where others = map NormalKey s ++ [NormalKey c]
-- step (ParseEsc s) c | otherwise = (Ready, KeyEsc : others)
--   where others = map NormalKey s ++ [UnknownKey $ "I don't know what '" ++ [c] ++ "' is"]

step (Done) c = error $ "user input when done?\n" ++ show c
