module Calc.Interactive.Key where

data Mod = CTRL | SHIFT | ALT | NONE deriving (Show, Eq)
type KeyMods = [Mod]

data Key
  = KeyUp           -- Go to the previously typed input
  | KeyDown         -- Go to the next typed input if it exists, or do nothing
  | KeyLeft         -- Move cursor left once, if possible
  | KeyRight        -- Move cursor right once, if possible
  | KeyEnd          -- Move to end of line
  | KeyHome         -- Move to start of line
  | KeyPageUp       -- Move to begin of history
  | KeyPageDown     -- Move to end of history
  | KeyEnter        -- Attempt to compute the written expression
  | KeyBackspace    -- Remove the character left of the cursor position
  | KeyDelete       -- Remove the character right of the cursor position
  | KeyTab          -- Attempt to autocomplete
  | KeyEsc          -- Deliberately ignore this to avoid  being annoying with other keycodes

  | KeyMod [Mod] Key -- Handles CTRL + SHIFT + ALT keys

  | NormalKey Char     -- A normal key, e.g. 'a' or '1' or '+'
  | UnknownKey String  -- Some form of error, e.g. a PageDown or something silly
  deriving (Show, Eq)
