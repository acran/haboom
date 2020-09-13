module Gui.Types where

-- | settings on how to display the game board
data DisplaySettings = DisplaySettings {
  debugMode :: Bool, -- ^show internal state of fixed cells
  countdownMode :: Bool -- ^show labels in countdown mode
}
