module Types where

-- | Board configuration for a new game
data GameConfig =
  -- | create new game configuration:
  --
  -- GameConfig @width@ @height@ @numMines@
  GameConfig {
    getBoardWidth :: Integer,
    getBoardHeight :: Integer,
    getNumMines :: Integer
  }
