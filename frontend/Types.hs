module Types where

-- | Board configuration for a new game
data GameConfig =
  -- | create new game configuration:
  --
  -- GameConfig @width@ @height@ @numMines@
  GameConfig {
    getBoardWidth :: Int,
    getBoardHeight :: Int,
    getNumMines :: Int
  }

data BoardCoordinate = BoardCoordinate {
  getXCoordinate :: Int,
  getYCoordinate :: Int
}

data InternalCellState = Safe | Mine
  deriving Show

data VisibleCellState = Unknown | Known | Labeled Int | Flagged | Unsure
  deriving Show

data CellState = CellState {
    getInternalState :: InternalCellState,
    getVisibleState :: VisibleCellState
  }
  deriving Show

type GameState = [[CellState]]
