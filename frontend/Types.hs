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

instance Semigroup CellState
instance Monoid CellState

isMine :: CellState -> Bool
isMine (CellState Mine _) = True
isMine _ = False

isSafe :: CellState -> Bool
isSafe (CellState Safe _) = True
isSafe _ = False

type GameState = [[CellState]]

data Action = Reveal BoardCoordinate | ToggleFlag BoardCoordinate
