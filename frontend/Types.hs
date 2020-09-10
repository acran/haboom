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

data InternalCellState = Safe | Mine | Undefined
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

type CellStates = [[CellState]]

isMine :: CellState -> Bool
isMine (CellState Mine _) = True
isMine _ = False

isSafe :: CellState -> Bool
isSafe (CellState Safe _) = True
isSafe _ = False

isUndefined :: CellState -> Bool
isUndefined (CellState Undefined _) = True
isUndefined _ = False

isFlagged :: CellState -> Bool
isFlagged (CellState _ Flagged) = True
isFlagged _ = False

isKnown :: CellState -> Bool
isKnown (CellState _ Known) = True
isKnown (CellState _ (Labeled _)) = True
isKnown _ = False

data GameState = GameState {
  getCells :: CellStates,
  getCache :: StateCache
}

data GameStatus = Playing | Won | Lost
  deriving Eq

data StateCache = StateCache {
    remainingMines :: Int, -- ^number of floating mines
    freeCells :: Int, -- ^number of floating safe cells
    gameStatus :: GameStatus -- ^whether game was won/lost or is still playing
  }

cellFromState :: BoardCoordinate -> CellStates -> CellState
cellFromState (BoardCoordinate column row) cellStates = cellStates !! row !! column

countCells :: (CellState -> Bool) -> [CellState] -> Int
countCells predicate = foldr ((+) . (fromEnum . predicate)) 0

countInState :: (CellState -> Bool) -> CellStates -> Int
countInState predicate cellStates = countCells predicate $ concat cellStates

data Action = Reveal BoardCoordinate | ToggleFlag BoardCoordinate
