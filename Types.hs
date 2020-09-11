module Types where

-- | Board configuration for a new game
data GameConfig =
  -- | create new game configuration:
  --
  -- GameConfig @width@ @height@ @numMines@
  GameConfig {
    boardWidth :: Int,
    boardHeight :: Int,
    totalMines :: Int
  }

data BoardCoordinate = BoardCoordinate {
  boardColumn :: Int,
  boardRow :: Int
}

data InternalCellState = Safe | Mine | Undefined
  deriving Show

data VisibleCellState = Unknown
                      | Known
                      | Flagged
                      | Unsure
                      | Labeled {
                          totalLabel :: Int,
                          countdownLabel:: Int
                        }
  deriving Show

data CellState = CellState {
    internalState :: InternalCellState,
    visibleState :: VisibleCellState
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
isKnown (CellState _ (Labeled _ _)) = True
isKnown _ = False

data GameState = GameState {
  gameConfig :: GameConfig,
  previousState :: Maybe GameState,
  cells :: CellStates,
  globalState :: GlobalGameState
}

data PlayState = Playing | Win | Dead
  deriving Eq

data GlobalGameState = GlobalGameState {
    remainingMines :: Int, -- ^number of floating mines
    freeCells :: Int, -- ^number of floating safe cells
    playState :: PlayState -- ^whether game was won/lost or is still playing
  }

cellFromState :: BoardCoordinate -> CellStates -> CellState
cellFromState (BoardCoordinate column row) cellStates = cellStates !! row !! column

countCells :: (CellState -> Bool) -> [CellState] -> Int
countCells predicate = foldr ((+) . (fromEnum . predicate)) 0

countInState :: (CellState -> Bool) -> CellStates -> Int
countInState predicate cellStates = countCells predicate $ concat cellStates

data Action = Reveal BoardCoordinate | RevealArea BoardCoordinate | ToggleFlag BoardCoordinate | Undo
