module Game.Types where

import Control.Lens (ix, view)
import Data.Maybe (fromMaybe)

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
  deriving Eq

totalCells :: GameConfig -> Int
totalCells config = boardWidth config * boardHeight config

data BoardCoordinate = BoardCoordinate {
  boardColumn :: Int,
  boardRow :: Int
}

data InternalCellState = Safe | Mine | Undefined
  deriving Eq

data VisibleCellState = Unknown
                      | Known
                      | Flagged
                      | Unsure
                      | Labeled {
                          totalLabel :: Int,
                          countdownLabel:: Int
                        }
  deriving Eq

data CellState = CellState {
    internalCellState :: InternalCellState,
    visibleCellState :: VisibleCellState
  }
  deriving Eq

instance Semigroup CellState where
  _ <> b = b

instance Monoid CellState where
  mempty = CellState Undefined Unknown

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
  globalGameState :: GlobalGameState
}
  deriving Eq

undo :: GameState -> GameState
undo state = fromMaybe state $ previousState state

data PlayState = Playing | Win | Dead
  deriving Eq

data GlobalGameState = GlobalGameState {
    remainingMines :: Int, -- ^number of floating mines
    freeCells :: Int, -- ^number of floating safe cells
    globalPlayState :: PlayState -- ^whether game was won/lost or is still playing
  }
  deriving Eq

cellFromState :: BoardCoordinate -> CellStates -> CellState
cellFromState (BoardCoordinate column row) cellStates = view (ix column) $ view (ix row) cellStates

countCells :: (CellState -> Bool) -> [CellState] -> Int
countCells predicate = foldr ((+) . (fromEnum . predicate)) 0

countInState :: (CellState -> Bool) -> CellStates -> Int
countInState predicate cellStates = countCells predicate $ concat cellStates

data Action = Reveal BoardCoordinate | RevealArea BoardCoordinate | ToggleFlag BoardCoordinate | Undo
