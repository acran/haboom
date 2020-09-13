module Game.Types where

import Control.Lens (ix, view)
import Data.Maybe (fromMaybe)

-- | Board configuration for a new game
data GameConfig =
  -- | GameConfig @width@ @height@ @numMines@
  GameConfig {
    boardWidth :: Int, -- ^number of columns on the board
    boardHeight :: Int, -- ^number of rows on the board
    totalMines :: Int -- ^total number of mines in the game
  }
  deriving Eq

-- | total number of cells for this 'GameConfig'
totalCells :: GameConfig -> Int
totalCells config = boardWidth config * boardHeight config

-- | position of a cell on the board
data BoardCoordinate =
  -- | BoardCoordinate @column@ @row@
  BoardCoordinate {
    boardColumn :: Int, -- ^column (x-axis)
    boardRow :: Int -- ^row (y-axis)
  }

-- | internal (non-visible) state of a cell
data InternalCellState =
      Safe -- ^the cell does not contain a mine
    | Mine -- ^the cell does contain a mine
    | Undefined -- ^it is not fixed __yet__ whether this cell will contain a mine
  deriving Eq

-- | state of a cell visible to the player
data VisibleCellState =
      Unknown -- ^cell is still hidden
    | Known -- ^cell is open (and contains a mine)
    | Flagged -- ^cell was falgged by the player
    | Unsure -- ^cell was flagged as /unsure/ by the player
    | Labeled -- ^cell was revealed and shows the number of adjacent mines
      {
        totalLabel :: Int, -- ^total number of adjacent mines
        countdownLabel:: Int -- ^remaining number of adjacent mines based on set flags
      }
  deriving Eq

-- | the state of cell on the board
data CellState =
    -- | CellState @internalState@ @visibleState@
    CellState {
      internalCellState :: InternalCellState, -- ^internal state of the cell
      visibleCellState :: VisibleCellState -- ^visible state of the cell
    }
  deriving Eq

-- mock implementation to satisfy Lens functions
instance Semigroup CellState where
  _ <> b = b

instance Monoid CellState where
  mempty = CellState Undefined Unknown

-- | rows and columns of cells representing the game board
type CellStates = [[CellState]]

-- | whether a cell (definitely) contains a mine
isMine :: CellState -> Bool
isMine (CellState Mine _) = True
isMine _ = False

-- | whether a cell (definitely) contains __no__ mine
isSafe :: CellState -> Bool
isSafe (CellState Safe _) = True
isSafe _ = False

-- | whether the content of the cell was not fixed /yet/
isUndefined :: CellState -> Bool
isUndefined (CellState Undefined _) = True
isUndefined _ = False

-- | whether the cell was flagged by the player
isFlagged :: CellState -> Bool
isFlagged (CellState _ Flagged) = True
isFlagged _ = False

-- | whether the cell is revealed
isKnown :: CellState -> Bool
isKnown (CellState _ Known) = True
isKnown (CellState _ (Labeled _ _)) = True
isKnown _ = False

-- | represents the complete and consistent state of the game
--   in a given point in time
data GameState =
    -- | GameState @config@ @Maybe previousState@ @cellStates@ @globalGameState@
    GameState {
      gameConfig :: GameConfig, -- ^board configuration for this game
      previousState :: Maybe GameState, -- ^previous game state used for undo operations
      cells :: CellStates, -- ^state of all cells on the board
      globalGameState :: GlobalGameState -- ^some global states calculated from the states of __all__ cells
    }
  deriving Eq

-- | return the previous game state to undo last reveal
--
--   if no previous state is available, the same state is returned
undo :: GameState -> GameState
undo state = fromMaybe state $ previousState state

-- | current win/lose state of the game
data PlayState =
      Playing -- ^game is still running
    | Win -- ^the player won
    | Dead -- ^the player lost
  deriving Eq

-- | global states of the game calculated from all cell states
data GlobalGameState =
  -- | GlobalGameState @remainingMines@ @freeCells@ @playState@
  GlobalGameState {
    remainingMines :: Int, -- ^number of floating mines
    freeCells :: Int, -- ^number of floating safe cells
    globalPlayState :: PlayState -- ^whether game was won/lost or is still playing
  }
  deriving Eq

-- | get cell on specific coordinate from @cellStates@
cellFromState :: BoardCoordinate -> CellStates -> CellState
cellFromState (BoardCoordinate column row) cellStates = view (ix column) $ view (ix row) cellStates

-- | count cells in list matching the @predicate@
countCells :: (CellState -> Bool) -> [CellState] -> Int
countCells predicate = length . filter predicate

-- | count cells in @cellStates@ matching the @predicate@
countInState :: (CellState -> Bool) -> CellStates -> Int
countInState predicate cellStates = countCells predicate $ concat cellStates

-- | a move in the game played by the player
data GameAction =
    Reveal BoardCoordinate -- ^reveal a single cell
  | RevealArea BoardCoordinate -- ^reveal all adjacent safe cells if possible
  | ToggleFlag BoardCoordinate -- ^toggle flag/unsure on cell
  | Undo -- ^undo last 'Reveal' / 'RevealArea' action
