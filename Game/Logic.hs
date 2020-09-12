module Game.Logic where

import Control.Lens (view, ix, over)
import Data.Maybe (fromMaybe)

import Game.Monad
import Game.Types

newGame :: GameConfig -> GameState
newGame config = GameState config Nothing cellStates globalState
  where
    numMines = totalMines config
    numCells = totalCells config
    globalState = GlobalGameState numMines (numCells - numMines) Playing

    freshCell = CellState Undefined Unknown
    row = flip map [0 .. boardWidth config - 1] $ const freshCell
    cellStates = flip map [0 .. boardHeight config - 1] $ const row

getCellFixed :: Bool -> BoardCoordinate -> GameMonad CellState
getCellFixed safe coordinates = do
    cell <- getCell coordinates
    globalState <- getGlobalState

    fixCell globalState coordinates cell
  where
    fixCell globalState coordinates cell
        | (CellState Undefined visibleState) <- cell = do
            let fixedCell = CellState fixedInternalState visibleState
            setCell coordinates fixedCell
            return fixedCell
        | otherwise = return cell
      where
        fixedInternalState
          | 0 == remainingMines globalState = Safe
          | 0 == freeCells globalState = Mine
          | safe = Safe
          | otherwise = case freeCells globalState `mod` remainingMines globalState of
              1 -> Mine
              _ -> Safe

execAction :: Action -> GameState -> GameState
execAction action state
  | Undo <- action, (playState . globalState) state /= Win = undo state
  | (playState . globalState) state /= Playing = state
  | (ToggleFlag coordinates) <- action = execState (toggleFlagState coordinates) state
  | (RevealArea coordinates) <- action = execStateWithUndo (revealArea coordinates) state
  | (Reveal coordinates) <- action = execStateWithUndo (reveal False coordinates) state

revealArea :: BoardCoordinate -> GameMonad ()
revealArea coordinates = do
    cell <- getCell coordinates
    revealArea cell
  where
    revealArea cell
      | (CellState _ (Labeled _ countdown)) <- cell, countdown <= 0 = do
          neighbors <- neighborCoordinates coordinates
          sequence $ reveal False <$> neighbors
          return ()

      | otherwise = return ()

reveal :: Bool -> BoardCoordinate -> GameMonad ()
reveal force coordinates = do
    cell <- getCellFixed True coordinates
    revealCell force cell coordinates
  where
    revealCell force cell coordinates
      | isKnown cell || (isFlagged cell && not force) = return ()
      | otherwise = do
          neighbors <- fmap (getCellFixed False) <$> neighborCoordinates coordinates >>= sequence
          let mines = countCells isMine neighbors
          let flags = countCells isFlagged neighbors

          let newCell = revealedCell cell mines flags
          setCell coordinates newCell

          maybeRevealNeighbors newCell coordinates
          maybeFixAll . playState <$> getGlobalState

          return ()

    revealedCell cell mines flags
      | isMine cell = CellState Mine Known
      | isSafe cell = CellState Safe (Labeled mines (mines - flags))

    maybeRevealNeighbors cell coordinates
      | (CellState Safe (Labeled 0 _)) <- cell = do
        neighbors <- neighborCoordinates coordinates
        mapM (reveal True) neighbors
      | otherwise = return []

    maybeFixAll Playing = return []
    maybeFixAll _ = do
      config <- getConfig
      sequence [
          getCellFixed False (BoardCoordinate column row)
          | column <- [0..boardWidth config - 1],
            row <- [0..boardHeight config - 1]
        ]

neighborCoordinates :: BoardCoordinate -> GameMonad [BoardCoordinate]
neighborCoordinates (BoardCoordinate x y) = do
  config <- getConfig
  return [
      BoardCoordinate nx ny
    | nx <- [x-1 .. x+1],
      ny <- [y-1 .. y+1],

      (nx, ny) /= (x, y),
      nx >= 0,
      nx < boardWidth config,

      ny >= 0,
      ny < boardHeight config
    ]

toggleFlagState :: BoardCoordinate -> GameMonad ()
toggleFlagState coordinates = do
    cell <- toggleCell <$> getCell coordinates
    setCell coordinates cell
    fmap updateCountdownLabel <$> neighborCoordinates coordinates >>= sequence

    return ()
  where
    toggleCell cell
        | currentState == Unknown = cell {visibleState = Flagged}
        | currentState == Flagged = cell {visibleState = Unsure}
        | currentState == Unsure = cell {visibleState = Unknown}
        | otherwise = cell
      where
        currentState = visibleState cell

updateCountdownLabel :: BoardCoordinate -> GameMonad ()
updateCountdownLabel coordinates = do
    neighbors <- fmap getCell <$> neighborCoordinates coordinates >>= sequence
    flags <- (fmap getCell) <$> neighborCoordinates coordinates >>= sequence >>= return . countCells isFlagged
    cell <- getCell coordinates
    setCell coordinates $ updatedCounter cell flags

    return ()
  where
    updatedCounter (CellState innerState (Labeled mines _)) flags = CellState innerState (Labeled mines (mines - flags))
    updatedCounter cell _ = cell
