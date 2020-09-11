module GameLogic where

import Control.Lens (view, ix, over)
import Control.Monad.State (modify, execState, gets, put, get, State)
import Data.Maybe (fromMaybe)

import GameMonad
import Types

getCellFixed :: Bool -> BoardCoordinate -> GameMonad CellState
getCellFixed safe coordinates = do
    GameState gameConfig _ gameState cache <- get
    let cell = cellFromState coordinates gameState

    fixCell gameConfig gameState cache coordinates cell
  where
    fixCell gameConfig gameState cache coordinates (CellState Undefined visibleState) = do
        let fixedCell = CellState internalState visibleState
        setCell coordinates fixedCell
        return fixedCell
      where
        internalState
          | remainingMines cache == 0 = Safe
          | freeCells cache == 0 = Mine
          | safe = Safe
          | otherwise = case freeCells cache `mod` remainingMines cache of
              1 -> Mine
              _ -> Safe

    fixCell _ _ _ _ cell = return cell

updateCell :: Action -> GameState -> GameState
updateCell action state =
    execState performAction state
  where
    performAction | Undo <- action, (playState . globalState) state /= Win = do
                    current@(GameState config undoState _ _) <- get
                    put (fromMaybe current undoState)

                  | (playState . globalState) state /= Playing = return ()
                  | (ToggleFlag coordinates) <- action = toggleFlagState coordinates
                  | (RevealArea coordinates) <- action = revealAreaAction coordinates
                  | (Reveal coordinates) <- action = do
                      undoState <- get
                      revealAction False coordinates
                      modify $
                        \(GameState config _ cellStates cache) -> (GameState config (Just undoState) cellStates cache)

revealAreaAction :: BoardCoordinate -> GameMonad ()
revealAreaAction coordinates = do
    cell <- getCell coordinates
    revealArea cell
  where
    revealArea cell
      | (CellState innerState (Labeled _ countdown)) <- cell, countdown <= 0 = do
          undoState <- get
          neighbors <- neighborCoordinates coordinates
          sequence $ revealAction False <$> neighbors
          modify $
            \(GameState config _ cellStates cache) -> GameState config (Just undoState) cellStates cache

      | otherwise = return ()

revealAction :: Bool -> BoardCoordinate -> GameMonad ()
revealAction force coordinates = do
  oldCell <- getCellFixed True coordinates
  revealCell force oldCell coordinates
  return ()

revealCell :: Bool -> CellState -> BoardCoordinate -> GameMonad ()
revealCell _ (CellState _ Known) _ = return ()
revealCell _ (CellState _ (Labeled _ _)) _ = return ()
revealCell False (CellState _ Flagged) _ = return ()
revealCell _ oldCell coordinates = do
    neighbors <- fmap (getCellFixed False) <$> neighborCoordinates coordinates >>= sequence
    let mines = countCells isMine neighbors
    let flags = countCells isFlagged neighbors

    let newCell = revealedCell oldCell mines flags
    setCell coordinates newCell

    maybeRevealNeighbors newCell coordinates
    playState <- playState . globalState <$> get
    maybeFixAll playState

    return ()
  where
    revealedCell (CellState Mine _) _ _ = CellState Mine Known
    revealedCell (CellState Safe _) numMines numFlags = CellState Safe (Labeled numMines (numMines - numFlags))

    maybeRevealNeighbors (CellState Safe (Labeled 0 _)) coordinates = do
      neighbors <- neighborCoordinates coordinates
      mapM (revealAction True) neighbors

    maybeRevealNeighbors _ coordinates = return []

    maybeFixAll Playing = return []
    maybeFixAll _ = do
      gameConfig <- gets gameConfig
      sequence [getCellFixed False (BoardCoordinate x y) | x <- [0..boardWidth gameConfig - 1], y <- [0..boardHeight gameConfig - 1]]

neighborCoordinates :: BoardCoordinate -> GameMonad [BoardCoordinate]
neighborCoordinates (BoardCoordinate x y) = do
  gameConfig <- gets gameConfig
  return [
      BoardCoordinate nx ny
    | nx <- [x-1 .. x+1],
      ny <- [y-1 .. y+1],

      (nx, ny) /= (x, y),
      nx >= 0,
      nx < boardWidth gameConfig,

      ny >= 0,
      ny < boardHeight gameConfig
    ]

toggleFlagState :: BoardCoordinate -> GameMonad ()
toggleFlagState coordinates = do
    toggledCell <- toggleCell <$> getCell coordinates
    setCell coordinates toggledCell
    fmap updateCountdownLabel <$> neighborCoordinates coordinates >>= sequence
    return ()
  where
    toggleCell (CellState inner Unknown) = CellState inner Flagged
    toggleCell (CellState inner Flagged) = CellState inner Unsure
    toggleCell (CellState inner Unsure) = CellState inner Unknown
    toggleCell cellState = cellState

updateCountdownLabel :: BoardCoordinate -> GameMonad ()
updateCountdownLabel coordinates = do
    neighbors <- fmap getCell <$> neighborCoordinates coordinates >>= sequence
    flags <- (fmap getCell) <$> neighborCoordinates coordinates >>= sequence >>= return . countCells isFlagged
    cell <- getCell coordinates
    setCell coordinates $ updatedCounter cell flags
    return ()
  where
    updatedCounter (CellState innerState (Labeled mines _)) flags = (CellState innerState (Labeled mines (mines-flags)))
    updatedCounter cell _ = cell

initializeCellStates :: GameConfig -> GameState
initializeCellStates config = GameState config Nothing [
      [newCell | y <- [0 .. (width - 1)]]
    | x <- [0 .. (height - 1)]] (GlobalGameState mines (numCells - mines) Playing)
  where
    width = boardWidth config
    height = boardHeight config
    mines = totalMines config
    newCell = CellState Undefined Unknown
    numCells = width * height
