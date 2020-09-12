module Game.Logic where

import Game.Monad
import Game.Types

newGame :: GameConfig -> GameState
newGame config = GameState config Nothing cellStates globalState
  where
    numMines = totalMines config
    numCells = totalCells config
    globalState = GlobalGameState numMines (numCells - numMines) Playing

    row = flip map [0 .. boardWidth config - 1] $ const mempty
    cellStates = flip map [0 .. boardHeight config - 1] $ const row

getCellFixed :: Bool -> BoardCoordinate -> GameMonad CellState
getCellFixed safe coordinates = do
    cell <- getCell coordinates
    globalState <- getGlobalState

    fixCell globalState cell
  where
    fixCell globalState cell
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
    | Undo <- action, playState /= Win = undo state
    | playState /= Playing = state
    | (ToggleFlag coordinates) <- action = execState (toggleFlagState coordinates) state
    | (RevealArea coordinates) <- action = execStateWithUndo (revealArea coordinates) state
    | (Reveal coordinates) <- action = execStateWithUndo (reveal False coordinates) state
    | otherwise = error "Bug: unkwon action"
  where
    playState = (globalPlayState . globalGameState) state

revealArea :: BoardCoordinate -> GameMonad ()
revealArea coordinates = do
    cell <- getCell coordinates
    revealArea' cell
  where
    revealArea' cell
      | (CellState _ (Labeled _ countdown)) <- cell, countdown <= 0 = do
          neighbors <- neighborCoordinates coordinates
          _ <- sequence $ reveal False <$> neighbors
          return ()

      | otherwise = return ()

reveal :: Bool -> BoardCoordinate -> GameMonad ()
reveal force coordinates = do
    cell <- getCellFixed True coordinates
    reveal' cell
  where
    reveal' cell
      | isKnown cell || (isFlagged cell && not force) = return ()
      | otherwise = do
          neighbors <- fmap (getCellFixed False) <$> neighborCoordinates coordinates >>= sequence
          let mines = countCells isMine neighbors
          let flags = countCells isFlagged neighbors

          let newCell = revealedCell cell mines flags
          setCell coordinates newCell

          _ <- maybeRevealNeighbors newCell
          _ <- maybeFixAll . globalPlayState <$> getGlobalState

          return ()

    revealedCell cell mines flags
      | isMine cell = CellState Mine Known
      | isSafe cell = CellState Safe (Labeled mines (mines - flags))
      | otherwise = error "Bug: invalid state trying to reveal cell"

    maybeRevealNeighbors cell
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
    _ <- fmap updateCountdownLabel <$> neighborCoordinates coordinates >>= sequence

    return ()
  where
    toggleCell cell
        | currentState == Unknown = cell {visibleCellState = Flagged}
        | currentState == Flagged = cell {visibleCellState = Unsure}
        | currentState == Unsure = cell {visibleCellState = Unknown}
        | otherwise = cell
      where
        currentState = visibleCellState cell

updateCountdownLabel :: BoardCoordinate -> GameMonad ()
updateCountdownLabel coordinates = do
    flags <- (fmap getCell) <$> neighborCoordinates coordinates >>= sequence >>= return . countCells isFlagged
    cell <- getCell coordinates
    setCell coordinates $ updatedCounter cell flags

    return ()
  where
    updatedCounter (CellState innerState (Labeled mines _)) flags = CellState innerState (Labeled mines (mines - flags))
    updatedCounter cell _ = cell
