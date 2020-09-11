module GameLogic where

import Control.Lens (view, ix, over)
import Control.Monad.State (modify, execState, gets, put, get, State)
import Data.Maybe (fromMaybe)

import Types

type GameMonad = State (GameConfig, GameState)

getCell :: BoardCoordinate -> GameMonad CellState
getCell coordinates = do
  cellStates <- gets $ getCells . snd
  let cell = cellFromState coordinates cellStates
  return cell

setCell :: BoardCoordinate -> CellState -> GameMonad ()
setCell coordinates cell = changeCell coordinates $ const cell

changeCell :: BoardCoordinate -> (CellState -> CellState) -> GameMonad ()
changeCell (BoardCoordinate column row) f = do
    (config, GameState prev oldState _) <- get
    let newState = over (ix row) (over (ix column) f) oldState
    put (config, GameState prev newState (newCache config newState))
  where
    newCache config newState =
      let
        fixedMines = countInState isMine newState
        remainingMines = getNumMines config - fixedMines
        remainingCells = countInState isUndefined newState
        freeCells = remainingCells - remainingMines
        gameStatus
          | any (\x -> isMine x && isKnown x) $ concat newState = Lost
          | countInState isKnown newState + getNumMines config == getBoardHeight config * getBoardWidth config = Won
          | otherwise = Playing
      in StateCache remainingMines freeCells gameStatus

getCellFixed :: Bool -> BoardCoordinate -> GameMonad CellState
getCellFixed safe coordinates = do
    (gameConfig, GameState _ gameState cache) <- get
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

updateCell :: GameConfig -> Action -> GameState -> GameState
updateCell gameConfig action state =
    snd $ execState performAction (gameConfig, state)
  where
    performAction | Undo <- action, (gameStatus . getCache) state /= Won = do
                    (config, current@(GameState undoState _ _)) <- get
                    put (config, fromMaybe current undoState)

                  | (gameStatus . getCache) state /= Playing = return ()
                  | (ToggleFlag coordinates) <- action = toggleFlagState coordinates
                  | (RevealArea coordinates) <- action = revealAreaAction coordinates
                  | (Reveal coordinates) <- action = do
                      undoState <- gets snd
                      revealAction coordinates
                      modify $
                        \(config, GameState _ cellStates cache) -> (config, GameState (Just undoState) cellStates cache)

revealAreaAction :: BoardCoordinate -> GameMonad ()
revealAreaAction coordinates = do
    cell <- getCell coordinates
    revealArea cell
  where
    revealArea cell
      | (CellState innerState (Labeled _ countdown)) <- cell, countdown <= 0 = do
          undoState <- gets snd
          neighbors <- neighborCoordinates coordinates
          sequence $ revealAction <$> neighbors
          modify $
            \(config, GameState _ cellStates cache) -> (config, GameState (Just undoState) cellStates cache)

      | otherwise = return ()

revealAction :: BoardCoordinate -> GameMonad ()
revealAction coordinates = do
  oldCell <- getCellFixed True coordinates
  revealCell oldCell coordinates
  return ()

revealCell :: CellState -> BoardCoordinate -> GameMonad ()
revealCell (CellState _ Known) _ = return ()
revealCell (CellState _ (Labeled _ _)) _ = return ()
revealCell (CellState _ Flagged) _ = return ()
revealCell oldCell coordinates = do
    neighbors <- fmap (getCellFixed False) <$> neighborCoordinates coordinates >>= sequence
    let mines = countCells isMine neighbors
    let flags = countCells isFlagged neighbors

    let newCell = revealedCell oldCell mines flags
    setCell coordinates newCell

    maybeRevealNeighbors newCell coordinates
    gameStatus <- gameStatus . getCache <$> gets snd
    maybeFixAll gameStatus

    return ()
  where
    revealedCell (CellState Mine _) _ _ = CellState Mine Known
    revealedCell (CellState Safe _) numMines numFlags = CellState Safe (Labeled numMines (numMines - numFlags))

    maybeRevealNeighbors (CellState Safe (Labeled 0 _)) coordinates = do
      neighbors <- neighborCoordinates coordinates
      mapM revealAction neighbors

    maybeRevealNeighbors _ coordinates = return []

    maybeFixAll Playing = return []
    maybeFixAll _ = do
      gameConfig <- gets fst
      sequence [getCellFixed False (BoardCoordinate x y) | x <- [0..getBoardWidth gameConfig - 1], y <- [0..getBoardHeight gameConfig - 1]]

neighborCoordinates :: BoardCoordinate -> GameMonad [BoardCoordinate]
neighborCoordinates (BoardCoordinate x y) = do
  gameConfig <- gets fst
  return [
      BoardCoordinate nx ny
    | nx <- [x-1 .. x+1],
      ny <- [y-1 .. y+1],

      (nx, ny) /= (x, y),
      nx >= 0,
      nx < getBoardWidth gameConfig,

      ny >= 0,
      ny < getBoardHeight gameConfig
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

initializeCellStates :: Int -> Int -> Int -> GameState
initializeCellStates width height mines = GameState Nothing [
      [newCell | y <- [0 .. (width - 1)]]
    | x <- [0 .. (height - 1)]] (StateCache mines (numCells - mines) Playing)
  where
    newCell = CellState Undefined Unknown
    numCells = width * height
