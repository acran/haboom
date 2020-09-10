module GameLogic where

import Control.Lens (view, ix, over)
import Control.Monad.State (execState, gets, put, get, State)

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
    (config, GameState oldState _) <- get
    let newState = over (ix row) (over (ix column) f) oldState
    put (config, GameState newState (newCache config newState))
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
    (gameConfig, GameState gameState cache) <- get
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
    performAction | (gameStatus . getCache) state /= Playing = return ()
                  | (ToggleFlag coordinates) <- action = changeCell coordinates toggleFlagState
                  | (Reveal coordinates) <- action = revealAction coordinates

revealAction :: BoardCoordinate -> GameMonad ()
revealAction coordinates = do
  oldCell <- getCellFixed True coordinates
  revealCell oldCell coordinates
  return ()

revealCell :: CellState -> BoardCoordinate -> GameMonad ()
revealCell (CellState _ Known) _ = return ()
revealCell (CellState _ (Labeled _)) _ = return ()
revealCell (CellState _ Flagged) _ = return ()
revealCell oldCell coordinates = do
    neighbors <- fmap (getCellFixed False) <$> neighborCoordinates coordinates >>= sequence
    let mines = countCells isMine neighbors

    let newCell = revealedCell oldCell mines
    setCell coordinates newCell

    maybeRevealNeighbors newCell coordinates
    gameStatus <- gameStatus . getCache <$> gets snd
    maybeFixAll gameStatus

    return ()
  where
    revealedCell (CellState Mine _) _ = CellState Mine Known
    revealedCell (CellState Safe _) 0 = CellState Safe Known
    revealedCell (CellState Safe _) numMines = CellState Safe (Labeled numMines)

    maybeRevealNeighbors (CellState Safe Known) coordinates = do
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

toggleFlagState :: CellState -> CellState
toggleFlagState (CellState inner Unknown) = CellState inner Flagged
toggleFlagState (CellState inner Flagged) = CellState inner Unsure
toggleFlagState (CellState inner Unsure) = CellState inner Unknown
toggleFlagState cellState = cellState

initializeCellStates :: Int -> Int -> Int -> GameState
initializeCellStates width height mines = GameState [
      [newCell | y <- [0 .. (width - 1)]]
    | x <- [0 .. (height - 1)]] (StateCache mines (numCells - mines) Playing)
  where
    newCell = CellState Undefined Unknown
    numCells = width * height
