module GameLogic where

import Control.Lens (view, ix, over)
import Control.Monad.State (execState, gets, put, get, State)

import Types

type GameMonad = State (GameConfig, GameState)

getCell :: BoardCoordinate -> GameMonad CellState
getCell coordinates = do
  gameState <- gets snd
  let cell = cellFromState coordinates gameState
  return cell

setCell :: BoardCoordinate -> CellState -> GameMonad ()
setCell coordinates cell = changeCell coordinates $ const cell

changeCell :: BoardCoordinate -> (CellState -> CellState) -> GameMonad ()
changeCell (BoardCoordinate column row) f = do
  (config, oldState) <- get
  let newState = over (ix row) (over (ix column) f) oldState
  put (config, newState)

getCellFixed :: Bool -> BoardCoordinate -> GameMonad CellState
getCellFixed safe coordinates = do
    (gameConfig, gameState) <- get
    let cell = cellFromState coordinates gameState

    fixCell gameConfig gameState coordinates cell
  where
    fixCell gameConfig gameState coordinates (CellState Undefined visibleState)= do
        let fixedCell = CellState internalState visibleState
        setCell coordinates fixedCell
        return fixedCell
      where
        internalState =
            case (remainingMines, remainingCells - remainingMines, safe) of
              (0, _, _) -> Safe
              (_, 0, _) -> Mine
              (_, _, True) -> Safe
              (_, freeCells, _) -> case freeCells `mod` remainingMines of
                  1 -> Mine
                  _ -> Safe
          where
            numMines = getNumMines gameConfig
            fixedMines = foldr ((+) . (fromEnum . isMine)) 0 $ concat gameState
            remainingCells = foldr ((+) . (fromEnum . isUndefined)) 0 $ concat gameState
            remainingMines = numMines - fixedMines

    fixCell _ _ _ cell = return cell

updateCell :: GameConfig -> Action -> GameState -> GameState
updateCell gameConfig action state =
    snd $ execState performAction (gameConfig, state)
  where
    performAction | (ToggleFlag coordinates) <- action = changeCell coordinates toggleFlagState
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

    return ()
  where
    revealedCell (CellState Mine _) _ = CellState Mine Known
    revealedCell (CellState Safe _) 0 = CellState Safe Known
    revealedCell (CellState Safe _) numMines = CellState Safe (Labeled numMines)

    maybeRevealNeighbors (CellState Safe Known) coordinates = do
      neighbors <- neighborCoordinates coordinates
      mapM revealAction neighbors

    maybeRevealNeighbors _ coordinates = return []

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

initializeCellStates :: Integral a => a -> a -> a -> GameState
initializeCellStates width height mines = [
      [CellState Undefined Unknown | y <- [0 .. (width - 1)]]
    | x <- [0 .. (height - 1)]]
