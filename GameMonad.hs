module GameMonad where

import Control.Monad.Trans.State (put, get, gets, State)
import Control.Lens (ix, over)

import Types

type GameMonad = State GameState

getCell :: BoardCoordinate -> GameMonad CellState
getCell coordinates = do
  cellStates <- gets cells
  let cell = cellFromState coordinates cellStates
  return cell

setCell :: BoardCoordinate -> CellState -> GameMonad ()
setCell coordinates cell = changeCell coordinates $ const cell

changeCell :: BoardCoordinate -> (CellState -> CellState) -> GameMonad ()
changeCell (BoardCoordinate column row) f = do
    GameState config prev oldState _ <- get
    let newState = over (ix row) (over (ix column) f) oldState
    put $ GameState config prev newState (newCache config newState)
  where
    newCache config newState =
      let
        fixedMines = countInState isMine newState
        remainingMines = totalMines config - fixedMines
        remainingCells = countInState isUndefined newState
        freeCells = remainingCells - remainingMines
        playState
          | any (\x -> isMine x && isKnown x) $ concat newState = Dead
          | countInState isKnown newState + totalMines config == boardHeight config * boardWidth config = Win
          | otherwise = Playing
      in GlobalGameState remainingMines freeCells playState
