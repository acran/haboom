module GameMonad where

import Control.Lens (ix, over)
import Data.Function ((&))

import Types

newtype GameMonad a = GameMonad {
  runState :: GameState -> (a, GameState)
}

instance Functor GameMonad where
  fmap f mx = mx >>= return . f

instance Applicative GameMonad where
  pure = return
  mf <*> mx = mf >>= (\f -> mx >>= return . f)

instance Monad GameMonad where
  return x = GameMonad $ \s -> (x, s)
  GameMonad x >>= f =
    GameMonad $ \s0 -> let
      (val_x, s1) = x s0
      (GameMonad cont) = f val_x
    in cont s1

get :: GameMonad GameState
get = GameMonad $ \s -> (s, s)

gets :: (GameState -> b) -> GameMonad b
gets = flip fmap get

put :: GameState -> GameMonad ()
put x = GameMonad $ \_ -> ((), x)

modify :: (GameState -> GameState) -> GameMonad ()
modify f = do
  a <- get
  put (f a)

execState :: GameMonad a -> GameState -> GameState
execState m x = snd (runState m x)

execStateWithUndo :: GameMonad a -> GameState -> GameState
execStateWithUndo action state
   | result == state = result
   | otherwise = result {previousState = Just state}
  where result = execState action state

getCell :: BoardCoordinate -> GameMonad CellState
getCell coordinates = do
  cellStates <- gets cells
  let cell = cellFromState coordinates cellStates
  return cell

setCell :: BoardCoordinate -> CellState -> GameMonad ()
setCell (BoardCoordinate column row) cell = do
    state  <- get
    let updatedCells = over (ix row) (over (ix column) $ const cell) $ state & cells
    put $ state {
        cells = updatedCells,
        globalState = updateGlobalState updatedCells $ gameConfig state
      }
  where
    updateGlobalState cellStates config =
      let
        fixedMines = countInState isMine cellStates
        remainingMines = totalMines config - fixedMines
        remainingCells = countInState isUndefined cellStates
        freeCells = remainingCells - remainingMines

        isRevealedMine cell = isMine cell && isKnown cell
        totalCells =  boardHeight config * boardWidth config
        revealedCells = countInState isKnown cellStates
        playState
          | any isRevealedMine $ concat cellStates = Dead
          | revealedCells + totalMines config == totalCells = Win
          | otherwise = Playing

      in GlobalGameState remainingMines freeCells playState
