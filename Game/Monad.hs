module Game.Monad where

import Control.Lens (ix, over)
import Data.Function ((&))

import Game.Types

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

execState :: GameMonad a -> GameState -> GameState
execState m x = snd (runState m x)

execStateWithUndo :: GameMonad a -> GameState -> GameState
execStateWithUndo action state
   | result == state = result
   | otherwise = result {previousState = Just state}
  where result = execState action state

get' :: GameMonad GameState
get' = GameMonad $ \s -> (s, s)

getConfig :: GameMonad GameConfig
getConfig = gameConfig <$> get'

getGlobalState :: GameMonad GlobalGameState
getGlobalState = globalGameState <$> get'

getCells :: GameMonad CellStates
getCells = cells <$> get'

getCell :: BoardCoordinate -> GameMonad CellState
getCell coordinates = cellFromState coordinates <$> getCells

setCell :: BoardCoordinate -> CellState -> GameMonad ()
setCell (BoardCoordinate column row) updatedCell = do
    state  <- get'
    let updatedCells = over (ix row) (over (ix column) $ const updatedCell) $ state & cells
    put $ state {
        cells = updatedCells,
        globalGameState = updateGlobalState updatedCells $ gameConfig state
      }
  where
    put x = GameMonad $ \_ -> ((), x)
    updateGlobalState cellStates config =
      let
        numFixedMines = countInState isMine cellStates
        numRemainingMines = totalMines config - numFixedMines
        numRemainingCells = countInState isUndefined cellStates
        numFreeCells = numRemainingCells - numRemainingMines

        isRevealedMine cell = isMine cell && isKnown cell
        numTotalCells =  boardHeight config * boardWidth config
        numRevealedCells = countInState isKnown cellStates
        playState
          | any isRevealedMine $ concat cellStates = Dead
          | numRevealedCells + totalMines config == numTotalCells = Win
          | otherwise = Playing

      in GlobalGameState numRemainingMines numFreeCells playState
