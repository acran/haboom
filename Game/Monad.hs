module Game.Monad where

import Control.Lens (ix, over)
import Data.Function ((&))

import Game.Types

-- | Monad to evaluate game a move in
newtype GameMonad a = GameMonad {
  runState :: GameState -> (a, GameState)
}

-- standard implementations of a State Monad
instance Functor GameMonad where
  fmap f mx = mx >>= return . f

instance Applicative GameMonad where
  pure = return
  mf <*> mx = mf >>= (\f -> mx >>= return . f)

instance Monad GameMonad where
  return x = GameMonad $ \s -> (x, s)
  GameMonad x >>= f =
    GameMonad $ \s0 -> let
      (val_x, s1)      = x s0
      (GameMonad cont) = f val_x
    in cont s1

-- | evaluate an action on a GameState and return new state
execState ::
     GameMonad a -- ^action to be executed
  -> GameState   -- ^current state of the game
  -> GameState   -- ^game state after the action
execState action state = snd $ runState action state

-- | same as 'execState' but save previous state for undo operations
execStateWithUndo :: GameMonad a -> GameState -> GameState
execStateWithUndo action state
    | result == state = result
    | otherwise       = result {previousState = Just state}
  where
    result = execState action state

-- | get current 'GameState'
--
-- /for internal use only/
get' :: GameMonad GameState
get' = GameMonad $ \s -> (s, s)

-- | get the 'GameConfig' of the current state
getConfig :: GameMonad GameConfig
getConfig = gameConfig <$> get'

-- | get the 'GlobalGameState' of the current state
getGlobalState :: GameMonad GlobalGameState
getGlobalState = globalGameState <$> get'

-- | get the 'CellStates' of the current state
getCells :: GameMonad CellStates
getCells = cells <$> get'

-- | get the 'CellState' of the cell at given coordinate
getCell :: BoardCoordinate -> GameMonad CellState
getCell coordinates = cellFromState coordinates <$> getCells

-- | update the 'CellState' at the given coordinate
--
--   this will also automatically update the 'GlobalGameState'
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
        numFixedMines     = countInState isMine cellStates
        numRemainingMines = totalMines config - numFixedMines
        numRemainingCells = countInState isUndefined cellStates
        numFreeCells      = numRemainingCells - numRemainingMines

        isRevealedMine cell = isMine cell && isKnown cell
        numTotalCells       = boardHeight config * boardWidth config
        numRevealed         = countInState isKnown cellStates
        total               = totalMines config
        playState
          | any isRevealedMine $ concat cellStates = Dead
          | numRevealed + total == numTotalCells   = Win
          | otherwise                              = Playing

      in GlobalGameState numRemainingMines numFreeCells playState
