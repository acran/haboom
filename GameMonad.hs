module GameMonad where

import Control.Lens (ix, over)

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
