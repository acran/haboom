module GameLogic where

import Control.Lens (view, ix, over)

import Types

updateCell :: GameConfig -> Action -> GameState -> GameState
updateCell gameConfig (Reveal (BoardCoordinate x y)) state = over (ix y) (over (ix x) (reveal $ getNeighbors (getBoardWidth gameConfig) (getBoardHeight gameConfig) x y state)) state
updateCell _ (ToggleFlag (BoardCoordinate x y)) state = over (ix y) (over (ix x) toggleFlagState) state

reveal :: Foldable t => t CellState -> CellState -> CellState
reveal neighbors cellState = case cellState of
    CellState _ Unknown -> updatedCell cellState
    CellState _ Unsure -> updatedCell cellState
    _ -> cellState
  where
    updatedCell (CellState Mine _) = CellState Mine Known
    updatedCell (CellState Safe _) = CellState Safe newLabel
    newLabel =
      let numMines = foldl (flip ((+) . (fromEnum . isMine))) 0 neighbors
      in case numMines of
        0 -> Known
        num -> Labeled num

toggleFlagState :: CellState -> CellState
toggleFlagState (CellState inner Unknown) = CellState inner Flagged
toggleFlagState (CellState inner Flagged) = CellState inner Unsure
toggleFlagState (CellState inner Unsure) = CellState inner Unknown
toggleFlagState cellState = cellState

getNeighbors :: Int -> Int -> Int -> Int -> GameState -> [CellState]
getNeighbors width height x y state = [
      view (ix nx) $ view (ix ny) state
  | nx <- [x-1 .. x+1],
    ny <- [y-1 .. y+1],

    (nx, ny) /= (x, y),
    nx >= 0,
    nx < width,

    ny >= 0,
    ny < height
  ]

initializeCellStates :: Integral a => a -> a -> a -> GameState
initializeCellStates width height mines = [
      [CellState (calcInternal x y) Unknown | y <- [0 .. (width - 1)]]
    | x <- [0 .. (height - 1)]]
  where
    calcInternal x y =
      let step = width * height `quot` mines
          cellNumber = x * width + y
          (cellDiv, cellMod) = cellNumber `divMod` step
      in case (cellMod, cellDiv < mines) of
        (0, True)-> Mine
        _ -> Safe
