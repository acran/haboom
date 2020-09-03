{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (over, ix, view)
import Data.Map (fromList)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Reflex.Dom hiding (Safe)

import Types

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Haboom"
  addStyleSheet "css/bootstrap.min.css"
  addStyleSheet "css/style.css"

addStyleSheet :: DomBuilder t m => Text -> m ()
addStyleSheet uri = elAttr "link" styleSheetAttr $ return ()
  where
    styleSheetAttr = fromList [
        ("rel", "stylesheet"),
        ("type", "text/css"),
        ("href", uri)
      ]

bodyElement :: MonadWidget t m => m ()
bodyElement = divClass "container" $ do
  rec
    el "h1" $ text "Haboom"

    dyn $ boardDiv <$> dynGameConfig
    dynGameConfig <- controlsDiv
  return ()

controlsDiv :: MonadWidget t m => m (Dynamic t GameConfig)
controlsDiv = divClass "controls row justify-content-center" $ do
  divClass "col-md-6 mt-2" $ do
    (buttonElement, _) <- el' "div" $ elClass "button" "btn btn-primary w-100" $ text "New game"

    let defaultConfig = GameConfig 10 10 20

    widthInput <- numberInput "width" $ getBoardWidth defaultConfig
    heightInput <- numberInput "height" $ getBoardHeight defaultConfig
    minesInput <- numberInput "mines" $ getNumMines defaultConfig

    let config = GameConfig <$> widthInput <*> heightInput <*> minesInput
    let clickEvent = domEvent Click buttonElement
    let configStream = tagPromptlyDyn config clickEvent

    holdDyn defaultConfig configStream

numberInput :: MonadWidget t m => Text -> Int -> m (Dynamic t Int)
numberInput label defValue = divClass "input-group mt-2" $ do
  divClass "input-group-prepend" $ divClass "input-group-text" $ text label
  inputElement <- textInput $
    def & textInputConfig_inputType .~ "number"
      & textInputConfig_initialValue .~ (pack . show $ defValue)
      & textInputConfig_attributes .~ constDyn ("class" =: "form-control")
  return $ parseInt <$> (value inputElement)
  where
    parseInt = getValue . decimal
      where
        getValue (Right (value, _)) = value
        getValue _ = defValue

boardDiv :: MonadWidget t m => GameConfig -> m ()
boardDiv gameConfig = divClass "card m-2" $ do
  divClass "row justify-content-center p-3" $ do
    divClass "board" $ do
      rec
        let gameState = initializeCellStates (getBoardWidth gameConfig) (getBoardHeight gameConfig) (getNumMines gameConfig)
        dynGameState <- foldDyn (updateCell reveal) gameState clickEvent
        events <- sequence [generateBoardRow x (getBoardWidth gameConfig) dynGameState | x <- [0 .. ((getBoardHeight gameConfig) - 1)]]
        let clickEvent = leftmost events
      return ()
      where
        updateCell update (BoardCoordinate x y) (state :: GameState) = over (ix x) (over (ix y) update) state
        reveal (CellState inner _) = CellState inner Known

initializeCellStates :: Integral a => a -> a -> a -> GameState
initializeCellStates width height mines = [
      [CellState (calcInternal x y) Unknown | y <- [0 .. (width - 1)]]
    | x <- [0 .. (height - 1)]]
  where
    calcInternal x y =
      let step = width * height `quot` mines
          cellNumber = x * width + y
      in case cellNumber `mod` step of
        0 -> Mine
        _ -> Safe

generateBoardRow :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> m (Event t BoardCoordinate)
generateBoardRow row width gameState = divClass "board-row" $ do
  events <- sequence [generateBoardCell row y gameState| y <- [0 .. (width - 1)]]
  return $ leftmost events

generateBoardCell :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> m (Event t BoardCoordinate)
generateBoardCell row column dynGameState = do
    let dynCellState = getCellState <$> dynGameState
    let dynClass = getDynClass <$> dynCellState
    (cellElement, _) <- elDynClass' "div" dynClass blank
    return $ const (BoardCoordinate row column) <$> domEvent Click cellElement
  where
    getCellState gameState = gameState !! row !! column

    getDynClass (CellState _ Unknown) = "cell clickable unknown"
    getDynClass (CellState Mine Known) = "cell known bomb"
    getDynClass (CellState _ Known) = "cell known"
