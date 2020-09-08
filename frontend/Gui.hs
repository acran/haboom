{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gui where

import Control.Lens ((%~))
import Data.Map (Map, fromList)
import Data.Proxy ( Proxy(..) )
import Data.Text (pack, Text)
import Data.Text.Read (decimal)
import Reflex.Dom

import Types

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

bodyElement :: MonadWidget t m => GameConfig -> Dynamic t GameState -> m (Event t GameConfig, Event t Action)
bodyElement gameConfig dynGameState = divClass "container" $ do
  el "h1" $ text "Haboom"

  actionEvent <- boardDiv gameConfig dynGameState
  gameConfigEvent <- controlsDiv gameConfig

  return (gameConfigEvent, actionEvent)

controlsDiv :: MonadWidget t m => GameConfig -> m (Event t GameConfig)
controlsDiv defaultConfig = divClass "controls row justify-content-center" $
  divClass "col-md-6 mt-2" $ do
    (buttonElement, _) <- el' "div" $ elClass "button" "btn btn-primary w-100" $ text "New game"

    widthInput <- numberInput "width" $ getBoardWidth defaultConfig
    heightInput <- numberInput "height" $ getBoardHeight defaultConfig
    minesInput <- numberInput "mines" $ getNumMines defaultConfig

    let config = GameConfig <$> widthInput <*> heightInput <*> minesInput
    let clickEvent = domEvent Click buttonElement

    return $ tagPromptlyDyn config clickEvent

numberInput :: MonadWidget t m => Text -> Int -> m (Dynamic t Int)
numberInput label defValue = divClass "input-group mt-2" $ do
  divClass "input-group-prepend" $ divClass "input-group-text" $ text label
  inputElement <- textInput $
    def & textInputConfig_inputType .~ "number"
      & textInputConfig_initialValue .~ (pack . show $ defValue)
      & textInputConfig_attributes .~ constDyn ("class" =: "form-control")
  return $ parseInt <$> value inputElement
  where
    parseInt = getValue . decimal
      where
        getValue (Right (value, _)) = value
        getValue _ = defValue

boardDiv :: MonadWidget t m => GameConfig -> Dynamic t GameState -> m (Event t Action)
boardDiv gameConfig dynGameState = divClass "card m-2" $
  divClass "row justify-content-center p-3" $
    divClass "board" $ do
      events <- sequence [generateBoardRow x (getBoardWidth gameConfig) dynGameState | x <- [0 .. (getBoardHeight gameConfig - 1)]]
      let actionEvent = leftmost events
      return actionEvent

generateBoardRow :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> m (Event t Action)
generateBoardRow row width gameState = divClass "board-row" $ do
  events <- sequence [generateBoardCell row y gameState| y <- [0 .. (width - 1)]]
  return $ leftmost events

generateBoardCell :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> m (Event t Action)
generateBoardCell row column dynGameState = do
    let dynCellState = getCellState <$> dynGameState
    let dynClass = getDynClass <$> dynCellState

    let dynAttr = classToAttr <$> dynClass

    (cellElement, _) <- cellElement Nothing "div" dynAttr blank

    let revealAction = Reveal (BoardCoordinate column row) <$ domEvent Click cellElement
    let toggleAction = ToggleFlag (BoardCoordinate column row) <$ domEvent Contextmenu cellElement

    return $ leftmost [revealAction, toggleAction]
  where
    classToAttr classString = fromList [("class", classString)]

    getCellState gameState = gameState !! row !! column

    getDynClass (CellState _ Unknown) = "cell clickable unknown"
    getDynClass (CellState _ Flagged) = "cell clickable unknown flag"
    getDynClass (CellState _ Unsure) = "cell clickable unknown unsure"

    getDynClass (CellState _ (Labeled num)) = pack $ "cell known label-" ++ show num
    getDynClass (CellState Mine Known) = "cell known bomb"
    getDynClass (CellState _ Known) = "cell known"

-- modified version of elDynAttrNS' to add preventDefault
cellElement :: forall t m a. (DomBuilder t m, PostBuild t m) => Maybe Text -> Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
cellElement mns elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_namespace .~ mns
        & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Contextmenu (const preventDefault)
  result <- element elementTag cfg child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  return result
