{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gui where

import Control.Lens ((%~))
import Data.Map (Map, fromList)
import Data.Proxy ( Proxy(..) )
import Data.Text (pack, Text)
import Data.Text.Read (decimal)
import Reflex.Dom hiding (Safe)

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

  rec
    actionEvent <- divClass "card m-2" $ do
      actionEvent <- divClass "row justify-content-center p-3" $
        divClass "board" $
          boardDiv gameConfig dynGameState dynDebugMode

      divClass "row justify-content-center" $
        dyn $ statusText gameConfig <$> dynGameState

      return actionEvent

    (gameConfigEvent, dynDebugMode) <- divClass "controls row justify-content-center" $ do
      gameConfigEvent <- divClass "col-lg-3 col-md-6 mt-2" $
        controlsDiv gameConfig
      dynDebugMode <- divClass "col-lg-3 col-md-6 mt-2"
        tweaksDiv
      return (gameConfigEvent, dynDebugMode)

  return (gameConfigEvent, actionEvent)

statusText :: DomBuilder t m => GameConfig -> GameState -> m ()
statusText gameConfig gameState = el "div" $
    text $ pack $ "Mines: " ++ show flaggedCells ++ "/" ++ show (getNumMines gameConfig)
  where flaggedCells = foldr ((+) . (fromEnum . isFlagged)) 0 $ concat gameState

controlsDiv :: MonadWidget t m => GameConfig -> m (Event t GameConfig)
controlsDiv defaultConfig = do
  (buttonElement, _) <- el' "div" $ elClass "button" "btn btn-primary w-100" $ text "New game"

  widthInput <- numberInput "width" $ getBoardWidth defaultConfig
  heightInput <- numberInput "height" $ getBoardHeight defaultConfig
  minesInput <- numberInput "mines" $ getNumMines defaultConfig

  let config = GameConfig <$> widthInput <*> heightInput <*> minesInput
  let clickEvent = domEvent Click buttonElement

  return $ tagPromptlyDyn config clickEvent

tweaksDiv :: MonadWidget t m => m (Dynamic t Bool)
tweaksDiv = el "div" $
  el "label" $ do
    debugMode <- checkbox False def
    text "Debug mode"
    return $ value debugMode

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

boardDiv :: MonadWidget t m => GameConfig -> Dynamic t GameState -> Dynamic t Bool -> m (Event t Action)
boardDiv gameConfig dynGameState dynDebugMode = do
  events <- sequence [generateBoardRow x (getBoardWidth gameConfig) dynGameState dynDebugMode | x <- [0 .. (getBoardHeight gameConfig - 1)]]
  let actionEvent = leftmost events
  return actionEvent

generateBoardRow :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> Dynamic t Bool -> m (Event t Action)
generateBoardRow row width gameState dynDebugMode = divClass "board-row" $ do
  events <- sequence [generateBoardCell row y gameState dynDebugMode| y <- [0 .. (width - 1)]]
  return $ leftmost events

generateBoardCell :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> Dynamic t Bool -> m (Event t Action)
generateBoardCell row column dynGameState dynDebugMode = do
    let dynCellState = getCellState <$> dynGameState
    let dynClass = getDynClass <$> dynDebugMode <*> dynCellState

    let dynAttr = classToAttr <$> dynClass

    (cellElement, _) <- cellElement Nothing "div" dynAttr blank

    let revealAction = Reveal (BoardCoordinate column row) <$ domEvent Click cellElement
    let toggleAction = ToggleFlag (BoardCoordinate column row) <$ domEvent Contextmenu cellElement

    return $ leftmost [revealAction, toggleAction]
  where
    classToAttr classString = fromList [("class", classString)]

    getCellState gameState = gameState !! row !! column

    getDynClass debugMode (CellState internalState visibleState) =
        pack $ "cell" ++ clickable ++ visibility ++ label ++ flag ++ hint
      where
        clickable | not revealed, Unknown <- visibleState = " clickable"
                  | not revealed, Unsure <- visibleState = " clickable"
                  | otherwise = ""

        visibility | revealed = " known"
                   | otherwise = " unknown"

        label | revealed, Mine <- internalState = " bomb"
              | Labeled x <- visibleState = " label-" ++ show x
              | otherwise = ""

        flag | Flagged <- visibleState = " flag"
             | Unsure <- visibleState = " unsure"
             | otherwise = ""

        hint | debugMode, not revealed, Mine <- internalState = " hint hint-mine"
             | debugMode, not revealed, Safe <- internalState = " hint hint-safe"
             | otherwise = ""

        revealed | Known <- visibleState = True
                 | Labeled _ <- visibleState = True
                 | otherwise = False

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
