{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gui where

import Control.Lens ((%~))
import Data.Map (Map, fromList)
import Data.Maybe (isJust)
import Data.Proxy ( Proxy(..) )
import Data.Text (pack, Text)
import Data.Text.Read (decimal)
import Reflex.Dom hiding (Safe)

import Types

data DisplaySettings = DisplaySettings {
  debugMode :: Bool,
  countdownMode :: Bool
}

headElement :: MonadWidget t m => m ()
headElement = do
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width") blank
  el "title" $ text "Haboom"
  addStyleSheet "css/bootstrap.min.css"
  addStyleSheet "css/style.css"

addStyleSheet :: DomBuilder t m => Text -> m ()
addStyleSheet uri = elAttr "link" styleSheetAttr blank
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
    actionEvent <- divClass "card overflow-auto" $
      divClass "card-body" $ do
        actionEvent <- divClass "board" $
          boardDiv (boardHeight gameConfig) (boardWidth gameConfig) dynGameState dynDisplaySettings

        el "div" $
          dyn $ statusText gameConfig <$> dynGameState

        return actionEvent

    (gameConfigEvent, dynDisplaySettings, undoEvent) <- divClass "row" $ do
      gameConfigEvent <- divClass "col-lg-3 col-md-6 mt-2" $
        controlsDiv gameConfig

      presetEvent <- divClass "col-lg-3 col-md-6 mt-2"
        presetsDiv

      (dynDisplaySettings, undoEvent) <- divClass "col-lg-3 col-md-6 mt-2" $ do
        dynDisplaySettings <- settingsDiv
        undoEvent <- undoButton dynGameState
        return (dynDisplaySettings, undoEvent)

      return (leftmost [gameConfigEvent, presetEvent], dynDisplaySettings, undoEvent)

  return (gameConfigEvent, leftmost [actionEvent, undoEvent])

statusText :: DomBuilder t m => GameConfig -> GameState -> m ()
statusText gameConfig gameState = el "div" $
    showStatus $ playState $ globalState gameState
  where
    flaggedCells = countInState isFlagged $ cells gameState
    showStatus Playing = text $ pack $ "Mines: " ++ show flaggedCells ++ "/" ++ show (totalMines gameConfig)
    showStatus Win = text $ "You win!"
    showStatus Dead = text $ "You lose!"

controlsDiv :: MonadWidget t m => GameConfig -> m (Event t GameConfig)
controlsDiv defaultConfig = do
  (formElement, config) <- formEl' $ do
    el "div" $
      elAttr "button" ("class" =: "btn btn-primary w-100" <> "type" =: "submit")
        $ text "New game"

    widthInput <- numberInput "width" $ boardWidth defaultConfig
    heightInput <- numberInput "height" $ boardHeight defaultConfig
    minesInput <- numberInput "mines" $ totalMines defaultConfig

    return $ GameConfig <$> widthInput <*> heightInput <*> minesInput

  return $ tagPromptlyDyn config $ domEvent Submit formElement

formEl' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
formEl' c = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Submit (const preventDefault)
  element "form" cfg c

presetsDiv :: MonadWidget t m =>  m (Event t GameConfig)
presetsDiv = do
    let presets = [
            ("I'm too young to die", GameConfig 8 8 8),
            ("Hey, not too rough", GameConfig 10 10 20),
            ("Hurt me plenty", GameConfig 15 15 50),
            ("Ultra-Violence", GameConfig 10 10 50)
          ]
    events <- sequence $ presetButton <$> presets
    return $ leftmost events

presetButton :: MonadWidget t m => (Text, GameConfig) -> m (Event t GameConfig)
presetButton (label, config) = do
  (buttonElement, _) <- elClass' "button" "btn btn-light w-100 mb-2" $ text label
  return $ config <$ domEvent Click buttonElement

undoButton :: MonadWidget t m => Dynamic t GameState -> m (Event t Action)
undoButton gameState = do
    let dynAttr = attr <$> gameState
    (buttonElement, _) <- elDynAttr' "button" dynAttr $ text "Undo"
    return $ Undo <$ domEvent Click buttonElement
  where
    attr state
      | isJust (previousState state) && Win /= playState (globalState state) = fromList [("class", "btn btn-light btn-sm mt-2")]
      | otherwise  = fromList [
        ("class", "btn btn-light btn-sm mt-2"),
        ("disabled", "disabled")
      ]

settingsDiv :: MonadWidget t m => m (Dynamic t DisplaySettings)
settingsDiv = do
  debugMode <- el "div" $
    el "label" $ do
      debugMode <- checkbox False def
      text " Debug mode"

      return $ value debugMode

  countdownMode <- el "div" $
    el "label" $ do
      countdownMode <- checkbox False def
      text " Countdown mode "
      elAttr "abbr"
        ("title" =: "Instead of showing the total number of mines around a tile, show the remaining (based on placed flags)") $
          text "(?)"

      return $ value countdownMode

  return $ DisplaySettings <$> debugMode <*> countdownMode

numberInput :: MonadWidget t m => Text -> Int -> m (Dynamic t Int)
numberInput label defValue = divClass "input-group mt-2" $ do
    divClass "input-group-prepend" $
      divClass "input-group-text" $
        text label

    inputElement <- textInput $
      def & textInputConfig_inputType .~ "number"
        & textInputConfig_initialValue .~ (pack . show $ defValue)
        & textInputConfig_attributes .~ constDyn ("class" =: "form-control")

    return $ parseInt <$> value inputElement
  where
    parseInt = getValue . decimal
    getValue (Right (value, _)) = value
    getValue _ = defValue

boardDiv :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> Dynamic t DisplaySettings -> m (Event t Action)
boardDiv rows columns dynState dynDisplaySettings = do
    events <- sequence $ generateRow columns <$> [0 .. rows - 1]
    return $ leftmost events
  where
    generateRow = generateBoardRow dynState dynDisplaySettings

generateBoardRow :: MonadWidget t m => Dynamic t GameState -> Dynamic t DisplaySettings -> Int -> Int -> m (Event t Action)
generateBoardRow gameState dynDisplaySettings width row = divClass "board-row" $ do
    events <- sequence $ generateCell row <$> [0 .. width - 1]
    return $ leftmost events
  where
    generateCell = generateBoardCell gameState dynDisplaySettings

generateBoardCell :: MonadWidget t m => Dynamic t GameState -> Dynamic t DisplaySettings -> Int -> Int -> m (Event t Action)
generateBoardCell dynState dynDisplaySettings row column = do
    let dynCellState = cellFromState (BoardCoordinate column row) . cells <$> dynState
    let dynPlayState = playState . globalState <$> dynState

    let dynAttr = classAttr <$> dynDisplaySettings <*> dynCellState <*> dynPlayState

    (cellElement, _) <- cellElement dynAttr

    let revealAction = Reveal (BoardCoordinate column row) <$ domEvent Click cellElement
    let revealAreaAction = RevealArea (BoardCoordinate column row) <$ domEvent Dblclick cellElement
    let toggleAction = ToggleFlag (BoardCoordinate column row) <$ domEvent Contextmenu cellElement

    return $ leftmost [revealAction, revealAreaAction, toggleAction]
  where
    classAttr settings cell@(CellState internalState visibleState) status =
        fromList [("class", pack $ "cell" ++ clickable ++ visibility ++ label ++ flag ++ hint)]
      where
        known = isKnown cell

        clickable
          | Playing <- status, not known, visibleState == Unknown || visibleState == Unsure = " clickable"
          | otherwise = ""

        visibility
          | known = " known"
          | otherwise = " unknown"

        label
          | Mine <- internalState, known || status == Dead = " bomb"
          | Win <- status, Mine <- internalState = " bomb-win"
          | settings & countdownMode, Labeled total x <- visibleState, total /= 0 = " label-" ++ show x
          | Labeled x _ <- visibleState, x /=0 = " label-" ++ show x
          | otherwise = ""

        flag
          | status /= Playing, Mine <- internalState = ""
          | Flagged <- visibleState = " flag"
          | Unsure <- visibleState = " unsure"
          | otherwise = ""

        hint
          | settings & debugMode, not known, Mine <- internalState = " hint hint-mine"
          | settings & debugMode, not known, Safe <- internalState = " hint hint-safe"
          | otherwise = ""

cellElement :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t (Map Text Text) -> m (Element EventResult (DomBuilderSpace m) t, ())
cellElement attrs = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Contextmenu (const preventDefault)

  element "div" cfg blank
