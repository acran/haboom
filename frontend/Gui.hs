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
          boardDiv gameConfig dynGameState dynDebugMode dynCountdownMode

      divClass "row justify-content-center" $
        dyn $ statusText gameConfig <$> dynGameState

      return actionEvent

    (gameConfigEvent, dynDebugMode, dynCountdownMode, undoEvent) <- divClass "controls row justify-content-center" $ do
      gameConfigEvent <- divClass "col-lg-3 col-md-6 mt-2" $
        controlsDiv gameConfig
      presetEvent <- divClass "col-lg-3 col-md-6 mt-2"
        presetsDiv
      (dynDebugMode, dynCountdownMode, undoEvent) <- divClass "col-lg-3 col-md-6 mt-2" $ do
        (dynDebugMode, dynCountdownMode) <- tweaksDiv
        undoEvent <- undoButton dynGameState
        return (dynDebugMode, dynCountdownMode, undoEvent)

      return (leftmost [gameConfigEvent, presetEvent], dynDebugMode, dynCountdownMode, undoEvent)

  return (gameConfigEvent, leftmost [actionEvent, undoEvent])

statusText :: DomBuilder t m => GameConfig -> GameState -> m ()
statusText gameConfig gameState = el "div" $
    showStatus $ gameStatus $ getCache gameState
  where
    flaggedCells = foldr ((+) . (fromEnum . isFlagged)) 0 $ concat $ getCells gameState
    showStatus Playing = text $ pack $ "Mines: " ++ show flaggedCells ++ "/" ++ show (getNumMines gameConfig)
    showStatus Won = text $ "You win!"
    showStatus Lost = text $ "You lose!"

controlsDiv :: MonadWidget t m => GameConfig -> m (Event t GameConfig)
controlsDiv defaultConfig = do
  (buttonElement, _) <- el' "div" $ elClass "button" "btn btn-primary w-100" $ text "New game"

  widthInput <- numberInput "width" $ getBoardWidth defaultConfig
  heightInput <- numberInput "height" $ getBoardHeight defaultConfig
  minesInput <- numberInput "mines" $ getNumMines defaultConfig

  let config = GameConfig <$> widthInput <*> heightInput <*> minesInput
  let clickEvent = domEvent Click buttonElement

  return $ tagPromptlyDyn config clickEvent

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
  let clickEvent = domEvent Click buttonElement
  return $ tagPromptlyDyn (constDyn config) clickEvent

undoButton :: MonadWidget t m => Dynamic t GameState -> m (Event t Action)
undoButton gameState = do
    let dynAttr = attr <$> gameState
    (buttonElement, _) <- elDynAttr' "button" dynAttr $ text "Undo"
    return $ Undo <$ domEvent Click buttonElement
  where
    attr (GameState (Just _) _ (StateCache _ _ Playing)) = fromList [("class", "btn btn-light btn-sm mt-2")]
    attr (GameState (Just _) _ (StateCache _ _ Lost)) = fromList [("class", "btn btn-light btn-sm mt-2")]
    attr _ = fromList [
        ("class", "btn btn-light btn-sm mt-2"),
        ("disabled", "disabled")
      ]

tweaksDiv :: MonadWidget t m => m (Dynamic t Bool, Dynamic t Bool)
tweaksDiv = el "div" $ do
  debugMode <- el "label" $ do
    debugMode <- checkbox False def
    text "Debug mode"
    return $ value debugMode
  countdownMode <- el "label" $ do
    countdownMode <- checkbox False def
    text "Countdown mode "
    elAttr "abbr" ("title" =: "Instead of showing the total number of mines around a tile, show the remaining (based on placed flags)") $ text "(?)"
    return $ value countdownMode

  return (debugMode, countdownMode)

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

boardDiv :: MonadWidget t m => GameConfig -> Dynamic t GameState -> Dynamic t Bool -> Dynamic t Bool -> m (Event t Action)
boardDiv gameConfig dynGameState dynDebugMode dynCountdownMode = do
  events <- sequence [generateBoardRow x (getBoardWidth gameConfig) dynGameState dynDebugMode dynCountdownMode | x <- [0 .. (getBoardHeight gameConfig - 1)]]
  let actionEvent = leftmost events
  return actionEvent

generateBoardRow :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> Dynamic t Bool -> Dynamic t Bool -> m (Event t Action)
generateBoardRow row width gameState dynDebugMode dynCountdownMode = divClass "board-row" $ do
  events <- sequence [generateBoardCell row y gameState dynDebugMode dynCountdownMode | y <- [0 .. (width - 1)]]
  return $ leftmost events

generateBoardCell :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> Dynamic t Bool -> Dynamic t Bool -> m (Event t Action)
generateBoardCell row column dynGameState dynDebugMode dynCountdownMode= do
    let dynCellState = getCellState . getCells <$> dynGameState
    let dynGameStatus = gameStatus . getCache <$> dynGameState
    let dynClass = getDynClass <$> dynDebugMode <*> dynCountdownMode <*> dynCellState <*> dynGameStatus

    let dynAttr = classToAttr <$> dynClass

    (cellElement, _) <- cellElement Nothing "div" dynAttr blank

    let revealAction = Reveal (BoardCoordinate column row) <$ domEvent Click cellElement
    let toggleAction = ToggleFlag (BoardCoordinate column row) <$ domEvent Contextmenu cellElement

    return $ leftmost [revealAction, toggleAction]
  where
    classToAttr classString = fromList [("class", classString)]

    getCellState gameState = gameState !! row !! column

    getDynClass debugMode countdownMode (CellState internalState visibleState) status =
        pack $ "cell" ++ clickable ++ visibility ++ label ++ flag ++ hint
      where
        clickable | Playing <- status, not revealed, Unknown <- visibleState = " clickable"
                  | Playing <- status, not revealed, Unsure <- visibleState = " clickable"
                  | otherwise = ""

        visibility | revealed = " known"
                   | otherwise = " unknown"

        label | revealed, Mine <- internalState = " bomb"
              | Lost <- status, Mine <- internalState = " bomb"
              | Won <- status, Mine <- internalState = " bomb-win"
              | Labeled 0 _ <- visibleState = ""
              | countdownMode, Labeled _ x <- visibleState = " label-" ++ show x
              | Labeled x _ <- visibleState = " label-" ++ show x
              | otherwise = ""

        flag | status /= Playing, Mine <- internalState = ""
             | Flagged <- visibleState = " flag"
             | Unsure <- visibleState = " unsure"
             | otherwise = ""

        hint | debugMode, not revealed, Mine <- internalState = " hint hint-mine"
             | debugMode, not revealed, Safe <- internalState = " hint hint-safe"
             | otherwise = ""

        revealed | Known <- visibleState = True
                 | Labeled _ _<- visibleState = True
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
