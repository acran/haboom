{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens ((%~), over, ix, view)
import Data.Map (Map, fromList)
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Reflex.Dom hiding (Safe)

import Types

main :: IO ()
main = mainWidgetWithHead headElement $ do
  let defaultConfig = GameConfig 10 10 20
  rec
    evGameConfigEvent <- dyn $ flip fmap dynGameConfig $ \gameConfig -> do
      rec
        let initialState = initializeCellStates (getBoardWidth gameConfig) (getBoardHeight gameConfig) (getNumMines gameConfig)
        dynGameState <- foldDyn (updateCell gameConfig) initialState actionEvent

        (gameConfigEvent, actionEvent) <- bodyElement gameConfig dynGameState

      return gameConfigEvent

    gameConfigEvent <- switchHold never evGameConfigEvent
    dynGameConfig <- holdDyn defaultConfig gameConfigEvent
  return ()

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
controlsDiv defaultConfig = divClass "controls row justify-content-center" $ do
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
  return $ parseInt <$> (value inputElement)
  where
    parseInt = getValue . decimal
      where
        getValue (Right (value, _)) = value
        getValue _ = defValue

boardDiv :: MonadWidget t m => GameConfig -> Dynamic t GameState -> m (Event t Action)
boardDiv gameConfig dynGameState = divClass "card m-2" $ do
  divClass "row justify-content-center p-3" $ do
    divClass "board" $ do
      events <- sequence [generateBoardRow x (getBoardWidth gameConfig) dynGameState | x <- [0 .. ((getBoardHeight gameConfig) - 1)]]
      let actionEvent = leftmost events
      return actionEvent

updateCell :: GameConfig -> Action -> GameState -> GameState
updateCell gameConfig (Reveal (BoardCoordinate x y)) (state :: GameState) = over (ix y) (over (ix x) (reveal $ getNeighbors (getBoardWidth gameConfig) (getBoardHeight gameConfig) x y state)) state
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

generateBoardRow :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> m (Event t Action)
generateBoardRow row width gameState = divClass "board-row" $ do
  events <- sequence [generateBoardCell row y gameState| y <- [0 .. (width - 1)]]
  return $ leftmost events

generateBoardCell :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> m (Event t Action)
generateBoardCell row column dynGameState = do
    let dynCellState = getCellState <$> dynGameState
    let dynClass = getDynClass <$> dynCellState

    let dynAttr = classToAttr <$> dynClass

    (cellElement, _) <- cellElement Nothing "div" dynAttr $ blank

    let revealAction = const (Reveal (BoardCoordinate column row)) <$> domEvent Click cellElement
    let toggleAction = const (ToggleFlag (BoardCoordinate column row)) <$> domEvent Contextmenu cellElement


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
