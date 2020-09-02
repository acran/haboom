{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Map (fromList)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Reflex.Dom

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

numberInput :: MonadWidget t m => Text -> Integer -> m (Dynamic t Integer)
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
      sequence [generateBoardRow x $ getBoardWidth gameConfig | x <- [1 .. getBoardHeight gameConfig]]
      return ()

generateBoardRow :: MonadWidget t m => Integer -> Integer -> m ()
generateBoardRow row width = divClass "board-row" $ do
  sequence [generateBoardCell row y | y <- [1 .. width]]
  return ()

generateBoardCell :: MonadWidget t m => Integer -> Integer -> m ()
generateBoardCell row column = do
  rec (cellElement, _) <- elDynClass' "div" dynClass blank
      clickEvent <- return $ domEvent Click cellElement
      toggleEvent <- toggle False clickEvent
      let dynClass = cellClass <$> toggleEvent
  return ()
  where
    cellClass True = "cell known"
    cellClass False = "cell clickable unknown"
