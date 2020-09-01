{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Map (fromList)
import Data.Text (Text, pack)
import Reflex.Dom

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
  el "h1" $ text "Haboom"
  boardDiv
  controlsDiv

controlsDiv :: MonadWidget t m => m ()
controlsDiv = divClass "controls row justify-content-center" $ do
  divClass "col-md-6 mt-2" $ do
    el "div" $ elClass "button" "btn btn-primary w-100" $ text "New game"
    numberInput "width" 10
    numberInput "height" 10
    numberInput "mines" 20

numberInput :: DomBuilder t m => Text -> Integer -> m ()
numberInput label defValue = divClass "input-group mt-2" $ do
  divClass "input-group-prepend" $ divClass "input-group-text" $ text label
  elAttr "input" inputAttr blank
  where
    inputAttr = "value" =: (pack . show $ defValue)
             <> "type" =: "number"
             <> "class" =: "form-control"

boardDiv :: MonadWidget t m => m ()
boardDiv = divClass "card m-2" $ do
  divClass "row justify-content-center p-3" $ do
    divClass "board" $ do
      divClass "board-row" $ do
        divClass "cell clickable unknown flag" blank
        divClass "cell known label-5" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
      divClass "board-row" $ do
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
      divClass "board-row" $ do
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
      divClass "board-row" $ do
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
      divClass "board-row" $ do
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
      divClass "board-row" $ do
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
        divClass "cell clickable unknown" blank
