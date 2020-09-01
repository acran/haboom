{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
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
  boardDiv 10 10
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

boardDiv :: MonadWidget t m => Integer -> Integer -> m ()
boardDiv width height = divClass "card m-2" $ do
  divClass "row justify-content-center p-3" $ do
    divClass "board" $ do
      sequence [generateBoardRow x width | x <- [1 .. height]]
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
