{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gui.Utils where

import Control.Lens ((%~))
import Data.Map (fromList, Map)
import Data.Proxy (Proxy(..))
import Data.Text (pack, Text)
import Data.Text.Read (decimal)
import Reflex.Dom

import Game.Types

-- | add a css stylesheet by uri
styleSheet :: DomBuilder t m => Text -> m ()
styleSheet uri = elAttr "link" styleSheetAttr blank
  where
    styleSheetAttr = fromList [
        ("rel",  "stylesheet"),
        ("type", "text/css"),
        ("href",  uri)
      ]

-- | create a <a> tag with href
linkEl :: DomBuilder t m => Text -> m a -> m a
linkEl href children = elAttr "a" ("href" =: href) children

-- | create new <form> tag with 'preventDefault' for the submit event
formEl' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
formEl' children = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Submit (const preventDefault)
  element "form" cfg children

-- | create new cell element with dynamic attributes and 'preventDefault' on the Contextmenu event (right click)
elCell' :: forall t m. (DomBuilder t m, PostBuild t m) => Dynamic t (Map Text Text) -> m (Element EventResult (DomBuilderSpace m) t, ())
elCell' attrs = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs

  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Contextmenu (const preventDefault)

  element "div" cfg blank

-- | create a button with preset 'GameConfig'
presetButton :: MonadWidget t m => (Text, GameConfig) -> m (Event t GameConfig)
presetButton (label, config) = do
  (buttonElement, _) <- elClass' "button" "btn btn-light w-100 mb-2" $ text label
  return $ config <$ domEvent Click buttonElement

-- | create a <input type="number"> tag with label returning its value
numberInput :: MonadWidget t m => Text -> Int -> m (Dynamic t Int)
numberInput label initialValue = divClass "input-group mt-2" $ do
    divClass "input-group-prepend" $
      divClass "input-group-text" $
        text label

    input <- textInput $
      def & textInputConfig_inputType    .~ "number"
          & textInputConfig_initialValue .~ (pack . show $ initialValue)
          & textInputConfig_attributes   .~ constDyn ("class" =: "form-control")

    return $ parseInt <$> value input
  where
    parseInt = getValue . decimal

    getValue (Right (textValue, _)) = textValue
    getValue _                      = initialValue
