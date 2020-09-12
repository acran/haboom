{-# LANGUAGE OverloadedStrings #-}

module Gui.Controls where

import Data.Map (fromList)
import Data.Maybe (isJust)
import Reflex.Dom

import Game.Types
import Gui.Types
import Gui.Utils

controlsDiv :: MonadWidget t m => GameConfig -> m (Event t GameConfig)
controlsDiv defaultConfig = do
  (formElement, config) <- formEl' $ do
    el "div" $
      elAttr "button" ("class" =: "btn btn-primary w-100" <> "type" =: "submit")
        $ text "New game"

    width <- numberInput "width" $ boardWidth defaultConfig
    height <- numberInput "height" $ boardHeight defaultConfig
    mines <- numberInput "mines" $ totalMines defaultConfig

    return $ GameConfig <$> width <*> height <*> mines

  return $ tagPromptlyDyn config $ domEvent Submit formElement

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
  dynDebugMode <- el "div" $
    el "label" $ do
      debugModeBox <- checkbox False def
      text " Debug mode"

      return $ value debugModeBox

  dynCountdownMode <- el "div" $
    el "label" $ do
      countdownModeBox <- checkbox False def
      text " Countdown mode "
      elAttr "abbr"
        ("title" =: "Instead of showing the total number of mines around a tile, show the remaining (based on placed flags)") $
          text "(?)"

      return $ value countdownModeBox

  return $ DisplaySettings <$> dynDebugMode <*> dynCountdownMode
