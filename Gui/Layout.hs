{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Gui.Layout where

import Reflex.Dom

import Game.Types
import Gui.Board
import Gui.Controls
import Gui.Utils

headElement :: MonadWidget t m => m ()
headElement = do
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width") blank

  el "title" $ text "Haboom"

  styleSheet "css/bootstrap.min.css"
  styleSheet "css/style.css"

bodyElement :: MonadWidget t m => GameConfig -> Dynamic t GameState -> m (Event t GameConfig, Event t GameAction)
bodyElement config dynGameState = divClass "container" $ do
  el "h1" $ text "Haboom"

  rec
    actionEvent <- divClass "card overflow-auto" $
      divClass "card-body" $ do
        actionEvent <- divClass "board" $
          boardDiv (boardHeight config) (boardWidth config) dynGameState dynDisplaySettings

        _ <- el' "div" $
          dyn $ statusText config <$> dynGameState

        return actionEvent

    (gameConfigEvent, dynDisplaySettings, undoEvent) <- divClass "row" $ do
      gameConfigEvent <- divClass "col-lg-3 col-md-6 mt-2" $
        controlsDiv config

      presetEvent <- divClass "col-lg-3 col-md-6 mt-2"
        presetsDiv

      (dynDisplaySettings, undoEvent) <- divClass "col-lg-3 col-md-6 mt-2" $ do
        dynDisplaySettings <- settingsDiv
        undoEvent          <- undoButton dynGameState

        return (dynDisplaySettings, undoEvent)

      return (leftmost [gameConfigEvent, presetEvent], dynDisplaySettings, undoEvent)

  return (gameConfigEvent, leftmost [actionEvent, undoEvent])
