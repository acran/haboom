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

  el "h2" $ text "What is this?"
  el "p" $
    text "This is a procedurally generated Minesweeper game written in Haskell:"
  el "ul" $ do
    el "li" $ text "The mines are not placed at the beginning, but determined as you play."
    el "li" $ text "Only when revealing a cell the state for it and the surrounding cells is fixed."
    el "li" $ text "If you open a cell which state is not fixed yet it will be safe..."
    el "li" $ text "...unless there are no other un-fixed cells left to place the remaining floating mines"

  el "h2" $ text "See also"
  el "p" $ do
    text "This project is heavily based on and uses assets from:"
    el "ul" $
      el "li" $ do
        linkEl "https://github.com/pwmarcz/kaboom" $
          el "strong" $ text "Kaboom"
        text " - a "
        el "strong" $ text "cruel, but fair"
        text " Minesweeper game by Paweł Marczewski ("
        linkEl "https://pwmarcz.pl/" $ text "pwmarcz.pl"
        text $ ")"

  return (gameConfigEvent, leftmost [actionEvent, undoEvent])
