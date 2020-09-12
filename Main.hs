{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom

import Game.Logic
import Game.Types
import Gui.Layout

defaultConfig :: GameConfig
defaultConfig = GameConfig 10 10 20

main :: IO ()
main = mainWidgetWithHead headElement $ do
  rec
    nestedGameConfigEvent <- dyn $ flip fmap dynGameConfig $ \gameConfig -> do
      rec
        let initialState = newGame gameConfig
        dynGameState <- foldDyn execAction initialState actionEvent

        (gameConfigEvent, actionEvent) <- bodyElement gameConfig dynGameState

      return gameConfigEvent

    gameConfigEvent <- switchHold never nestedGameConfigEvent
    dynGameConfig <- holdDyn defaultConfig gameConfigEvent
  return ()
