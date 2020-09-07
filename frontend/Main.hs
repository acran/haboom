{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom

import GameLogic
import Gui
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
