{-# LANGUAGE OverloadedStrings #-}

module Gui.Board where

import Data.Map (fromList)
import Data.Text (pack)
import Reflex.Dom hiding (Safe)

import Game.Types
import Gui.Utils
import Gui.Types

statusText :: DomBuilder t m => GameConfig -> GameState -> m ()
statusText config state = el "div" $
    showStatus $ state & globalState & playState
  where
    flags = countInState isFlagged $ cells state
    mines = config & totalMines
    showStatus Playing = text $ pack $ "Mines: " ++ show flags ++ "/" ++ show mines
    showStatus Win = text $ "You win!"
    showStatus Dead = text $ "You lose!"

boardDiv :: MonadWidget t m => Int -> Int -> Dynamic t GameState -> Dynamic t DisplaySettings -> m (Event t Action)
boardDiv rows columns dynState dynDisplaySettings = do
    events <- sequence $ generateRow columns <$> [0 .. rows - 1]
    return $ leftmost events
  where
    generateRow = generateBoardRow dynState dynDisplaySettings

generateBoardRow :: MonadWidget t m => Dynamic t GameState -> Dynamic t DisplaySettings -> Int -> Int -> m (Event t Action)
generateBoardRow gameState dynDisplaySettings width row = divClass "board-row" $ do
    events <- sequence $ generateCell row <$> [0 .. width - 1]
    return $ leftmost events
  where
    generateCell = generateBoardCell gameState dynDisplaySettings

generateBoardCell :: MonadWidget t m => Dynamic t GameState -> Dynamic t DisplaySettings -> Int -> Int -> m (Event t Action)
generateBoardCell dynState dynDisplaySettings row column = do
    let dynCellState = cellFromState (BoardCoordinate column row) . cells <$> dynState
    let dynPlayState = playState . globalState <$> dynState
    let dynAttr = classAttr <$> dynDisplaySettings <*> dynCellState <*> dynPlayState

    (cellElement, _) <- elCell' dynAttr

    let revealAction = Reveal (BoardCoordinate column row) <$ domEvent Click cellElement
    let revealAreaAction = RevealArea (BoardCoordinate column row) <$ domEvent Dblclick cellElement
    let toggleAction = ToggleFlag (BoardCoordinate column row) <$ domEvent Contextmenu cellElement

    return $ leftmost [revealAction, revealAreaAction, toggleAction]
  where
    classAttr settings cell@(CellState internalState visibleState) status =
        fromList [("class", pack $ "cell" ++ clickable ++ visibility ++ label ++ flag ++ hint)]
      where
        known = isKnown cell

        clickable
          | Playing <- status, not known, visibleState == Unknown || visibleState == Unsure = " clickable"
          | otherwise = ""

        visibility
          | known = " known"
          | otherwise = " unknown"

        label
          | Mine <- internalState, known || status == Dead = " bomb"
          | Win <- status, Mine <- internalState = " bomb-win"
          | settings & countdownMode, Labeled total x <- visibleState, total /= 0 = " label-" ++ show x
          | Labeled x _ <- visibleState, x /=0 = " label-" ++ show x
          | otherwise = ""

        flag
          | status /= Playing, Mine <- internalState = ""
          | Flagged <- visibleState = " flag"
          | Unsure <- visibleState = " unsure"
          | otherwise = ""

        hint
          | settings & debugMode, not known, Mine <- internalState = " hint hint-mine"
          | settings & debugMode, not known, Safe <- internalState = " hint hint-safe"
          | otherwise = ""
