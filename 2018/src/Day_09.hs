module Day_09 where

import Control.Monad.State
import qualified Data.IntMap as IntMap
import Data.List.PointedList.Circular
import Data.Maybe

type Player = Int

type Marble = Int

data GameState = GS
  { currentPlayer :: Player,
    nextMarble :: Marble,
    playerScores :: IntMap.IntMap Int,
    marbleCircle :: PointedList Int
  }
  deriving (Show)

iterateGame :: Int -> Int -> Int
iterateGame numMarbles numPlayers =
  evalState
    iterateGame'
    GS
      { currentPlayer = 0,
        nextMarble = 1,
        playerScores = IntMap.empty,
        marbleCircle = singleton 0
      }
  where
    iterateGame' :: State GameState Int
    iterateGame' = do
      m <- gets nextMarble
      if m > numMarbles
        then gets $ maximum . playerScores
        else do
          if m `mod` 23 == 0
            then do
              modify (\st -> st {marbleCircle = moveN (-7) (marbleCircle st)})
              modify
                ( \st ->
                    st
                      { playerScores =
                          IntMap.insertWith
                            (+)
                            (currentPlayer st)
                            (_focus (marbleCircle st) + m)
                            (playerScores st)
                      }
                )
              modify (\st -> st {marbleCircle = fromJust $ deleteRight $ marbleCircle st})
            else do
              modify (\st -> st {marbleCircle = insertRight m $ next $ marbleCircle st})
          modify
            ( \st ->
                st
                  { currentPlayer = succ (currentPlayer st) `mod` numPlayers,
                    nextMarble = succ (nextMarble st)
                  }
            )
          iterateGame'

-- Skipping parsing for this one

p1 :: Int
p1 = iterateGame 72059 410

p2 :: Int
p2 = iterateGame (72059 * 100) 410

main :: IO ()
main = do
  print p1
  print p2
