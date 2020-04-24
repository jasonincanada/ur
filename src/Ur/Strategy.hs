{-# Language RecordWildCards #-}

module Ur.Strategy where

import Data.IntSet
import Data.List      (sortBy)
import Data.Ord       (comparing)
import System.Random  (getStdRandom, randomR)

import Ur


-- when we have more than one possible move, a strategy picks its preferred one
type Strategy = [Ur] -> IO Ur


-- the first move in the list
firstStrat :: Strategy
firstStrat = return . head

-- a random move of the ones available
randomStrat :: Strategy
randomStrat boards = do
  idx <- getStdRandom (randomR (0, length boards - 1))
  return $ boards !! idx


type Scorer = Ur -> Int

bestScoreStrat :: Scorer -> Strategy
bestScoreStrat score boards = do
  let best = last . sortBy (comparing score) $ boards

  -- TODO: this is coupled to the 1 returned by rosette
  if score best == 1
    then return best
    else randomStrat boards


holdRosette :: Strategy
holdRosette = bestScoreStrat rosette


{- Board Scoring -}


-- a simple scorer that awards us 1 point if we're on rosette 8 but -1 if they are
rosette :: Scorer
rosette Ur{..}
  | 8 `member` mine  =  1  --   we're on the rosette - good for us
  | 8 `member` yours = -1  -- they're on the rosette - bad for us
  | otherwise        =  0


{-
    λ> tournament blank 1000 randomStrat randomStrat  -- both of us playing randomly
    Won 515 times

    λ> tournament blank 1000 holdRosette randomStrat  -- we take/hold the rosette
    Won 687 times

    λ> tournament blank 1000 holdRosette holdRosette  -- we both fight over the rosette
    Won 537 times

    λ> tournament blank 1000 randomStrat holdRosette  -- only they prefer the rosette
    Won 323 times

-}

