
module Ur.Strategy where

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

