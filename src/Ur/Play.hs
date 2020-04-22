
module Ur.Play where

import Prelude hiding (flip)
import Control.Monad  (replicateM)
import Data.IntSet
import System.Random  (getStdRandom, randomR)
import Text.Printf    (printf)

import Ur


{- Some starting boards for simulating tournaments from a given position -}

-- the board at the start of every game, us to play first
blank :: Ur
blank  = Ur { mine      = empty
            , yours     = empty
            , mineLeft  = 7
            , yoursLeft = 7
            , me        = White }

-- a snapshot from an average game, they have the rosette
board  = Ur { mine      = fromList [2,4,6,14]
            , yours     = fromList [1,3,5,8,9,13]
            , mineLeft  = 3
            , yoursLeft = 1
            , me        = White }

-- both players are a 1-roll away from winning the game
almost = Ur { mine      = fromList [14]
            , yours     = fromList [14]
            , mineLeft  = 0
            , yoursLeft = 0
            , me        = White }


{- Strategies -}

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


{- Simulation -}

-- play a game to completion from a particular starting position, using the given
-- strategy for both players, reporting the final board, number of turns, and winner
play :: Strategy -> Ur -> IO (Ur, Int, Player)
play strat ur = go 0 ur
  where
    go :: Int -> Ur -> IO (Ur, Int, Player)
    go turns ur = 

      case winner ur of
        Nothing  -> playTurn ur strat >>= go (turns+1)
        Just who -> return (ur, turns, who)


    playTurn :: Ur -> Strategy -> IO Ur
    playTurn ur strat = roll >>= move
      where
        move jump = case moves ur jump of
                      []     -> return $ flip ur
                      boards -> strat boards


    -- sum up four random choices of {0,1}
    roll :: IO Int
    roll = sum <$> replicateM 4 one
      where
        one = getStdRandom (randomR (0,1))


-- play a particular board to completion n times with a given strategy for
-- each player, counting the number of games won by White
--
-- TODO: for now both sides play the same strategy
tournament :: Ur -> Int -> Strategy -> Strategy -> IO ()
tournament ur n strat _ = go n 0
  where
    go :: Int -> Int -> IO ()
    go 0 wins = putStrLn $ printf "Won %d times" wins
    go n wins = do
                  (ur', moves, winner) <- play strat ur
                  
                  let wins' = case winner of
                                White -> wins + 1
                                _     -> wins

                  go (n-1) wins'



{-  λ> board
    -B-B10-B
    BW-BB---
    W-W-30W-
    W


    λ> :t play
    play :: Strategy -> Ur -> IO (Ur, Int, Player)

    λ> play randomStrat board
    (----07--
    --W-W-W-
    ----04--
    W
    ,113,B)

    λ> play randomStrat board
    (----07--
    W---W---
    W--W03--
    W
    ,97,B)


    λ> :t tournament
    tournament :: Ur -> Int -> Strategy -> Strategy -> IO ()

    λ> tournament board 10000 randomStrat randomStrat
    Won 3572 times



    λ> blank
    ----70--
    --------
    ----70--
    W

    λ> tournament blank 10000 randomStrat randomStrat
    Won 5164 times



    λ> almost
    ----06B-
    --------
    ----06W-
    W

    λ> tournament almost 10000 randomStrat randomStrat
    Won 5725 times

-}

