
module Ur.Play where

import Prelude hiding (flip)
import Control.Monad  (replicateM)
import Data.IntSet
import System.Random  (getStdRandom, randomR)
import Text.Printf    (printf)

import Ur
import Ur.Strategy


{- Some starting boards for simulating tournaments from a given position -}

-- the board at the start of every game, us to play first
blank :: Ur
blank  = Ur { mine      = empty                             --  ----70--
            , yours     = empty                             --  --------
            , mineLeft  = 7                                 --  ----70--
            , yoursLeft = 7                                 --  W
            , me        = White }

-- a snapshot from an average game, they have the rosette
board  = Ur { mine      = fromList [2,4,6,14]               --  -B-B10-B
            , yours     = fromList [1,3,5,8,9,13]           --  BW-BB---
            , mineLeft  = 3                                 --  W-W-30W-
            , yoursLeft = 1                                 --  W
            , me        = White }

-- both players are a 1-roll away from winning the game
almost = Ur { mine      = fromList [14]                     --  ----06B-
            , yours     = fromList [14]                     --  --------
            , mineLeft  = 0                                 --  ----06W-
            , yoursLeft = 0                                 --  W
            , me        = White }


-- create a board with no pieces remaining other than the positions listed for
-- us/them in the arguments
ur :: [Pos] -> [Pos] -> Ur
ur mine yours = Ur (fromList mine)
                   (fromList yours)
                   0 -- none waiting off board
                   0
                   White

-- like ur, but also specify the number of pieces remaining off-board for each player
ur2 :: [Pos] -> [Pos] -> Int -> Int -> Ur
ur2 mine yours mineLeft yoursLeft = Ur (fromList mine)
                                       (fromList yours)
                                       mineLeft
                                       yoursLeft
                                       White


{- Simulation -}

-- play a game to completion from a particular starting position, using the given
-- strategies for the players, reporting the final board, number of turns, and winner
play :: Strategy -> Strategy -> Ur -> IO (Ur, Int, Player)
play strat1 strat2 start = go 0 start
  where
    go :: Int -> Ur -> IO (Ur, Int, Player)
    go turns ur = 

      case winner ur of
        Nothing  -> playTurn ur strategy >>= go (turns+1)
        Just who -> return (ur, turns, who)

      where
        strategy = if me ur == me start
                   then strat1
                   else strat2


    playTurn :: Ur -> Strategy -> IO Ur
    playTurn ur strat = roll >>= move
      where
        -- sum up four random choices of {0,1}
        roll :: IO Jump
        roll = sum <$> replicateM 4 one
          where
            one = getStdRandom (randomR (0,1))

        move :: Jump -> IO Ur
        move jump = case moves ur jump of
                      []     -> return $ flip ur
                      boards -> strat boards



-- play a particular board to completion n times with a given strategy for
-- each player, counting the number of games won by White
tournament :: Ur -> Int -> Strategy -> Strategy -> IO ()
tournament ur n strat1 strat2 = go n 0
  where
    go :: Int -> Int -> IO ()
    go 0 wins = putStrLn $ printf "Won %d times" wins
    go n wins = do
                  (_, _, winner) <- play strat1 strat2 ur
                  
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

