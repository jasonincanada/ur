{-# Language MultiWayIf, RecordWildCards #-}

{- Ur.hs - Modeling the game of Ur


  Resources:

    Royal Game of Ur
    https://en.wikipedia.org/wiki/Royal_Game_of_Ur
      
    Tom Scott and Irving Finkel play Ur - The British Museum
    https://www.youtube.com/watch?v=WZskjLq040I

-}


module Ur where

import Prelude hiding (flip)
import Data.Bool      (bool)
import Data.Function  ((&))
import Data.IntSet


{- Types -}

data Move   = Move Pos Jump
type Pos    = Int
type Jump   = Int

data Player = White
            | Black
            deriving Eq

data Ur = Ur { mine      :: IntSet    -- positions of pieces
             , yours     :: IntSet
             , mineLeft  :: Int       -- number of pieces waiting off the board
             , yoursLeft :: Int
             , me        :: Player
             }



{- Game Logic -}

data Error  = NotOurPiece
            | NoPieceThere
            | OwnPieceInWay
            | OutOfBounds
            | PieceOnRosette
            | CanOnlyJumpForwards
            deriving Show


pickUp :: Ur -> Pos -> Either Error (Ur, Pos)
pickUp ur@Ur{..} pos

  -- check that we can move a piece from this position
  | middle && isYours = Left NotOurPiece
  | noPiece           = Left NoPieceThere

  -- remove the piece from the board and remember which square we took it from
  | otherwise         = Right (board, pos)

  where
    middle   = pos `elem` [5..12]
    isYours  = pos `member` yours
    noPiece  = if pos == 0
               then mineLeft == 0
               else pos `notMember` mine
    board    = if pos == 0
               then ur { mineLeft = mineLeft - 1 }
               else ur { mine = delete pos mine }


placeDown :: Jump -> (Ur, Pos) -> Either Error Ur
placeDown jump (ur@Ur{..}, pos)
  | jump < 0           = Left CanOnlyJumpForwards
  | placed >  15       = Left OutOfBounds
  | placed == 15       = Right movedOff
  | middle && isYours  = if placed == 8
                         then Left PieceOnRosette
                         else Right captured
  | isMine             = Left OwnPieceInWay
  | otherwise          = Right moved
  where
    placed   = pos + jump
    middle   = placed `elem` [5..12]
    isYours  = placed `member` yours
    isMine   = placed `member` mine

    -- move our piece off the board (by not placing it back down)
    -- so just flip the board so it's the other person's turn
    movedOff = flip ur

    -- we remove one of our opponent's pieces and take its spot
    captured = Ur (placed `insert` mine )
                  (placed `delete` yours)
                  mineLeft
                  (yoursLeft + 1)  -- add it back to their pile
                  me

               & flip  -- now his turn


    -- place the piece on a blank tile
    moved    = Ur (placed `insert` mine)
                  yours
                  mineLeft
                  yoursLeft
                  me

               & if placed `elem` [4,8,14]  -- go again on rosettes
                 then id
                 else flip


-- monadically thread together the actions of picking up a piece and placing it
-- back down, both of which can error out with one of our custom reasons
tryMove :: Ur -> Move -> Either Error Ur
tryMove board (Move pos jump)

      = pickUp board pos >>= placeDown jump

      -- pickUp    :: Ur   -> Pos       -> Either Error (Ur, Pos)
      -- placeDown :: Jump -> (Ur, Pos) -> Either Error Ur
      -- 
      --       >>= :: Monad m => m a -> (a -> m b) -> m b


-- the board is symmetric along the middle lane, so we can pretty
-- easily simulate both sides by flipping the board back and forth
flip :: Ur -> Ur
flip Ur{..} = Ur { mine      = yours
                 , yours     = mine
                 , mineLeft  = yoursLeft
                 , yoursLeft = mineLeft
                 , me        = if me == White
                               then Black
                               else White }


-- get all valid board positions reachable from here with this size jump
moves :: Ur -> Jump -> [Ur]
moves ur@Ur{..} jump
  | jump == 0 = []
  | otherwise = foldMap p tries
  where
    tries :: [Either Error Ur]
    tries = [ tryMove ur (Move from jump) | from <- froms ]

    froms = if mineLeft > 0
            then 0 : toList mine
            else     toList mine

    p :: Either Error Ur -> [Ur]
    p (Right ur) = [ur]
    p (Left  _ ) = []


winner :: Ur -> Maybe Player
winner Ur{..}
  | mineLeft  == 0 && mine  == empty = Just me
  | yoursLeft == 0 && yours == empty = Just (if me == White then Black else White)
  | otherwise                        = Nothing


{- Views -}


-- λ> show board
--    -B-B10-B
--    BW-BB---
--    W-W-30W-
--    W
instance Show Ur where
  show (Ur w b wl bl m) = unlines [black, shared, white, toMove]
    where
      black = concat [
                concat [ bool "-" you (i `member` b) | i <- backFrom 4 ],
                show bl,
                show $ 7 - bl - size b,
                concat [ bool "-" you (i `member` b) | i <- [14,13] ]
              ]

      shared = concat [
                 (if | i `member` w -> me
                     | i `member` b -> you
                     | otherwise    -> "-") | i <- [5..12]                  
               ]

      white = concat [
                concat [ bool "-" me (i `member` w) | i <- backFrom 4 ],
                show wl,
                show $ 7 - wl - size w,
                concat [ bool "-" me (i `member` w) | i <- [14,13] ]
              ]

      (me, you)  = if m == White
                   then ("W", "B")
                   else ("B", "W")
      toMove     = show m
      backFrom n = [n,n-1..1]


instance Show Player where
  show White = "W"
  show Black = "B"

