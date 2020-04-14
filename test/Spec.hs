{- Test suite for Ur -}

import Prelude hiding (flip)
import Data.IntSet
import Test.Hspec
import Ur


main :: IO ()
main = hspec $ do

  describe "boards" $ do

    -- A common starting board for tests
    let board    = Ur (fromList [2,4,6,14])
                      (fromList [1,3,5,8,9,13])
                      3
                      1
                      White


    let expected = unlines ["-B-B10-B",
                            "BW-BB---",
                            "W-W-30W-",
                            "W"]



    context "instance Show Ur" $ do

      it "blank" $ do
        show blank `shouldBe` unlines ["----70--",
                                       "--------",
                                       "----70--", "W"]
      it "board" $ do
        show board `shouldBe` expected


    context "tryMove" $ do

      it "doesn't allow picking up an opponent's piece" $ do
        show (tryMove board (Move 5 2))
          `shouldBe` "Left NotOurPiece"

      it "doesn't allow picking up air" $ do
        show (tryMove board (Move 1 2))
          `shouldBe` "Left NoPieceThere"

      it "doesn't allow capturing your own piece" $ do
        show (tryMove board (Move 2 2))
          `shouldBe` "Left OwnPieceInWay"

      it "doesn't allow placing off the board" $ do
        show (tryMove board (Move 14 2))
          `shouldBe` "Left OutOfBounds"

      it "doesn't allow capturing an opponent's piece on the rosette" $ do
        show (tryMove board (Move 6 2))
          `shouldBe` "Left PieceOnRosette"

      it "doesn't allow jumping backwards" $ do
        show (tryMove board (Move 2 (-1)))
          `shouldBe` "Left CanOnlyJumpForwards"

      it "takes opponent's piece" $ do
        show (flip <$> tryMove board (Move 4 1))
          `shouldBe` (unlines ["Right -B-B20-B",
                               "WW-BB---",
                               "--W-30W-",
                               "W"])

      it "moves to an empty square" $ do
        show (flip <$> tryMove board (Move 2 1))
          `shouldBe` (unlines ["Right -B-B10-B",
                               "BW-BB---",
                               "WW--30W-",
                               "W"])

      it "moves off the board" $ do
        show (flip <$> tryMove board (Move 14 1))
          `shouldBe` (unlines ["Right -B-B10-B",
                               "BW-BB---",
                               "W-W-31--",
                               "W"])


    context "misc" $ do

      it "flip" $ do
        (show $ flip board) `shouldBe` (unlines ["W-W-30W-",
                                                 "BW-BB---",
                                                 "-B-B10-B",
                                                 "B"])
