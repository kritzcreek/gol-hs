module Main where

import Game
import Data.List
import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "isAlive" $ do
    it "checks that a cell is alive" $ do
      let world = Set.fromList [(1, 1), (2, 2)]
      isAlive world (1, 1) `shouldBe` True
  
    it "checks that a cell is dead" $ do
      let world = Set.fromList [(1, 1), (2, 2)]
      isAlive world (1, 2) `shouldBe` False
  
  describe "neighbours" $ do
    it "finds neighbours of the given cell" $ do
      neighbours (0, 0) `shouldBe` Set.fromList 
        [(-1, -1), (-1, 0), (-1, 1),
         ( 0, -1),          ( 0, 1),
         ( 1, -1), ( 1, 0), ( 1, 1)]
  
  describe "groupCells" $ do
    it "groups cells as neighbours" $ do
      groupCells [(1, 1), (2, 2), (1, 1)] `shouldBe` Map.fromList
        [ ((1, 1), 2)
        , ((2, 2), 1)
        ]
  describe "evolve" $ do
    let blinker = Set.fromList [(0, 1), (0, 0), (0, -1)]
    it "blinks" $ do
      evolve blinker `shouldBe`
        Set.fromList [(1, 0), (0, 0), (-1, 0)]
    it "blinks twice" $ do
      evolve (evolve (blinker)) `shouldBe` blinker
