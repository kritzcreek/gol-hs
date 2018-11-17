module Game where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef

type Cell = (Int, Int)
type World = Set Cell

isAlive :: World -> Cell -> Bool
isAlive world cell = Set.member cell world

neighbours :: Cell -> Set Cell
neighbours (x, y) = Set.fromList 
  [ (x + x', y + y')
  | x' <- [-1, 0, 1]
  , y' <- [-1, 0, 1]
  , x' /= 0 || y' /= 0
  ]

neighbourMap :: World -> Map Cell Int
neighbourMap world =
  groupCells (concatMap (Set.toList . neighbours) (Set.toList world))

groupCells :: [Cell] -> Map Cell Int
groupCells = foldr go Map.empty
  where 
    go cell acc = case Map.lookup cell acc of
      Nothing -> Map.insert cell 1 acc
      Just count -> Map.insert cell (count+1) acc

evolve :: World -> World
evolve world =
  let
    nm = neighbourMap world
    go cell count acc = 
      if count == 3 
        then Set.insert cell acc 
        else (if count == 2 && isAlive world cell 
          then Set.insert cell acc 
          else acc)
  in
    Map.foldrWithKey go Set.empty nm

