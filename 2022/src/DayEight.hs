module DayEight where

import Data.Char (digitToInt)

type Grid = [[Int]]

type Coordinate = (Int, Int)

getCoord :: Grid -> Coordinate -> Maybe Int
getCoord grid (x, y)
  | x < 0 || y < 0 || x == length grid || y == length (head grid) = Nothing
  | otherwise = Just (grid !! y !! x)

west :: Coordinate -> Coordinate
west (x, y) = (x - 1, y)

east :: Coordinate -> Coordinate
east (x, y) = (x + 1, y)

north :: Coordinate -> Coordinate
north (x, y) = (x, y - 1)

south :: Coordinate -> Coordinate
south (x, y) = (x, y + 1)

fromDirection :: (Coordinate -> Coordinate) -> Grid -> Coordinate -> [Int]
fromDirection dir grid coord = go
  where
    move = dir coord
    go = case getCoord grid coord of
      Just value -> value : fromDirection dir grid move
      Nothing -> []

isVisibleFrom :: (Coordinate -> Coordinate) -> Grid -> Coordinate -> Bool
isVisibleFrom from grid node = go
  where
    (value : compared) = fromDirection from grid node
    go = foldr (\a b -> (value > a) && b) True compared

scenicScoreFrom :: (Coordinate -> Coordinate) -> Grid -> Coordinate -> Int
scenicScoreFrom from grid node = go
  where
    (value : compared) = fromDirection from grid node
    go = viewingDistance value compared

viewingDistance :: Int -> [Int] -> Int
viewingDistance from [] = 0
viewingDistance from (x:xs)
    | from > x = 1 + viewingDistance from xs
    | from <= x = 1
    | otherwise = 0

scenicScore :: Grid -> Coordinate -> Int
scenicScore grid node = product scores
  where
    directions = [north, west, south, east]
    scores = map (\x -> scenicScoreFrom x grid node) directions

isVisible :: Grid -> Coordinate -> Bool
isVisible grid node = foldl f False dirs
  where
    dirs = [north, west, south, east]
    f a b = a || isVisibleFrom b grid node

testgrid =
  [ [3, 0, 3, 7, 3],
    [2, 5, 5, 1, 2],
    [6, 5, 3, 3, 2],
    [3, 3, 5, 4, 9],
    [3, 5, 3, 9, 0]
  ] ::
    Grid

dimension :: Grid -> (Int, Int)
dimension grid@(i : xs) = (length grid - 1, length i - 1)

allCoordinates :: Grid -> [Coordinate]
allCoordinates grid = [(x, y) | x <- [0 .. w], y <- [0 .. l]]
  where
    (w, l) = dimension grid

countVisible :: Grid -> Int
countVisible grid = foldr f 0 coords
  where
    f a = if isVisible grid a then (1 +) else (0 +)
    coords = allCoordinates grid

allScenicScore :: Grid -> [Int]
allScenicScore grid = scores
  where
    coords = allCoordinates grid
    scores = map (scenicScore grid) coords

parseString :: String -> [Int]
parseString = map digitToInt

test = do
  d <- readFile "data/day8test"
  let grid = map parseString (lines d)
  print $ countVisible grid
  print $ (maximum . allScenicScore) grid

main = do
  d <- readFile "data/day8"
  let grid = map parseString $ lines d
  print $ countVisible grid
  print $ (maximum . allScenicScore) grid
  -- 294462 too low
