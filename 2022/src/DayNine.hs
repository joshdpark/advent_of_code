module DayNine where

import Data.List (foldl', nub)

type Point = (Int, Int)

data Direction
  = North
  | South
  | East
  | West
  | NorthWest
  | SouthWest
  | NorthEast
  | SouthEast
  deriving (Eq, Show)

data Knot = Knot (Point, Knot) | End Point
  deriving (Eq, Show)

type Thread = (Knot, [Point])

constructKnot :: Point -> Int -> Knot
constructKnot initialPoint 0 = End initialPoint
constructKnot initialPoint length =
  Knot (initialPoint, constructKnot initialPoint (length - 1))

peekKnot :: Knot -> Point
peekKnot knot = case knot of
  Knot (point, _) -> point
  End point -> point

knotTail :: Knot -> Point
knotTail knot = case knot of
  Knot (_, knot') -> knotTail knot'
  End point -> point

moveKnot :: Thread -> Direction -> Thread
moveKnot (knot, stack) direction =
  case knot of
    Knot (head, tail) -> (Knot (newhead, newtail), newstack)
      where
        tailpeek = peekKnot tail
        newhead = step head direction
        (newtail, newstack)
          | isAdjacent newhead tailpeek = (tail, stack)
          | otherwise = moveKnot (tail, stack) dir
          where
            Just dir = followTo newhead tailpeek
    End tail -> (End newtail, newtail : stack)
      where
        newtail = step tail direction

moveKnotAmt :: Thread -> Direction -> Int -> Thread
moveKnotAmt thread dir amt =
  foldl' moveKnot thread (replicate amt dir)

isAdjacent :: Point -> Point -> Bool
isAdjacent (x, y) (x', y')
  | dist <= 1 = True
  | otherwise = False
  where
    dist = max (abs (x' - x)) (abs (y' - y))

step :: Point -> Direction -> Point
step (x, y) direction = case direction of
  North -> (x, y + 1)
  South -> (x, y - 1)
  East -> (x + 1, y)
  West -> (x - 1, y)
  NorthEast -> (x + 1, y + 1)
  SouthEast -> (x + 1, y - 1)
  NorthWest -> (x - 1, y + 1)
  SouthWest -> (x - 1, y - 1)

followTo :: Point -> Point -> Maybe Direction
followTo (x, y) (x', y') =
  case (compare x x', compare y y') of
    (EQ, LT) -> Just South
    (EQ, GT) -> Just North
    (LT, EQ) -> Just West
    (GT, EQ) -> Just East
    (GT, GT) -> Just NorthEast
    (LT, LT) -> Just SouthWest
    (LT, GT) -> Just NorthWest
    (GT, LT) -> Just SouthEast
    (EQ, _) -> Nothing

parseLine :: String -> (Direction, Int)
parseLine line = case words line of
  "R" : [x] -> (East, read x)
  "L" : [x] -> (West, read x)
  "U" : [x] -> (North, read x)
  "D" : [x] -> (South, read x)

test = go
  where
    thread = (constructKnot (0, 0) 9, [(0, 0)])
    go = foldl' moveKnot thread (replicate 5 East)

main :: IO ()
main = do
  file <- readFile "data/day9"
  let instructions = map parseLine . lines $ file
      thread = (constructKnot (0, 0) 9, [(0, 0)])
      (knot, stack) = foldl' f thread instructions
        where
          f knot (direction, amt) = moveKnotAmt knot direction amt
  print (knot, stack)
  print $ length (nub stack)
