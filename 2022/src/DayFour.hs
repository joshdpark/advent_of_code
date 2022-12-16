module DayFour where

import Data.Char
import Data.List

stringToInt :: String -> Int
stringToInt s = case s of
  [] -> 0
  (a : b) -> (10 ^ length b) * digitToInt a + stringToInt b

parse :: String -> ([Int], [Int])
parse s = go
  where
    (x, y) = break (== ',') s
    (x', x'') = break (== '-') x
    (y', y'') = break (== '-') (drop 1 y)
    f = stringToInt
    go = ([(f x') .. (f . drop 1 $ x'')], [(f y') .. (f . drop 1 $ y'')])

fullyContains :: ([Int], [Int]) -> Bool
fullyContains (a,b) = a `isInfixOf` b || b `isInfixOf` a

overlaps :: ([Int], [Int]) -> Bool
overlaps (x,y) = case x `intersect` y of
    [] -> False
    _ -> True

sumBools :: [Bool] -> Int
sumBools = foldr f 0
    where
      f x = if x then (+1) else (+0)


test = go
  where
    x = "4-90,1-4"
    (a,b) = parse x
    go = fullyContains (a,b)

main = do
  d <- readFile "data/day4"
  let resultOne = go
        where
          d' = lines d
          e' = map (fullyContains . parse) d'
          go = sumBools e'
  print resultOne
  let resultTwo = go
        where
          d' = lines d
          e' = map (overlaps . parse) d'
          go = sumBools e'
  print resultTwo
