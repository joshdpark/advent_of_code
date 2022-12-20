module DayFive where

import Data.Char
import Data.List

newtype Stack = Stack (Int, [Char])
  deriving (Show, Eq)

data Instruction = Instruction {amount :: Int, from :: Int, to :: Int}
  deriving (Show, Eq)

appendStack :: Int -> Char -> [Stack] -> [Stack]
appendStack _ _ [] = []
appendStack i s (Stack (id, str) : xs) =
  if i == id
    then Stack (id, s : str) : xs
    else Stack (id, str) : appendStack i s xs

layerToStack :: String -> Int -> [Stack] -> [Stack]
layerToStack [] _ stack = stack
layerToStack (c : xs) counter stack = go
  where
    updatedstack = if isSpace c then stack else appendStack counter c stack
    go = layerToStack xs (counter + 1) updatedstack

parseStack :: [String] -> [Stack] -> [Stack]
-- use fixed width to parse the crates
parseStack layers stack = go
  where
    indices = [1, 5 .. 33]
    -- foreach layer layerToStack
    f [] stack = stack
    f (x : xs) stack = ho
      where
        layer = map (x !!) indices
        newstack = layerToStack layer 1 stack
        ho = f xs newstack
    go = f layers stack

stackHeader :: String -> [Stack]
stackHeader = map (\x -> Stack (digitToInt x, ""))

parseInstruction :: String -> Instruction
parseInstruction s = go
  where
    [_, amount, _, from, _, to] = map read (words s)
    go = Instruction amount from to

getCrates :: [Stack] -> Int -> Int -> [Char]
getCrates [] amt from = ""
getCrates (Stack (i, s) : xs) amt from
  | from == i = take amt s
  | otherwise = getCrates xs amt from

getCratesUpg :: [Stack] -> Int -> Int -> [Char]
getCratesUpg [] amt from = ""
getCratesUpg (Stack (i, s) : xs) amt from
  | from == i = reverse (take amt s)
  | otherwise = getCratesUpg xs amt from

pickCrates :: [Stack] -> Int -> Int -> [Stack]
pickCrates stack index amt = foldr f [] stack
  where
    f (Stack (i, v)) =
      if i == index
        then (Stack (i, drop amt v) :)
        else (Stack (i, v) :)

dropCrates :: [Stack] -> Int -> [Char] -> [Stack]
dropCrates stack index values = foldr f [] stack
  where
    f (Stack (i, v)) =
      if i == index
        then (Stack (i, values ++ v) :)
        else (Stack (i, v) :)

moveStack :: [Stack] -> Instruction -> [Stack]
moveStack xs i = go
  where
    Instruction amount from to = i
    crates = getCrates xs amount from
    go = dropCrates (pickCrates xs from amount) to (reverse crates)

moveStackUpg :: [Stack] -> Instruction -> [Stack]
moveStackUpg xs i = go
  where
    Instruction amount from to = i
    crates = getCratesUpg xs amount from
    go = dropCrates (pickCrates xs from amount) to (reverse crates)

followInstructions :: [Instruction] -> [Stack] -> [Stack]
followInstructions is stack = foldl moveStack stack is

followInstructionsUpg :: [Instruction] -> [Stack] -> [Stack]
followInstructionsUpg is stack = foldl moveStackUpg stack is

topOfEachStack :: [Stack] -> String
topOfEachStack = foldr (\(Stack (_, xs)) -> (:) . head $ xs) []

test = go
  where
    lines = ["move 2 from 2 to 1", "move 1 from 1 to 3"]
    stacks = [Stack (1, "HBK"), Stack (2, "BOB"), Stack (3, "KP")]
    instructions = map parseInstruction lines
    go = followInstructions instructions stacks

test' = go
  where
    lines = ["move 2 from 2 to 1", "move 1 from 1 to 3"]
    stacks = [Stack (1, "HBK"), Stack (2, "BOB"), Stack (3, "KP")]
    instructions = map parseInstruction lines
    go = followInstructionsUpg instructions stacks

test2 = go
  where
    a = [(1, "A"), (2, "B"), (3, "C")]
    g' i' v' = foldr f [] a
      where
        f (i, v) =
          if i == i'
            then ((i, v' ++ v) :)
            else ((i, v) :)
    go = g' 2 "HO"

main = do
  d <- readFile "data/day5"
  let l = lines d
  let input = reverse (take 8 l)
  let empty_stack = map (\x -> Stack (x, "")) [1 .. 9]
  let stacks = parseStack input empty_stack
  let instructions = map parseInstruction (drop 10 l)
  let result1 = followInstructions instructions stacks
  print result1
  print (topOfEachStack result1)
  let result2 = followInstructionsUpg instructions stacks
  print result2
  print (topOfEachStack result2)
