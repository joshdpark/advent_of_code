module DayThirteen where

import Data.Char (digitToInt)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T
import Data.List (elemIndices)

data Packet
  = V Integer
  | P [Packet]
  deriving (Show)

data InputOrder = RightOrder | NotRightOrder
  deriving (Eq, Show)

-- if both are lists, iteratively compare each element
  -- if the left list runs out of items, then right
  -- otherwise then notright
  -- if the same length, then compare the next element in the list
-- if one value is an int and the other is an int then surround the int in a list

lexer = T.makeTokenParser emptyDef

parseValue = V <$> T.integer lexer

parsePacket = P <$> P.between open close (P.sepBy p (P.char ','))
  where
    open = T.symbol lexer "["
    close = T.symbol lexer "]"
    p = parseValue <|> parsePacket

parsePair = do
  first <- parsePacket
  second <- parsePacket
  return (first, second)

comparePacket :: Packet -> Packet -> Maybe InputOrder
comparePacket left right = case (left, right) of
  (V l, V r) -> case compare l r of
    LT -> Just RightOrder
    GT -> Just NotRightOrder
    EQ -> Nothing
  (P l, V r) -> compareParcel l [V r]
  (V l, P r) -> compareParcel [V l] r
  (P l, P r) -> compareParcel l r

compareParcel :: [Packet] -> [Packet] -> Maybe InputOrder
compareParcel [] [] = Nothing
compareParcel _ [] = Just NotRightOrder
compareParcel [] _ = Just RightOrder
compareParcel (l:ls) (r:rs) = case comparePacket l r of
  Just x -> Just x
  Nothing -> compareParcel ls rs

packetLength :: Packet -> Int
packetLength (P [P []]) = 0
packetLength (P xs) = length xs
packetLength (V _) = 1

compareLength :: Packet -> Packet -> InputOrder
compareLength (P ls) (P rs) = case compare (packetLength (P ls)) (packetLength (P rs)) of
  GT -> NotRightOrder
  LT -> RightOrder
  EQ -> NotRightOrder -- this is wrong

comparePairs :: [(Packet, Packet)] -> [InputOrder] 
comparePairs = map go
  where
    go (a, b) = case comparePacket a b of
      Just x -> x
      Nothing -> compareLength a b

main :: IO ()
main = do
  f <- readFile "data/day13"
  let t = P.parse (P.many parsePair) "" f
      res = case t of
        Left _ -> undefined
        Right a -> comparePairs a
      id = map (+1) $ elemIndices RightOrder res
  print (sum id)
  
ex1 = "[1,1,3,1,1]\n[1,1,5,1,1]"                                 -- right
ex2 = "[[1],[2,3,4]]\n[[1],4]"                                   -- right
ex3 = "[9]\n[[8,7,6]]"                                           -- notright
ex4 = "[[4,4],4,4]\n[[4,4],4,4,4]"                               -- right
ex5 = "[7,7,7,7]\n[7,7,7]"                                       -- notright
ex6 = "[]\n[3]"                                                  -- right
ex7 = "[[[]]]\n[[]]"                                             -- notright
ex8 = "[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]" -- notright

test x = go
  where
    res = P.parse parsePair "" x
    f (Right (a, b)) = case comparePacket a b of
      Just x -> x
      Nothing -> compareLength a b
    go = f res

  -- 5755: answers too low
