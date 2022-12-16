module DayOne where
import Data.List

splitOn :: (String -> Bool) -> [String] -> [[String]]
splitOn p x = go
    where
      (a, b) = break p x
      go = case (a,b) of
        (a, []) -> [a]
        (a, b) -> a : splitOn p (dropWhile p b)
      -- go = [splitOnLine (filter (not . isNewLine) y), x]


main :: IO ()
main = do
    content <- readFile "data/day1"
    let a = lines content
    let b = splitOn (=="") a
    let c = map (map (\y -> read y :: Int)) b
    let d = map sum c
    let e = maximum d
    let f = sort d
    print (sum $ take 3 $ reverse f)

test = ["1", "", "2"]
