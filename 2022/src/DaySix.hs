module DaySix where

isUnique :: String -> Bool
isUnique "" = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

detectMarker :: String -> Int -> Int
detectMarker str counter = go
  where
    packet = take 4 str
    go = if isUnique packet
        then counter + 4
        else detectMarker (drop 1 str) (counter + 1)


detectMessage :: String -> Int -> Int
detectMessage str counter = go
  where
    packet = take 14 str
    go = if isUnique packet
        then counter + 14
        else detectMessage (drop 1 str) (counter + 1)

test = go
  where
    teststr = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    go = detectMarker teststr 0

test' = go
  where
    str = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    go = detectMessage str 0

main :: IO ()
main = do
    d <- readFile "data/day6"
    print (detectMarker d 0)
    print (detectMessage d 0)
