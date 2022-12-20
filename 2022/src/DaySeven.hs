module DaySeven where

type Path = [String]

data FileNode = File Path Int | Dir Path [FileNode]
  deriving (Eq, Show)

up :: Path -> Path
up = drop 1

down :: String -> Path -> Path
down name path = name : path

parseLines :: Path -> [String] -> [FileNode]
parseLines _ [] = []
parseLines root (x : y : xs) = go
  where
    go = case words x of
      "$" : "cd" : [".."] -> parseLines (up root) (y : xs)
      _ -> go'
        where
          _ : _ : [dir] = words x
          path = dir : root
          brk = foldr f 0 xs
            where
              f a b = case words a of
                "$" : "cd" : _ -> 0
                [] -> 0
                _ -> 1 + b
          (command, rest) = splitAt brk xs
          children = foldr f [] command
            where
              f a b = case words a of
                "dir" : [childir] -> Dir (down childir path) [] : b
                size : [file] -> File (down file path) (read size) : b
          go' = Dir path children : parseLines path rest

-- f (1 f (2 f(3 f(4 (f 5 [])))))

-- parseCommandIndex :: [String] -> [Int]
-- parseCommandIndex lines = go
--   where
--     (indices, _) = foldl f ([], 0) lines
--       where
--         f (indices, counter) line = case words line of
--           "$" : "cd" : [".."] -> (indices, counter + 1)
--           "$" : "cd" : [x] -> (counter : indices, counter + 1)
--           [] -> (counter : indices, counter + 1)
--           _ -> (indices, counter + 1)
--     go = reverse (length lines : indices)

-- range :: Int -> Int -> [a] -> [a]
-- range start stop xs = take (stop - start) (drop start xs)

-- parseCommand :: [String] -> FileNode
-- parseCommand (dir : _ : nodes) = go
--   where
--     dirname = case words dir of
--       "$" : "cd" : [x] -> x
--       _ -> ""
--     files = foldr f [] nodes
--       where
--         f a = case words a of
--           ("dir" : [d]) -> (:) (Dir d [])
--           (size : [filename]) -> (:) (File filename (read size))
--     go = Dir dirname files

-- lineToFileNodes :: [Int] -> [String] -> [FileNode]
-- lineToFileNodes [a] input = []
-- lineToFileNodes _ [] = []
-- lineToFileNodes i input = go
--   where
--     start : [stop] = take 2 i
--     command = range start stop input
--     go = parseCommand command : lineToFileNodes (drop 1 i) input

-- parseInput lines = go
--   where
--     clean = filter (/= "$ cd ..") lines
--     indices = parseCommandIndex clean
--     go = lineToFileNodes indices clean

findNode :: Path -> [FileNode] -> Maybe FileNode
findNode str [] = Nothing
findNode str (n@(Dir name _) : ns) = go
  where
    go =
      if str == name
        then Just n
        else findNode str ns

findPopNode :: FileNode -> [FileNode] -> [FileNode]
findPopNode node = foldr f []
  where
    f a b = if node == a then b else a : b

buildTree :: FileNode -> [FileNode] -> FileNode
buildTree root@(Dir name children) nodes = go
  where
    f [] = []
    f (n : ns) = case n of
      File _ _ -> n : f ns
      Dir dirname _ -> buildTree node newnodes : f ns
        where
          Just node = findNode dirname nodes
          newnodes = findPopNode node (findPopNode root nodes)
    foundchildren = f children
    go = Dir name foundchildren

sumDir :: [FileNode] -> Int
sumDir nodes = go
  where
    -- eligible size = if size <= 100000 then size else 0
    f [] = 0
    f (n : ns) = case n of
      File filename size -> size + f ns
      dir@(Dir dirname child) -> f child + f ns
    go = f nodes

sumAllDirs :: [FileNode] -> [(Path, Int)]
sumAllDirs [] = []
sumAllDirs (n : ns) = go
  where
    go = case n of
      Dir dirname children -> (dirname, sumDir children) : sumAllDirs ns ++ sumAllDirs children
      File _ _ -> sumAllDirs ns

filterSumDirs :: [(Path, Int)] -> Int
filterSumDirs = foldr f 0
  where
    f (_, i) = if i < 100000 then (i +) else (0 +)

-- test = go
--   where
--     command1 = lines "$ cd /\n ls\n dir a\n 14848514 b.txt\n 8504156 c.dat"
--     Dir name children = parseCommand command1
--     go = sumDir children

-- go = parseCommand command1

-- test' :: IO ()
-- test' = do
--   d <- readFile "data/day7test"
--   let nodes = parseInput . lines $ d
--   let tree@(Dir root children) = buildTree (head nodes) nodes
--   print (filterSumDirs . sumAllDirs $ children)

-- main :: IO ()
-- main = do
--   d <- readFile "data/day7"
--   let nodes = parseInput . lines $ d
--   let tree@(Dir root children) = buildTree (head nodes) nodes
--   print (filterSumDirs . sumAllDirs $ children)

main' :: IO ()
main' = do
  d <- readFile "data/day7"
  let l = lines d
  let nodes = parseLines [] l
  let f = findNode ["a", "/"] nodes
  let tree@(Dir root children) = buildTree (head nodes) nodes
  print (filterSumDirs . sumAllDirs $ children)
  -- print f

-- print (findNode "tddmdzd" nodes)
--
-- wrong attempt 954180 -> too low
-- right attemp 1555642
-- NOTE: I had to go back and basically fix my parser so that I kept track of
-- the paths because there were many duplicate dir names and that really messed
-- up the way that I built my tree
