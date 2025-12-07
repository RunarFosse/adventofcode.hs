import Data.Array
import Data.Set as S hiding (filter)

-- 1

type Position = (Int, Int)
type Graph = Array Position Char

createGraph :: String -> Graph
createGraph file = array ((0, 0), (n - 1, m - 1)) [((x, y), c) | (y, line) <- zip [0..] (lines file), (x, c) <- zip [0..] line]
                where (m, n) = ((length . lines) file, (length . head . lines) file)

countSplits :: [Position] -> Graph -> S.Set Position -> Int
countSplits [] graph _ = 0
countSplits ((x, y):positions) graph seen | graph ! (x, y) == '^' = 1 + countSplits ((x-1, y):(x+1, y):positions) graph (S.insert (x, y) seen)
                                          | S.notMember (x, y) seen && inBounds = countSplits ((x, y+1):positions) graph (S.insert (x, y) seen)
                                          | otherwise = countSplits positions graph seen
                                            where (n, m) = (snd . bounds) graph
                                                  inBounds = x >= 0 && x < n && y >= 0 && y < m

-- 2

countTimelines :: Position -> Graph -> Int
countTimelines position graph = timelines ! position
                    where (n, m) = (snd . bounds) graph
                          timelines = array ((0, 0), (n, m)) [((x, y), opt (x, y)) | x <- [0..n], y <- [0..m]]
                          opt :: Position -> Int
                          opt (x, y) | graph ! (x, y) == '^' = timelines ! (x-1, y) + timelines ! (x+1, y)
                                     | inBounds = timelines ! (x, y+1)
                                     | otherwise = 1
                                    where inBounds = x >= 0 && x < n && y >= 0 && y < m

main :: IO()
main = do
        file <- readFile "Day 7/7.txt"
        let graph = createGraph file
            start = (fst . head . filter ((=='S') . snd) . assocs) graph
            splits = countSplits [start] graph S.empty
            timelines = countTimelines start graph
        putStrLn $ show $ splits
        putStrLn $ show $ timelines