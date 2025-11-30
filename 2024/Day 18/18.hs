import Data.Array
import Data.Char
import qualified Data.Set as S

-- 1

type Position = (Int, Int)
type Grid = Array Position Bool

parseInput :: String -> (Grid, [Position])
parseInput file = (grid, bytes)
                where bytes = map (\line -> ((read . takeWhile isDigit) line, (read . tail . dropWhile isDigit) line)) (lines file)
                      n = foldr (\(x, y) r -> max (max x y) r) 0 bytes
                      grid = array ((0, 0), (n, n)) [((x, y), False) | x <- [0..n], y <- [0..n]]

dropMemory :: Grid -> [Position] -> Grid
dropMemory grid = foldr (\position grid -> grid // [(position, True)]) grid 

traverseMemory :: Grid -> S.Set Position -> [(Int, Position)] -> Int
traverseMemory _ _ [] = 1_000_000_000
traverseMemory grid seen ((distance, (x, y)):queue) | outsideBounds || S.member (x, y) seen || grid ! (x, y) = traverseMemory grid seen queue
                                                    | (x, y) == (n, n) = distance
                                                    | otherwise = traverseMemory grid (S.insert (x, y) seen) (queue ++ neighbours)
                                                    where n = (snd . snd . bounds) grid
                                                          outsideBounds = x < 0 || x > n || y < 0 || y > n
                                                          neighbours = [(distance + 1, (x + a, y + b)) | a <- [-1..1], b <- [-1..1], abs a /= abs b]

-- 2

findBlockageByte :: Grid -> [Position] -> [(Int, Position)] -> Position
findBlockageByte grid bytes queue = bytes !! index
                                  where index = binarySearchBlocking 0 (length bytes)
                                        binarySearchBlocking :: Int -> Int -> Int
                                        binarySearchBlocking left right | left == right = (left - 1)
                                                                        | shortestPath == 1_000_000_000 = binarySearchBlocking left pivot
                                                                        | otherwise = binarySearchBlocking (pivot + 1) right
                                                                        where pivot = (left + right) `div` 2
                                                                              shortestPath = traverseMemory ((dropMemory grid . take pivot) bytes) S.empty queue

main :: IO()
main = do
        file <- readFile "Day 18/18.txt"
        let (grid, bytes) = parseInput file
            shortestPath = traverseMemory ((dropMemory grid . take 1024) bytes) S.empty [(0, (0, 0))]
            blockageByte = findBlockageByte grid bytes [(0, (0, 0))]
        putStrLn $ show shortestPath
        putStrLn $ (init . tail . show) blockageByte