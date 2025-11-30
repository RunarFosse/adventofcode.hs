import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

data PriorityQueue a = Empty | PriorityQueue a [PriorityQueue a] deriving Show

merge :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
merge Empty pq = pq
merge pq Empty = pq
merge (PriorityQueue a1 pqs1) (PriorityQueue a2 pqs2) | a1 < a2 = PriorityQueue a1 ((PriorityQueue a2 pqs2):pqs1)
                                                      | otherwise = PriorityQueue a2 ((PriorityQueue a1 pqs1):pqs2)

mergePairs :: Ord a => [PriorityQueue a] -> PriorityQueue a
mergePairs [] = Empty
mergePairs [pq] = pq
mergePairs (pq1:pq2:pqs) = merge (merge pq1 pq2) (mergePairs pqs)

push :: Ord a => a -> PriorityQueue a -> PriorityQueue a
push a = merge (singleton a)

pushAll :: Ord a => [a] -> PriorityQueue a -> PriorityQueue a
pushAll as = merge (mergePairs $ map singleton as)

pop :: Ord a => PriorityQueue a -> (a, PriorityQueue a)
pop (PriorityQueue a pqs) = (a, mergePairs pqs)

singleton :: Ord a => a -> PriorityQueue a
singleton a = PriorityQueue a []

-- 1

type Position = (Int, Int)
type Map = Array Position Char

readMap :: String -> Map
readMap file = array ((0, 0), (n - 1, m - 1)) [((x, y), c) | (y, line) <- zip [0..] (lines file), (x, c) <- zip [0..] line]
            where (m, n) = ((length . lines) file, (length . head . lines) file)

type Queue = PriorityQueue (Int, Position, Position)

findShortestPath :: Map -> S.Set (Position, Position) -> Queue -> Int
findShortestPath map seen queue | map ! (x, y) == 'E' = distance
                                | S.member ((x, y), (dx, dy)) seen || map ! (x, y) == '#' = findShortestPath map seen newQueue 
                                | otherwise = findShortestPath map (S.insert ((x, y), (dx, dy)) seen) (pushAll neighbours newQueue)
                                where ((distance, (x, y), (dx, dy)), newQueue) = pop queue
                                      neighbours = [(distance + 1, (x + dx, y + dy), (dx, dy)), (distance + 1000, (x, y), (-dy, dx)), (distance + 1000, (x, y), (dy, -dx))]

-- 2

type PathsQueue = PriorityQueue (Int, Position, Position, S.Set (Position, Position))

findBestSeats :: Map -> M.Map (Position, Position) Int -> PathsQueue -> S.Set Position
findBestSeats _ _ Empty = S.empty
findBestSeats map seen queue | M.member ((x, y), (dx, dy)) seen && seen M.! ((x, y), (dx, dy)) < distance = findBestSeats map seen newQueue
                             | map ! (x, y) == 'E' && M.null (M.filterWithKey (\(position, _) value -> position == (x, y) && value < distance) seen) = (S.insert (x, y) . S.union (S.map fst seats)) (findBestSeats map newSeen newQueue)
                             | map ! (x, y) == '#' = findBestSeats map seen newQueue
                             | otherwise = findBestSeats map newSeen (pushAll neighbours newQueue)
                             where ((distance, (x, y), (dx, dy), seats), newQueue) = pop queue
                                   newSeen = M.insert ((x, y), (dx, dy)) distance seen
                                   newSeats = S.insert ((x, y), (dx, dy)) seats
                                   neighbours = [(distance + 1, (x + dx, y + dy), (dx, dy), newSeats), (distance + 1000, (x, y), (-dy, dx), newSeats), (distance + 1000, (x, y), (dy, -dx), newSeats)]

main :: IO()
main = do
        file <- readFile "Day 16/16.txt"
        let map = readMap file
            start = (fst . head . filter ((=='S') . snd) . assocs) map
            shortestPath = findShortestPath map S.empty (singleton (0, start, (1, 0)))
            bestSeats = findBestSeats map M.empty (singleton (0, start, (1, 0), S.empty))
        putStrLn $ show shortestPath
        putStrLn $ show $ length bestSeats