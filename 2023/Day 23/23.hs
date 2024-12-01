import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S

-- 1

type Position = (Int, Int)
type Hike = Array (Int, Int) Char

createHike :: [String] -> Hike
createHike lines = array ((0,0),(n-1,m-1)) [((x, y), c) | (y, line) <- zip [0..m-1] lines, (x, c) <- zip [0..n-1] line]
                 where (m, n) = (length lines, length $ head lines)

isMovable :: Hike -> Position -> Position -> Bool
isMovable hike (x', y') (x, y) = (x' == x || y' == y) && (x', y') /= (x, y) && x' >= 0 && x' <= n && y' >= 0 && y' <= m && hike ! (x', y') /= '#'
                               where (n, m) = snd $ bounds hike


isValidPreviousStep :: Hike -> Position -> Position -> Bool
isValidPreviousStep hike new@(x', y') curr@(x, y) | not $ isMovable hike new curr = False
                                                  | hike ! new == '>' = (x == x'+1 && y == y')
                                                  | hike ! new == '<' = (x == x'-1 && y == y')
                                                  | hike ! new == 'v' = (x == x' && y == y'+1)
                                                  | hike ! new == '^' = (x == x' && y == y'-1)
                                                  | otherwise = True

findPreviousSteps :: Hike -> Position -> [Position]
findPreviousSteps hike (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], isValidPreviousStep hike (x', y') (x, y)]

type DAG = M.Map Position [Position]

createReverseDAG :: Hike -> [(Position, Position)] -> DAG -> DAG
createReverseDAG _ [] dag = dag
createReverseDAG hike ((node, last):queue) dag = createReverseDAG hike (queue ++ (map (\n -> (n, node)) possiblePrevious)) (M.insert node possiblePrevious dag)
                                                where possiblePrevious = filter (/= last) $ findPreviousSteps hike node


calculateLongestPath :: Hike -> Position -> Position -> DAG -> Int
calculateLongestPath hike start target dag = memo M.! target
                where memo = M.fromList [((x, y), solve (x, y)) | (x,y) <- range $ bounds hike]
                      solve :: Position -> Int
                      solve (x, y) | (x, y) == start = 0
                                   | otherwise = 1 + (maximum . map (memo M.!)) possiblePrevious
                                   where possiblePrevious = dag M.! (x, y)

-- 2

type SegmentMap = M.Map Position [(Position, Int)]
type Visited = S.Set Position

findPossibleMoves :: Hike -> Position -> [Position]
findPossibleMoves hike (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], isMovable hike (x', y') (x, y)]

iterateToClosestCrossroad :: Hike -> Position -> Position -> (Position, Position) -> Int -> (Position, Int)
iterateToClosestCrossroad hike node last (start, end) steps | node == start || node == end || length possibleMoves > 1 = (node, steps)
                                                            | otherwise = iterateToClosestCrossroad hike (head possibleMoves) node (start, end) (steps + 1)
                                                            where possibleMoves = filter (/=last) $ findPossibleMoves hike node

findClosestCrossroads :: Hike -> Position -> (Position, Position) -> [(Position, Int)]
findClosestCrossroads hike node (start, end) = map (\position -> iterateToClosestCrossroad hike position node (start, end) 1) possibleMoves
                                             where possibleMoves = findPossibleMoves hike node

createSegmentMap :: Hike -> [Position] -> (Position, Position) -> SegmentMap -> SegmentMap
createSegmentMap _ [] _ segments = segments
createSegmentMap hike (node:queue) (start, end) segments | M.member node segments = createSegmentMap hike queue (start, end) segments
                                                         | otherwise = createSegmentMap hike (queue ++ (map fst closestCrossroads)) (start, end) (M.insert node closestCrossroads segments)
                                                          where closestCrossroads = findClosestCrossroads hike node (start, end)

dfs :: SegmentMap -> Position -> Position -> Visited -> Int
dfs segmentMap node end visited | node == end = 0
                                | length possiblePrevious == 0 = -100000000
                                | otherwise = (maximum . map (\(position, steps) -> steps + dfs segmentMap position end (S.insert node visited))) possiblePrevious
                                where possiblePrevious = filter (\(position, _) -> S.notMember position visited) $ segmentMap M.! node

main :: IO()
main = do
        file <- readFile "Day 23/23.txt"
        let hike = createHike $ lines file
            (n, m) = snd $ bounds hike
            (start, end) = ((1, 0), (n-1, m))
            reverseDAG = createReverseDAG hike [(end, (-1,-1))] M.empty
            longestPath = calculateLongestPath hike start end reverseDAG
            segmentMap = createSegmentMap hike [start] (start, end) M.empty
            longestPathWithoutSlopes = dfs segmentMap start end S.empty
        putStrLn $ show longestPath
        putStrLn $ show longestPathWithoutSlopes