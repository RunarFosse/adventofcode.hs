import Data.Array
import Data.Char
import qualified Data.Set as S
import PriorityQueue
import Prelude hiding (Left, Right)

-- 1

type Position = (Int, Int)
type LossMap = Array Position Int 

createLossMap :: [String] -> LossMap
createLossMap lines = array ((0, 0), (n-1, m-1)) [((x, y), digitToInt c) | (y, line) <- zip [0..m-1] lines, (x, c) <- zip [0..n-1] line]
                    where (m, n) = (length lines, length $ head lines)

data Direction = Up | Down | Left | Right deriving (Ord, Eq)

move :: Position -> Direction -> Position
move (x, y) Up = (x, y-1)
move (x, y) Down = (x, y+1)
move (x, y) Left = (x-1, y)
move (x, y) Right = (x+1, y)

canMove :: (Position, Direction) -> (Int, Int) -> Bool
canMove ((x, y), Up) (_, _) = y /= 0
canMove ((x, y), Down) (_, m) = y /= m
canMove ((x, y), Left) (_, _) = x /= 0
canMove ((x, y), Right) (n, _) = x /= n

getDifferentValidMoves :: (Position, Direction) -> (Int, Int) -> ((Position, Direction) -> (Int, Int) -> Bool) -> (Position -> Direction -> Position) -> [(Position, Direction)]
getDifferentValidMoves ((x, y), Up) bounds canMove move | not $ canMove ((x, y), Left) bounds = [(move (x, y) Right, Right)]
                                                        | not $ canMove ((x, y), Right) bounds = [(move (x, y) Left, Left)]
                                                        | otherwise = [(move (x, y) Left, Left), (move (x, y) Right, Right)]
getDifferentValidMoves ((x, y), Down) bounds canMove move | not $ canMove ((x, y), Left) bounds = [(move (x, y) Right, Right)]
                                                          | not $ canMove ((x, y), Right) bounds = [(move (x, y) Left, Left)]
                                                          | otherwise = [(move (x, y) Left, Left), (move (x, y) Right, Right)]
getDifferentValidMoves ((x, y), Left) bounds canMove move | not $ canMove ((x, y), Up) bounds = [(move (x, y) Down, Down)]
                                                          | not $ canMove ((x, y), Down) bounds = [(move (x, y) Up, Up)]
                                                          | otherwise = [(move (x, y) Up, Up), (move (x, y) Down, Down)]
getDifferentValidMoves ((x, y), Right) bounds canMove move | not $ canMove ((x, y), Up) bounds = [(move (x, y) Down, Down)]
                                                           | not $ canMove ((x, y), Down) bounds = [(move (x, y) Up, Up)]
                                                           | otherwise = [(move (x, y) Up, Up), (move (x, y) Down, Down)]

type Queue = Heap (Int, Position, Direction, Int)
type Visited = S.Set (Position, Direction, Int)

findShortestPath :: LossMap -> Queue -> Visited -> Int
findShortestPath lossMap queue visited | position == (n, m) = heatloss
                                       | steps < 3 && canMove (position, direction) (n, m) && S.notMember (position, direction, steps) visited = findShortestPath lossMap (merge newqueue $ fromList ((heatloss + lossMap ! (move position direction), move position direction, direction, steps+1):(map (\(pos, dir) -> (heatloss + lossMap ! pos, pos, dir, 1)) differentValidMoves))) $ S.insert (position, direction, steps) visited
                                       | otherwise = findShortestPath lossMap (merge newqueue $ fromList (map (\(pos, dir) -> (heatloss + lossMap ! pos, pos, dir, 1)) differentValidMoves)) $ S.insert (position, direction, steps) visited
                    where (n, m) = snd $ bounds lossMap
                          ((heatloss, position, direction, steps), newqueue) = case extractMin queue of
                                                                                Just (next, nextqueue) -> (next, nextqueue)
                                                                                Nothing -> error "Queue is empty"
                          differentValidMoves = filter (\(pos, dir) -> S.notMember (pos, dir, 1) visited) $ getDifferentValidMoves (position, direction) (n, m) canMove move

-- 2

canUltraMove :: (Position, Direction) -> (Int, Int) -> Bool
canUltraMove ((x, y), Up) (_, _) = y >= 4
canUltraMove ((x, y), Down) (_, m) = y <= m-4
canUltraMove ((x, y), Left) (_, _) = x >= 4
canUltraMove ((x, y), Right) (n, _) = x <= n-4

findUltraShortestPath :: LossMap -> Queue -> Visited -> Int
findUltraShortestPath lossMap queue visited | position == (n, m) = heatloss
                                            | steps < 4 = findUltraShortestPath lossMap (insert (heatloss + lossMap ! (move position direction), move position direction, direction, steps+1) newqueue) $ S.insert (position, direction, steps) visited
                                            | steps < 10 && canMove (position, direction) (n, m) && S.notMember (position, direction, steps) visited = findUltraShortestPath lossMap (merge newqueue $ fromList ((heatloss + lossMap ! (move position direction), move position direction, direction, steps+1):(map (\(pos, dir) -> (heatloss + lossMap ! pos, pos, dir, 1)) differentValidMoves))) $ S.insert (position, direction, steps) visited
                                            | otherwise = findUltraShortestPath lossMap (merge newqueue $ fromList (map (\(pos, dir) -> (heatloss + lossMap ! pos, pos, dir, 1)) differentValidMoves)) $ S.insert (position, direction, steps) visited
                    where (n, m) = snd $ bounds lossMap
                          ((heatloss, position, direction, steps), newqueue) = case extractMin queue of
                                                                                Just (next, nextqueue) -> (next, nextqueue)
                                                                                Nothing -> error "Queue is empty"
                          differentValidMoves = filter (\(pos, dir) -> S.notMember (pos, dir, 1) visited) $ getDifferentValidMoves (position, direction) (n, m) canUltraMove move

main :: IO()
main = do
        file <- readFile "Day 17/17.txt"
        let lossMap = createLossMap $ lines file
            shortestPath = findShortestPath lossMap (fromList [(0, (0, 0), Right, 0)]) S.empty
            ultraShortestPath = findUltraShortestPath lossMap (fromList [(0, (0, 0), Right, 0), (0, (0, 0), Down, 0)]) S.empty
        putStrLn $ show ultraShortestPath