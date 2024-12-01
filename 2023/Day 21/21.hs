import Data.List
import Data.Array
import qualified Data.Set as S

-- 1

type Position = (Int, Int)
type Garden = Array Position Char

createGarden :: [String] -> Garden
createGarden lines = array ((0,0),(n-1,m-1)) [((x, y), c) | (y, line) <- zip [0..m-1] lines, (x, c) <- zip [0..n-1] line]
                   where (m, n) = (length lines, length $ head lines)

type Visited = S.Set Position

findWalkableNeighbours :: Garden -> Position -> Visited -> [Position]
findWalkableNeighbours garden (x, y) visited = [(x', y') | x' <- [x-1..x+1], y'<-[y-1..y+1], 
                                        (x', y') /= (x, y) && x' >= 0 && x' <= n && y' >= 0 && y' <= m && (x' == x || y' == y) && garden ! (x', y') /= '#' && S.notMember (x', y') visited]
                                     where (n, m) = snd $ bounds garden

walkExact :: Garden -> [Position] -> [Position] -> Visited -> Int -> Int -> [Int]
walkExact garden queue _ _ parity 0 | parity /= 0 = [length $ nub queue]
                                    | otherwise = []
walkExact garden [] next visited parity steps | steps `mod` 2 /= parity = (length $ nub next) : walkExact garden (nub next) [] visited parity (steps - 1)
                                              | otherwise = walkExact garden (nub next) [] visited parity (steps - 1)
walkExact garden (pos:queue) next visited parity steps = walkExact garden queue (next ++ routes) visitedUpdated parity steps
                                   where visitedUpdated = S.insert pos visited 
                                         routes = findWalkableNeighbours garden pos visitedUpdated

walk :: Garden -> Position -> Int -> [Int]
walk garden start steps = 1 : (walkExact garden [start] [] S.empty (steps `mod` 2) steps)

-- 2

findWalkableNeighboursInfinite :: Garden -> Position -> Visited -> [Position]
findWalkableNeighboursInfinite garden (x, y) visited = [(x', y') | x' <- [x-1..x+1], y'<-[y-1..y+1], 
                                                (x', y') /= (x, y) && (x' == x || y' == y) && garden ! (x' `mod` (n+1), y' `mod` (m+1)) /= '#' && S.notMember (x', y') visited]
                                     where (n, m) = snd $ bounds garden

walkExactInfinite :: Garden -> [Position] -> [Position] -> Visited -> Int -> Int -> [Int]
walkExactInfinite garden [] next visited parity steps | steps == 0 = if parity == 0 then [1] else []
                                                      | steps `mod` 2 /= 0 = (length $ nub next) : walkExactInfinite garden (nub next) [] visited parity (steps - 1)
                                                      | otherwise = walkExactInfinite garden (nub next) [] visited parity (steps - 1)
walkExactInfinite garden (pos:queue) next visited parity steps = walkExactInfinite garden queue (next ++ routes) visitedUpdated parity steps
                                   where visitedUpdated = S.insert pos visited 
                                         routes = findWalkableNeighboursInfinite garden pos visitedUpdated

walkInfinite :: Garden -> Position -> Int -> [Int]
walkInfinite garden start steps = walkExactInfinite garden [start] [] S.empty (steps `mod` 2) steps

computeInfiniteWalkSequence :: Garden -> Position -> Int -> Int -> Int -> [Int]
computeInfiniteWalkSequence garden start iterations n x = [sum $ walkInfinite garden start (n + i*x) | i <- [0..iterations-1]]

computeQuadraticFit :: [Int] -> (Int -> Int)
computeQuadraticFit [y0, y1, y2] = (\x -> a*x*x + b*x + c)
                                 where c = y0
                                       a_plus_b = y1 - c
                                       a = (y2 - c - 2*a_plus_b) `div` 2
                                       b = a_plus_b - a

main :: IO()
main = do
        file <- readFile "Day 21/21.txt"
        let garden = createGarden $ lines file
            startPosition = (fst . head . filter ((=='S') . snd)) $ assocs garden
            endPositions = walk garden startPosition 64
            mapSize = ((+1) . snd . snd . bounds) garden
            distanceToEdge = snd startPosition
            calculateTotalEndPositionsInfinite = computeQuadraticFit $ computeInfiniteWalkSequence garden startPosition 3 distanceToEdge mapSize
            totalEndPositionsInfinite = calculateTotalEndPositionsInfinite (26501365 `div` mapSize)
        putStrLn $ show $ sum endPositions
        putStrLn $ show totalEndPositionsInfinite