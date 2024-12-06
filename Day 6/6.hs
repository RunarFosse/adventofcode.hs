import Data.Array
import Data.Set as S hiding (filter, elems)

-- 1

type Map = Array (Int, Int) Char

createMap :: String -> Map
createMap file = array ((0, 0), (n-1, m-1)) [((x, y), cell) | (y, line) <- zip [0..] ls, (x, cell) <- zip [0..] line]
            where ls = lines file
                  (m, n) = (length ls, length $ head ls)

findGuard :: Map -> (Int, Int)
findGuard map = (fst . head . filter (\(_, c) -> c == '^') . assocs) map

simulateGuard :: Map -> (Int, Int) -> (Int, Int) -> Map
simulateGuard map (gx, gy) (dx, dy) | willWalkOutsideBounds = map // [((gx, gy), '@')]
                                    | map ! (gx + dx, gy + dy) == '#' = simulateGuard map (gx, gy) (-dy, dx)
                                    | otherwise = simulateGuard (map // [((gx, gy), '@')]) (gx + dx, gy + dy) (dx, dy)
                                    where (_, (n, m)) = bounds map
                                          willWalkOutsideBounds = (gx, dx) == (0, -1) || (gx, dx) == (n, 1) || (gy, dy) == (0, -1) || (gy, dy) == (m, 1)

-- 2

isLoop :: Map -> (Int, Int) -> (Int, Int) -> S.Set ((Int, Int), (Int, Int)) -> Bool
isLoop map (gx, gy) (dx, dy) seen | willWalkOutsideBounds = False
                                  | S.member ((gx, gy), (dx, dy)) seen = True
                                  | map ! (gx + dx, gy + dy) == '#' = isLoop map (gx, gy) (-dy, dx) (S.insert ((gx, gy), (dx, dy)) seen)
                                  | otherwise = isLoop map (gx + dx, gy + dy) (dx, dy) (S.insert ((gx, gy), (dx, dy)) seen)
                                  where (_, (n, m)) = bounds map
                                        willWalkOutsideBounds = (gx, dx) == (0, -1) || (gx, dx) == (n, 1) || (gy, dy) == (0, -1) || (gy, dy) == (m, 1)

simulateGuardCountLoops :: Map -> (Int, Int) -> (Int, Int) -> S.Set ((Int, Int), (Int, Int)) -> Int
simulateGuardCountLoops map (gx, gy) (dx, dy) seen | willWalkOutsideBounds = 0
                                                   | map ! (gx + dx, gy + dy) == '#' = simulateGuardCountLoops (map // [((gx, gy), '@')]) (gx, gy) (-dy, dx) (S.insert ((gx, gy), (dx, dy)) seen)
                                                   | map ! (gx + dx, gy + dy) /= '@' = if isLoop (map // [((gx + dx, gy + dy), '#')]) (gx, gy) (-dy, dx) seen then 1 + nextSimulation else nextSimulation
                                                   | otherwise = nextSimulation
                                                   where (_, (n, m)) = bounds map
                                                         willWalkOutsideBounds = (gx, dx) == (0, -1) || (gx, dx) == (n, 1) || (gy, dy) == (0, -1) || (gy, dy) == (m, 1)
                                                         nextSimulation = simulateGuardCountLoops (map // [((gx, gy), '@')]) (gx + dx, gy + dy) (dx, dy) (S.insert ((gx, gy), (dx, dy)) seen)

main :: IO()
main = do
        file <- readFile "Day 6/6.txt"
        let map = createMap file
            guard = findGuard map
            guardedMap = simulateGuard map guard (0, -1)
            guardedPositions = (length . filter (=='@') . elems) guardedMap
            possibleLoops = simulateGuardCountLoops map guard (0, -1) S.empty
        putStrLn $ show guardedPositions
        putStrLn $ show possibleLoops