import Prelude hiding (Left, Right, flip)
import Data.List
import Data.Array

-- 1

type Position = (Int, Int)
type Cave = Array Position Char

createCave :: [String] -> Cave
createCave lines = array ((0, 0),(n-1, m-1)) [((x, y), c) | (y, line) <- zip [0..m-1] lines, (x, c) <- zip [0..n-1] line]
                  where (m, n) = (length lines, length $ head lines)

data Direction = Up | Down | Left | Right deriving Eq
type Beam = (Position, Direction)

calculateNewDirections :: Direction -> Char -> [Direction]
calculateNewDirections dir '.' = [dir]
calculateNewDirections dir '|' | any (==dir) [Up, Down] = [dir]
                               | otherwise = [Up, Down]
calculateNewDirections dir '-' | any (==dir) [Left, Right] = [dir]
                               | otherwise = [Left, Right]
calculateNewDirections Up '/' = [Right]
calculateNewDirections Down '/' = [Left]
calculateNewDirections Left '/' = [Down]
calculateNewDirections Right '/' = [Up]
calculateNewDirections Up '\\' = [Left]
calculateNewDirections Down '\\' = [Right]
calculateNewDirections Left '\\' = [Up]
calculateNewDirections Right '\\' = [Down]

calculateNewBeam :: Position -> (Int, Int) -> Direction -> Maybe Beam
calculateNewBeam (x, y) _ Up | y > 0 = Just ((x, y-1), Up)
                             | otherwise = Nothing
calculateNewBeam (x, y) (_, m) Down | y < m = Just ((x, y+1), Down)
                                    | otherwise = Nothing
calculateNewBeam (x, y) _ Left | x > 0 = Just ((x-1, y), Left)
                               | otherwise = Nothing
calculateNewBeam (x, y) (n, _) Right | x < n = Just ((x+1, y), Right)
                                     | otherwise = Nothing

calculateNewBeams :: Beam -> Cave -> [Beam]
calculateNewBeams (pos@(x, y), dir) cave = map (\(Just (pos, dir)) -> (pos, dir)) $ filter (/= Nothing) newBeams
                                          where bounceDirections = calculateNewDirections dir (cave ! pos)
                                                newBeams = map (calculateNewBeam pos (snd $ bounds cave)) bounceDirections

canRemove :: Cave -> Beam -> Bool
canRemove energized (pos@(x, y), dir) = energized ! pos == '#' && case dir of
                                        Up -> y == 0 || energized ! (x, y-1) == '#'
                                        Down -> y == m || energized ! (x, y+1) == '#'
                                        Left -> x == 0 || energized ! (x-1, y) == '#'
                                        Right -> x == n || energized ! (x+1, y) == '#'
                                      where (n, m) = snd $ bounds energized

energize :: Cave -> Cave -> [Beam] -> [Beam] -> Cave
energize _ energized [] _ = energized
energize cave energized (beam@(pos, _):beams) seen = energize cave (energized // [(pos, '#')]) (nonDuplicateBeams ++ beams) (beam:seen)
                                                     where newBeams = calculateNewBeams beam cave
                                                           nonDuplicateBeams = filter (\beam -> all (/= beam) seen) newBeams

countEnergized :: Cave -> Int
countEnergized energized = length $ filter (\status -> status == '#') [energized ! pos | pos <- range(bounds energized)]

-- 2

findPossibleOptimalStartingBeams :: [(Position, Char)] -> (Int, Int) -> [Beam]
findPossibleOptimalStartingBeams [] _ = []
findPossibleOptimalStartingBeams (((x, y), '-'):cave) (n, m) = ((x, 0), Down) : ((x, m), Up) : findPossibleOptimalStartingBeams cave (n, m)
findPossibleOptimalStartingBeams (((x, y), '|'):cave) (n, m) = ((0, y), Right) : ((n, y), Left) : findPossibleOptimalStartingBeams cave (n, m)
findPossibleOptimalStartingBeams (((x, y), '/'):cave) (n, m) = ((x, 0), Down) : ((x, m), Up) : ((0, y), Right) : ((n, y), Left) : findPossibleOptimalStartingBeams cave (n, m)
findPossibleOptimalStartingBeams (((x, y), '\\'):cave) (n, m) = ((x, 0), Down) : ((x, m), Up) : ((0, y), Right) : ((n, y), Left) : findPossibleOptimalStartingBeams cave (n, m)
findPossibleOptimalStartingBeams (c:cave) (n, m) = findPossibleOptimalStartingBeams cave (n, m)


calculateSpacesEnergized :: Cave -> Cave -> [Beam] -> [Int]
calculateSpacesEnergized cave emptyEnergized = map (countEnergized . (\beam -> energize cave emptyEnergized [beam] []))

main :: IO()
main = do
        file <- readFile "Day 16/16.txt"
        let cave = createCave $ lines file
            emptyEnergized = array (bounds cave) [(pos, '.') | pos <- range (bounds cave)]
            caveEnergized = energize cave emptyEnergized [((0, 0), Right)] []
            spacesEnergized = countEnergized caveEnergized
            possibleStartingBeams = nub $ findPossibleOptimalStartingBeams [(pos, cave ! pos) | pos <- range (bounds cave)] (snd $ bounds cave)
            allSpacesEnergized = calculateSpacesEnergized cave emptyEnergized possibleStartingBeams
        putStrLn $ show $ spacesEnergized
        putStrLn $ show $ maximum $ allSpacesEnergized