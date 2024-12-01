import Data.List

-- 1

type Galaxy = (Int, Int)

findGalaxies :: [String] -> (Int, Int) -> [Galaxy]
findGalaxies [] _ = []
findGalaxies ("":lines) (x, y) = findGalaxies lines (0, y+1)
findGalaxies ((c:cs):lines) (x, y) | c == '#' = (x, y) : findGalaxies (cs:lines) (x+1, y)
                                   | otherwise = findGalaxies (cs:lines) (x+1, y)

findExpansions :: [String] -> Int -> [Int]
findExpansions [] _ = []
findExpansions (line:lines) i | all (=='.') line = i : findExpansions lines (i+1)
                              | otherwise = findExpansions lines (i+1)

expandGalaxies :: [Galaxy] -> ([Int], [Int]) -> Int -> [Galaxy]
expandGalaxies [] _ _ = []
expandGalaxies ((gx, gy):galaxies) expansions@(xExpansions, yExpansions) expansionAmount = newGalaxy : expandGalaxies galaxies expansions expansionAmount
                                                                         where newGalaxy = (gx + expansionAmount * (length $ filter (<gx) xExpansions), gy + expansionAmount * (length $ filter (<gy) yExpansions))

calculateShortestPaths :: Galaxy -> [Galaxy] -> [Int]
calculateShortestPaths _ [] = []
calculateShortestPaths s@(sx, sy) ((tx, ty):ts) = distance : calculateShortestPaths s ts
                                                where distance = abs (sx - tx) + abs (sy - ty)

sumAllShortestPaths :: [Galaxy] -> Int
sumAllShortestPaths galaxies = sum $ map (sum . (\(i, galaxy) -> calculateShortestPaths galaxy (drop i galaxies))) $ enumerate galaxies

enumerate :: [a] -> [(Int, a)]
enumerate list = zip [1..length list] list

-- 2 (Nothing new)

main :: IO()
main = do
        file <- readFile "Day 11/11.txt"
        let ls = lines file
            galaxies = findGalaxies ls (0, 0)
            expansions = (findExpansions (transpose ls) 0, findExpansions ls 0)
            expandedGalaxies = expandGalaxies galaxies expansions 1
            shortestPaths = sumAllShortestPaths expandedGalaxies
            gigaExpandedGalaxies = expandGalaxies galaxies expansions (1000000-1) -- For some reason we have to -1 ?
            gigaShortestPaths = sumAllShortestPaths gigaExpandedGalaxies
        putStrLn $ show shortestPaths
        putStrLn $ show gigaShortestPaths