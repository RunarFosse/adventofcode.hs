import Data.List

-- 1

findTargetRow :: [String] -> Int -> Int
findTargetRow [] _ = 0
findTargetRow (row:rows) index | row !! index == '.' = 1 + findTargetRow rows index
                               | otherwise = 0

placeRock :: [String] -> Int -> Int -> [String]
placeRock [] _ _ = [""]
placeRock ((c:cs):rows) 1 0 = ('O' : cs) : rows
placeRock ((c:cs):rows) 1 i = let (current:others) = placeRock (cs:rows) 1 (i-1) in (c : current) : others
placeRock (row:rows) targetRow i = row : placeRock rows (targetRow-1) i

slideNorth :: String -> [String] -> Int -> [String]
slideNorth row [] _ = [row]
slideNorth [] tilted _ = []:tilted
slideNorth (c:cs) tilted index | c == 'O' && targetRow > 0 = let (current:others) = slideNorth cs (placeRock tilted targetRow index) (index+1) in ('.':current) : others
                               | otherwise = let (current:others) = slideNorth cs tilted (index+1) in (c:current) : others
                                where targetRow = findTargetRow tilted index

tiltNorth :: [String] -> [String] -> [String]
tiltNorth [] tilted = tilted
tiltNorth (row:rows) tilted = tiltNorth rows $ slideNorth row tilted 0

calculateLoad :: [String] -> Int -> Int
calculateLoad [] _ = 0
calculateLoad (row:rows) multiplier = length (filter (=='O') row) * multiplier + calculateLoad rows (multiplier-1)

-- 2

tiltCycle :: [String] -> [String]
tiltCycle rows = rotate90 tiltedEast
                where tiltedNorth = reverse $ tiltNorth rows []
                      tiltedWest = reverse $ tiltNorth (rotate90 tiltedNorth) []
                      tiltedSouth = reverse $ tiltNorth (rotate90 tiltedWest) []
                      tiltedEast = reverse $ tiltNorth (rotate90 tiltedSouth) []
                      rotate90 :: [String] -> [String]
                      rotate90 = (map reverse) . transpose

cycleCycleIterations :: [String] -> [[String]] -> (Int, [String])
cycleCycleIterations rows previous | any (==rows) previous = (0, rows)
                                   | otherwise = (1 + iterations, endRows)
                                    where tilted = tiltCycle rows
                                          (iterations, endRows) = cycleCycleIterations tilted (rows:previous)

performCycles :: [String] -> Int -> [String]
performCycles rows 0 = rows
performCycles rows iterations = performCycles (tiltCycle rows) (iterations-1)

main :: IO()
main = do
        file <- readFile "Day 14/14.txt"
        let ls = lines file
            tilted = reverse $ tiltNorth ls []
            load = calculateLoad tilted (length tilted)
            (cycleStart, cycleEnd) = cycleCycleIterations ls []
            (cycleLength, _) = cycleCycleIterations cycleEnd []
            billionCycles = performCycles cycleEnd ((1000000000 - cycleStart) `rem` cycleLength)
            billionCyclesLoad = calculateLoad billionCycles (length billionCycles)
        putStrLn $ show load
        putStrLn $ show billionCyclesLoad