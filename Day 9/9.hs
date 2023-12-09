
-- 1

calculateDifferences :: [Int] -> [Int]
calculateDifferences [n] = []
calculateDifferences (n1:n2:ns) = (n2 - n1) : calculateDifferences (n2:ns)

extrapolateReadings :: ([Int] -> Int -> Int) -> [Int] -> Int
extrapolateReadings func readings | sum differences == 0 = func readings 0
                                  | otherwise = func readings nextDifference
                                   where differences = calculateDifferences readings
                                         nextDifference = extrapolateReadings func differences

extrapolateRight :: [Int] -> Int -> Int
extrapolateRight readings addend = (head . reverse) readings + addend

-- 2

extrapolateLeft :: [Int] -> Int -> Int
extrapolateLeft readings addend = head readings - addend

main :: IO()
main = do
        file <- readFile "Day 9/9.txt"
        let readings = map ((map (\n -> read n :: Int)) . words) (lines file)
            extrapolationsRight = map (extrapolateReadings extrapolateRight) readings
            extrapolationsLeft = map (extrapolateReadings extrapolateLeft) readings
        putStrLn $ show $ sum extrapolationsRight
        putStrLn $ show $ sum extrapolationsLeft