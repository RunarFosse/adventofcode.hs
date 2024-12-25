import Data.List

-- 1

type Pins = [Int]

computePins :: [String] -> Pins
computePins = map (foldr (\c count -> if c == '#' then count + 1 else count) 0) . transpose . init . tail

computeLocksAndKeys :: String ->  ([Pins], [Pins])
computeLocksAndKeys file = (map computePins locks, map computePins keys)
                        where ls = lines file
                              contraptions = [(take 7 . drop (i * 8)) ls | i <- [0..((`div` 8) . length) ls]]
                              locks = filter ((all (=='.')) . last) contraptions
                              keys = filter ((all (=='.')) . head) contraptions

fits :: Pins -> Pins -> Bool
fits lock key = all (\(lockpin, keypin) -> lockpin + keypin <= 5) (zip lock key)

-- 2

-- Merry Christmas!

main :: IO()
main = do
        file <- readFile "Day 25/25.txt"
        let (locks, keys) = computeLocksAndKeys file
            combinations = (concat . map (\lock -> zip (repeat lock) keys)) locks
            fittingCombinations = filter (uncurry fits) combinations
        putStrLn $ show $ length fittingCombinations